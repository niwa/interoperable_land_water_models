#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Run Aparima groundwater model from a command line.

@author: Mike Toews, GNS Science

Debug from interactive Python console:
>>> import sys
>>> sys.path.insert(0, r'F:/es/aparima/Aparima-git/put')
>>> import run

Using a YAML config file:
>>> gm = run.GroundwaterModel.initialize('path/to/config.yaml')
>>> gm.update(-1)

Using commands:
>>> run_dir = r'F:/es/aparima/Aparima-git/run'
>>> common_dir = r'F:/ES/aparima/common'
>>> gm = run.GroundwaterModel('steady', run_dir=run_dir, common_dir=common_dir)
>>> gm.update(-1)
"""

import datetime
import flopy
import logging
import numpy as np
import os
import pandas as pd
import sqlite3

from cftime import date2num
try:
    from cftime import num2pydate
except ImportError:
    from cftime import num2date as num2pydate
from netCDF4 import Dataset
from shutil import which
from subprocess import Popen, PIPE
from types import SimpleNamespace


# model variable metadata
# See master spreadsheet on NIWA OneDrive: DataItemsNamesInventory.xlsx
var_d = {
    'gw_level': SimpleNamespace(
        # HEAD
        long_name='groundwater hydraulic head',
        cdsm_name='soil_water_sat-zone_top__depth',
        units='m'),
    'gw_flux': SimpleNamespace(
        # RECH
        long_name='groundwater flux on water table',
        cdsm_name='soil_water_sat-zone__volume_flux',
        units='m/s'),
    'gw_sw_cov': SimpleNamespace(
        # fraction of model/catchment area
        long_name='fraction of catchment area covered by groundwater model',
        cdsm_name='model_groundwater__coverage',
        units='dimensionless'),
    'gw_sw_flow': SimpleNamespace(
        # DRAIN budget
        long_name='groundwater flow to stream',
        cdsm_name='reservoir_water~outgoing__volume_flux',
        units='m^3/s'),
    'gw_N_conc': SimpleNamespace(
        # UCN CONCENTRATION
        long_name='groundwater N concentration at water table',
        cdsm_name='soil_water_sat-zone__nitrogen_concentration',
        units='kg/m^3'),
    # 'soil_water_sat-zone__phosphorous_concentration' ?
    'gw_sw_N_transport': SimpleNamespace(
        # from sw_conc.dat via SFT
        long_name='N transport from groundwater to surface water',
        cdsm_name='N_transport_GW_to_SW',  # TODO: find aggreed name
        units='kg/m^3/s'),
}
# ensure cdsm_name is unique
assert len(var_d) == len(set([var.cdsm_name for var in var_d.values()]))
# ordered is enforced with internal_var_names property

# this is fixed for all models as a referene date/time unit
default_time_units = 'hours since 1970-01-01 00:00:00'


class GroundwaterModel(object):

    def __init__(
            self,
            timeword=None,
            run_dir='.',
            common_dir=None,
            nc_fname='aparima-groundwater.nc',
            sqlite_fname='aparima-groundwater.db',
            luci_nc_fname='LUCI_output.nc',
            process_only=False,
            verbose=1):
        """Initialize Python class"""
        # Start a logger -- TODO: how to get this to BMI C++ callback?
        self.logger = logging.getLogger(self.__class__.__name__)
        self.verbose = verbose
        if not self.logger.handlers:
            ch = logging.StreamHandler()
            if self.verbose == 1:
                ch.setLevel(logging.INFO)
                self.logger.level = logging.INFO
            elif self.verbose > 1:
                ch.setLevel(logging.DEBUG)
                self.logger.level = logging.DEBUG
            formatter = logging.Formatter(
                '%(asctime)s.%(msecs)03d:%(levelname)s:%(name)s:%(message)s',
                '%H:%M:%S')
            ch.setFormatter(formatter)
            self.logger.addHandler(ch)

        self.prefix = 'Aparima'
        valid_timewords = ('steady', 'dynamic')
        if timeword not in valid_timewords:
            raise ValueError(
                'timeword {!r} not valid; use one of {}'
                .format(timeword, valid_timewords))
        self.timeword = timeword
        if run_dir is None:
            run_dir = os.path.dirname(os.path.abspath(__file__))
        if not os.path.isdir(run_dir):
            raise RuntimeError(
                'run dir does not exist: "{}"'.format(run_dir))
        self.run_dir = run_dir
        self.logger.info(
            'Starting {} {} model in run dir: "{}"'.format(
                self.timeword, self.prefix, self.run_dir))
        if common_dir is None:
            common_dir = run_dir
        if not os.path.isdir(common_dir):
            raise RuntimeError(
                'common dir does not exist: "{}"'.format(common_dir))
        self.common_dir = common_dir
        self.nc_fname = nc_fname
        self.nc_path = os.path.join(common_dir, nc_fname)
        self.logger.info(
            'Establishing output {!r} in common dir: "{}"'.format(
                self.nc_fname, self.common_dir))
        self.sqlite_fname = sqlite_fname
        if self.sqlite_fname is None:
            self.logger.info('Output SQLite file not set')
            self.sqlite_path = None
        else:
            self.logger.info('Output SQLite file: %s', self.sqlite_fname)
            self.sqlite_path = os.path.join(self.common_dir, self.sqlite_fname)

        self.process_only = process_only

        # Read misc data
        self.catchments = pd.read_csv(
            os.path.join(self.run_dir, '../gis/Aparima_catchments.csv'))\
            .set_index('nzseg_v3')
        self.reaches = pd.read_csv(
            os.path.join(self.run_dir, '../gis/Aparima_stream_reaches.csv'))\
            .set_index('reachID')
        nc = Dataset(
            os.path.join(self.run_dir, '../gis/Aparima_catchment_weights.nc'))
        np.testing.assert_array_equal(nc['rchid'], self.catchments.index)
        cw = nc['catchment_weights']
        assert cw.dimensions == ('rchid', 'y', 'x')
        self.catchment_weights = np.array(cw).transpose((1, 2, 0))
        del cw
        nc.close()

        self.luci_nc_fname = luci_nc_fname
        if luci_nc_fname:
            self.luci_nc_path = os.path.join(common_dir, luci_nc_fname)
            if not os.path.isfile(self.luci_nc_path):
                self.logger.warning(
                    'LUCI netCDF file does not exist at path: %s',
                    self.luci_nc_path)
                self.luci_nc_path = None
        else:
            self.luci_nc_path = None

        # Set time_units and time_offset_hr attributes using luci_nc_path
        self._set_time_units_and_offset()

        # Track model times in a dataframe
        start_datetime_obj = num2pydate(self.time_offset_hr, self.time_units)
        self.start_datetime = pd.to_datetime(start_datetime_obj)
        time_df = pd.DataFrame({'dt': [np.inf]})
        if self.timeword == 'dynamic':
            time_df['duration'] = pd.TimedeltaIndex([np.inf], 'hours')
            time_df['start'] = time_df['end'] = self.start_datetime
            # Add float versions of hours for BMI
            time_df['start_hours'] = time_df['end_hours'] = \
                float((time_df['start'] - self.start_datetime).dt.days) * 24.0
        self.time_df = time_df

        # Gather paths to executables
        self.bin_dir = os.path.abspath(os.path.join(self.run_dir, '../bin'))
        mfnwt_exe = which('mfnwt', path=self.bin_dir) or 'mfnwt'
        mt3d_exe = which('mt3d-usgs', path=self.bin_dir) or 'mt3d-usgs'

        # Read MODFLOW groundwater flow model data
        mf_nam_fname = self.prefix + '_mf.nam'
        self.mf = flopy.modflow.Modflow.load(
            mf_nam_fname, version='mfnwt', exe_name=mfnwt_exe,
            verbose=verbose >= 2, model_ws=self.run_dir, check=False)
        self.ibound_mask = self.mf.bas6.ibound.array == 0

        # Read MT3D groundwater transport model data
        mt_nam_fname = self.prefix + '_mt.nam'
        self.mt = flopy.mt3d.Mt3dms.load(
            mt_nam_fname, version='mt3d-usgs', exe_name=mt3d_exe,
            verbose=self.verbose >= 2, model_ws=self.run_dir,
            modflowmodel=self.mf)
        if not self.mt.btn.DRYCell:
            self.logger.debug('fixing flopy bug for DRYCell')
            self.mt.btn.DRYCell = True
        if self.mt.btn.delr.format.free:
            self.logger.debug('fixing flopy bug for FREE format')
            self.mt.btn.delr.format.free = False
            self.mt.btn.delc.format.free = False
            self.mt.btn.htop.format.free = False
            self.mt.dsp.trpt.format.free = False
            self.mt.dsp.trpv.format.free = False
            for ilay in range(self.mt.btn.nlay):
                self.mt.btn.dz[ilay].format.free = False
                self.mt.btn.prsity[ilay].format.free = False
                self.mt.btn.icbund[ilay].format.free = False
                for icomp in range(self.mt.btn.ncomp):
                    self.mt.btn.sconc[icomp][ilay].format.free = False
                self.mt.dsp.al[ilay].format.free = False
            for ar in self.mt.dsp.dmcoef:  # not sure if nlay or ncomp
                ar.format.free = False
            for icomp in range(self.mt.btn.ncomp):
                for iper in range(self.mt.btn.nper):
                    self.mt.ssm.crch[icomp][iper].format.free = False

        # Rename models to generic names to preserve original files
        self.mf.name = 'flow'
        self.mt.name = 'transport'
        self.mt.lst.file_name = ['transport.list']  # for consistency

        # Rewrite model start time
        self.mf.dis.start_datetime = \
            self.start_datetime.strftime('%Y-%m-%d %H:%M:%S')

        # Get dimensions
        nrow = self.mf.dis.nrow
        ncol = self.mf.dis.ncol
        ncatch = len(self.catchments)

        # Prepare empty netCDF file
        if os.path.isfile(self.nc_path):
            os.unlink(self.nc_path)
        with Dataset(self.nc_path, 'w') as nc:

            # Basic metadata
            nc.title = 'Groundwater fluxes from {} model'.format(self.prefix)
            nc.file_creation_time = str(datetime.datetime.now())
            nc.Conventions = 'CF-1.7'
            nc.institution = 'GNS Science'
            nc.source = 'MODFLOW/MT3D'
            nc.timeword = timeword
            nc.description = 'Transfer of groundwater fluxes to streams'

            # Dimensions
            _ = nc.createDimension('time', None)
            _ = nc.createDimension('x', ncol)
            _ = nc.createDimension('y', nrow)
            _ = nc.createDimension('rchid', ncatch)

            # Dimension variables and metadata
            tmerc = nc.createVariable('transverse_mercator', 'c')
            tmerc.grid_mapping_name = 'transverse_mercator'
            tmerc.longitude_of_central_meridian = 173.
            tmerc.false_easting = 1600000.0
            tmerc.false_northing = 10000000.0
            tmerc.latitude_of_projection_origin = 0.0
            tmerc.scale_factor_at_central_meridian = 0.9996
            tmerc.long_name = 'CRS definition'
            tmerc.longitude_of_prime_meridian = 0.0
            tmerc.semi_major_axis = 6378137.0
            tmerc.inverse_flattening = 298.257222101
            tmerc.spatial_ref = (
                'PROJCS["NZGD_2000_Transverse_Mercator",'
                'GEOGCS["GCS_NZGD_2000",DATUM["NZGD_2000",'
                'SPHEROID["GRS_1980",6378137,298.257222101]],'
                'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],'
                'PROJECTION["Transverse_Mercator"],'
                'PARAMETER["latitude_of_origin",0],'
                'PARAMETER["central_meridian",173],'
                'PARAMETER["scale_factor",0.9996],'
                'PARAMETER["false_easting",1600000],'
                'PARAMETER["false_northing",10000000],'
                'UNIT["metre",1,AUTHORITY["EPSG","9001"]]]')

            x_nc = nc.createVariable('x', 'f8', ('x',))
            x_nc.standard_name = 'projection_x_coordinate'
            x_nc.long_name = 'x coordinate of projection'
            x_nc.units = 'm'

            y_nc = nc.createVariable('y', 'f8', ('y',))
            y_nc.standard_name = 'projection_y_coordinate'
            y_nc.long_name = 'y coordinate of projection'
            y_nc.units = 'm'

            xcenters = self.mf.modelgrid.xcellcenters
            ycenters = self.mf.modelgrid.ycellcenters.T
            assert (xcenters == xcenters[0]).all()
            assert (ycenters == ycenters[0]).all()
            x_nc[:] = xcenters[0]
            y_nc[:] = ycenters[0]

            time_nc = nc.createVariable('time', 'f8', ('time',))
            time_nc.standard_name = 'time'
            time_nc.units = self.time_units
            time_nc.calendar = 'standard'

            rchid_nc = nc.createVariable('rchid', 'i4', ('rchid',))
            rchid_nc.long_name = 'REC3 stream identifier'
            rchid_nc[:] = np.array(self.catchments.index)

            gw_level_nc = nc.createVariable(
                'gw_level', 'f4', ('time', 'y', 'x'),
                fill_value=self.mf.bas6.hnoflo)
            gw_level_nc.grid_mapping = 'transverse_mercator'

            gw_flux_nc = nc.createVariable(
                'gw_flux', 'f4', ('time', 'y', 'x'),
                fill_value=-999.0)  # TODO: fill value?
            gw_flux_nc.grid_mapping = 'transverse_mercator'

            gw_sw_cov_nc = nc.createVariable(
                'gw_sw_cov', 'f4', ('rchid',))
            # write this once, as it does not get updated
            gw_sw_cov_nc[:] = np.array(self.catchments['grid_coverage'])

            gw_sw_flow_nc = nc.createVariable(
                'gw_sw_flow', 'f4', ('time', 'rchid'),
                fill_value=-999.0)  # TODO: fill value?

            gw_N_conc_nc = nc.createVariable(
                'gw_N_conc', 'f4', ('time', 'y', 'x'),
                fill_value=self.mt.btn.cinact)
            gw_N_conc_nc.grid_mapping = 'transverse_mercator'

            gw_sw_N_transport_nc = nc.createVariable(
                'gw_sw_N_transport', 'f4', ('time', 'rchid'),
                fill_value=self.mt.btn.cinact)  # TODO: fill value?

            # update attributes for each netCDF variable
            var_names = [
                'gw_level', 'gw_flux', 'gw_sw_cov', 'gw_sw_flow',
                'gw_N_conc', 'gw_sw_N_transport'
            ]
            for var_name in var_names:
                var = var_d[var_name]
                var_nc = locals()[var_name + '_nc']
                var_nc.long_name = var.long_name
                var_nc.standard_name = getattr(
                    var, 'standard_name', '(no standard name)')
                var_nc.cdsm_name = var.cdsm_name
                var_nc.units = var.units

        # And do initial model run!
        self.run()

    @classmethod
    def initialize(cls, config_file: str) -> 'GroundwaterModel':
        """Hook for: BMI_API int initialize(const char *config_file)

        Except that this returns an instance of a class, not an integer"""
        try:
            import yaml
        except ImportError:
            raise ImportError('PyYAML required to read config_file')
        with open(config_file) as fp:
            config = yaml.safe_load(fp)
        run_dir = config.get('run_dir')
        if run_dir is None:
            raise ValueError("config file does not have 'run_dir' entry")
        elif not (isinstance(run_dir, str) and os.path.isdir(run_dir)):
            raise ValueError("'run_dir' is not a directory")
        args = config.get('args')
        if args is None:
            raise ValueError("config file does not have 'args' entry")
        elif not isinstance(args, dict):
            raise ValueError("'args' is not a dict object")
        args['run_dir'] = run_dir
        gm = GroundwaterModel(**args)
        # first model run done
        gm.logger.info('initialize(config_file=%r)', config_file)
        return gm

    def _set_time_units_and_offset(self):
        """Internal function to set time units "hours since "YYYY-MM-DD HH:MM"
        and offset in HR

        If a path to LUCI's output file is available, these are used, otherwise
        internal defauls are used.
        """
        self.time_units = default_time_units
        self.time_offset_hr = 0.0
        if self.luci_nc_path and os.path.isfile(self.luci_nc_path):
            time_nc = None
            rchid_nc = None
            with Dataset(self.luci_nc_path, 'r') as luci_nc:
                for key, var in luci_nc.variables.items():
                    if key == 'time':
                        time_nc = var
                    elif key == 'rchid':
                        rchid_nc = var
                if any(x is None for x in [time_nc, rchid_nc]):
                    self.logger.error(
                        "can't find one or more required variables from "
                        "LUCI file")
                    return
                # Misc. checks
                assert time_nc.dimensions == ('time',), time_nc.dimensions
                assert time_nc.standard_name == 'time', time_nc.standard_name
                assert rchid_nc.dimensions == ('nrch',), rchid_nc.dimensions
                assert len(rchid_nc) == len(self.catchments.index)
                assert list(rchid_nc) == list(self.catchments.index)
                self.logger.info(
                    'setting netCDF time units from LUCI: %s', time_nc.units)
                self.time_units = time_nc.units
                if time_nc.size == 0:
                    self.logger.error('no time data available in LUCI file')
                    return
                self.time_offset_hr = float(time_nc[0])
                self.logger.info(
                    'setting time offset from LUCI: %s h', self.time_offset_hr)

    def finalize(self) -> int:
        """Hook for: BMI_API int finalize()

        Nothing needs to be done, as Python's gc will do everything needed
        """
        self.logger.info('finalize()')
        # self.clean()
        self.logger.info('time dataframe:\n%s', self.time_df)
        return 0

    def update(self, dt: float) -> int:
        """Hook for: BMI_API int update(double dt) [hours]

        Special values for dt:
            * -1: run one native timestep
            * 0: don't do anything
            * 999999999.0: run to the end

        Advance state variables by one timestep (model's own, if dt=-1)
        """
        self.logger.info('update(dt=%s) [hours]', dt)
        if dt == 0:
            self.logger.info("don't do anything")
            return
        elif dt > 1000000:
            self.logger.info('converting dt=%s to -1', dt)
            dt = -1
        # Build next time from previous step
        dtd = pd.Timedelta(hours=dt)
        next_time = dict(self.time_df.iloc[-1])
        if self.timeword == 'dynamic':
            if dt == -1.0:
                dt = self.time_df['dt'].iloc[-1]
                if not np.isfinite(dt):
                    # convert model time from days to hours
                    dt = np.round(self.mf.dis.perlen.array[-1] * 24.0, 6)
                dtd = pd.Timedelta(hours=dt)
                self.logger.info('last dt=%s [hours] to be used', dt)
            next_time['dt'] = dt
            next_time['start'] = next_time['end']
            next_time['start_hours'] = next_time['end_hours']
            next_time['end'] += dtd
            next_time['end_hours'] += dt
            next_time['duration'] = dtd
        self.time_df.loc[len(self.time_df)] = next_time
        try:
            self.run()
            return 0
        except Exception as e:
            self.logger.error('update(dt=%s) [hours]: %s', dt, e)
            return 1

    def get_start_time(self) -> float:
        """Hook for: BMI_API void get_start_time(double *t)

        Always return 0.0
        """
        self.logger.info('get_start_time()')
        return 0.0
        if self.timeword == 'dynamic':
            return self.time_df['start_hours'].iloc[0]
        else:
            return 0.0

    def get_end_time(self) -> float:
        """Hook for: BMI_API void get_end_time(double *t)

        Always return 999999999.0
        """
        self.logger.info('get_end_time()')
        return 999999999.0
        if self.timeword == 'dynamic':
            return self.time_df['end_hours'].iloc[-1]
        else:
            return 1.0

    def get_current_time(self) -> float:
        """Hook for: BMI_API void get_current_time(double *t)
        """
        self.logger.info('get_current_time()')
        if self.timeword == 'dynamic':
            return self.time_df['end_hours'].iloc[-1]
        else:
            return 1.0

    def get_time_step(self) -> float:
        """Hook for: BMI_API void get_time_step(double *dt)
        """
        self.logger.info('get_time_step()')
        if self.timeword == 'dynamic':
            dt = self.time_df['dt'].iloc[-1]
            if not np.isfinite(dt):
                # convert model time days to hours
                dt = np.round(self.mf.dis.perlen.array[-1] * 24.0, 6)
            return dt
        else:
            return 0.0

    @property
    def internal_var_names(self) -> list:
        """Returns ordered list of variables names used internally"""
        keys = [
            'gw_level', 'gw_flux', 'gw_sw_cov', 'gw_sw_flow',
            'gw_N_conc', 'gw_sw_N_transport'
        ]
        return keys

    @property
    def cdsm_var_names(self) -> list:
        """Returns list of CDSM variable names"""
        return [var_d[key].cdsm_name for key in self.internal_var_names]

    @property
    def var_shapes(self) -> dict:
        ncatch = len(self.catchments)
        nrow = self.mf.dis.nrow
        ncol = self.mf.dis.ncol
        ret = {
            var_d['gw_level'].cdsm_name: (nrow, ncol),
            var_d['gw_flux'].cdsm_name: (nrow, ncol),
            var_d['gw_sw_cov'].cdsm_name: (ncatch,),
            var_d['gw_sw_flow'].cdsm_name: (ncatch,),
            var_d['gw_N_conc'].cdsm_name: (nrow, ncol),
            var_d['gw_sw_N_transport'].cdsm_name: (ncatch,),
        }
        return ret

    @property
    def var_dtypes(self) -> dict:
        return {
            var_d[key].cdsm_name: np.float32
            for key in self.internal_var_names}

    def get_var_count(self) -> int:
        """Hook for: BMI_API void get_var_count(int *count)
        """
        self.logger.info('get_var_count()')
        return len(self.internal_var_names)

    def get_var_name(self, index: int) -> str:
        """Hook for: BMI_API void get_var_count(int *count)
        """
        self.logger.info('get_var_name(%s)', index)
        return var_d[self.internal_var_names[index]].cdsm_name

    def get_var_rank(self, name: str) -> int:
        """Hook for: BMI_API void get_var_rank(const char *name, int *rank)
        """
        self.logger.info('get_var_rank(%r)', name)
        return len(self.get_var_shape(name))

    def get_var_shape(self, name: str) -> tuple:
        """BMI_API void get_var_shape(const char *name, int shape[MAXDIMS])
        """
        self.logger.info('get_var_shape(%r)', name)
        return self.var_shapes.get(name, ())

    def get_var_type(self, name: str) -> str:
        """Hook for: BMI_API void get_var_type(const char *name, char *type)
        """
        self.logger.info('get_var_type(%r)', name)
        return str(self.var_dtypes[name]().dtype)

    def clean(self):
        """Delete simulation output files from the previous run"""
        files = ['MT3D.CNF']
        flow_exts = ['.list', '.hds', '.cbc', '.sfo', '.ftl']
        files += ['flow' + ext for ext in flow_exts]
        transport_exts = ['.list', '.ucn', '.mas', '.ftl']
        files += ['transport' + ext for ext in transport_exts]
        all_exts = flow_exts + transport_exts + ['_mf.list', '_mt.list']
        files += [self.prefix + ext for ext in all_exts]
        removed = []
        for fname in files:
            if os.path.isfile(fname):
                try:
                    os.remove(fname)
                    removed.append(fname)
                except Exception as e:
                    self.logger.error('cannot remove file: %s\n%s', fname, e)
        if self.verbose:
            if removed:
                self.logger.info(
                    'cleaned up %s files: %s',
                    len(removed), ', '.join(removed))
            else:
                self.logger.info('no files needed to be cleaned up')

    def convert_values_to_grid(self, values):
        """Convert catchment values to a grid"""
        assert values.ndim == 1, values.ndim
        assert values.shape == self.catchment_weights.shape[2:3], values.shape
        return (self.catchment_weights * values).sum(2)

    def convert_grid_to_values(self, grid):
        """Convert grid to catchment values"""
        assert grid.ndim == 2, grid.ndim
        assert grid.shape == self.catchment_weights.shape[0:2], grid.shape
        vals = (self.catchment_weights.transpose((2, 0, 1)) * grid).sum((1, 2))
        wd = self.catchment_weights.sum((0, 1))
        sel = wd != 0.0
        vals[sel] /= wd[sel]
        return vals

    def update_recharge(self):
        """Update recharge from LUCI

        Also store cathcment recharge to cover regions not covered by GW
        """
        if self.timeword == 'steady' or len(self.time_df) <= 1:
            self.logger.info(
                'recharge for steady model or first dynamic step not modified')
            return
        elif not self.luci_nc_path:
            self.logger.warning('path to LUCI not set; recharge unchanged')
            return
        time_nc = None
        rchid_nc = None
        cdsm_rech_name = 'soil_water_sat-zone_top__recharge_volume_flux'
        self.logger.info('update_recharge with %s', self.luci_nc_path)
        with Dataset(self.luci_nc_path, 'r') as nc:
            nc.set_always_mask(True)
            for key, var in nc.variables.items():
                if key == 'time':
                    time_nc = var
                elif key == 'rchid':
                    rchid_nc = var
                elif getattr(var, 'cdsm_name', None) == cdsm_rech_name:
                    rech_nc = var
            if any(x is None for x in [time_nc, rchid_nc, rech_nc]):
                self.logger.error(
                    "can't find one or more required variables from LUCI file")
                return
            elif time_nc.size == 0:
                self.logger.error('no time data available in LUCI file')
                return
            # Misc. checks
            assert time_nc.dimensions == ('time',), time_nc.dimensions
            assert time_nc.standard_name == 'time', time_nc.standard_name
            assert rchid_nc.dimensions == ('nrch',), rchid_nc.dimensions
            assert len(rchid_nc) == len(self.catchments.index)
            assert list(rchid_nc) == list(self.catchments.index)
            assert rech_nc.dimensions == ('time', 'nrch'), rech_nc.dimensions
            # Pers. comm. with Beth: untis are mm/hr
            # assert rech_nc.units == 'm3 s-1', rech_nc.units
            assert time_nc.units == self.time_units, time_nc.units
            nc_time_offset = float(time_nc[0])
            assert nc_time_offset == self.time_offset_hr, nc_time_offset
            # TODO: determine which time(s) to extract and use
            datetime_ar = num2pydate(np.array(time_nc), time_nc.units)
            del datetime_ar
            # take last time slice (for now?)
            time_idx = -1
            rech_catch = rech_nc[time_idx]
            if rech_catch.mask.all() and rech_nc.shape[0] > 1:
                # find last non-zero time slice using last 24 hours
                rech_ar = rech_nc[-24:]
                rech_sum = rech_ar.sum()
                if np.ma.is_masked(rech_sum):
                    self.logger.error('LUCI recharge data is masked')
                    time_idx = None
                elif rech_sum == 0.0:
                    self.logger.warning('LUCI recharge data is zero')
                    time_idx = None
                else:
                    self.logger.warning(
                        'recharge data from LUCI at last timestep is masked')
                    # time_idx = (rech_ar.sum(1) > 0.0).argmin() - 1
                    time_idx = rech_ar.mask.all(1).argmax() - 1
                    rech_catch = rech_ar[time_idx]
            if time_idx is not None:
                timestamp = num2pydate(time_nc[time_idx], time_nc.units)
                self.logger.info(
                    'extracting LUCI %r from index %d: %s', rech_nc.name,
                    time_idx, timestamp.strftime('%Y-%m-%d %H:%M:%S'))
                # convert from mm/hr to m/day
                self.catchments['recharge'] = rech_catch * 0.024
            else:
                self.catchments['recharge'] = 0.0
        # convert from m/day to mm/year
        v = self.catchments['recharge'] * 365250.0
        self.logger.info(
            'LUCI catchment recharge rate (in mm/year) has a min={:.1f}, '
            'median={:.1f}, mean={:.1f}, max={:.1f}'
            .format(v.min(), v.median(), v.mean(), v.max()))
        inner_rch = (
            self.catchment_weights * self.catchments['recharge'].values).sum(2)
        # Get original recharge (for steady model)
        shape2d = self.mf.dis.nrow, self.mf.dis.ncol
        rech_path = os.path.join(self.run_dir, '../put/Aparima.RECH.ref')
        with open(rech_path, 'r') as fp:
            orig_rch = flopy.utils.util_array.Util2d.load_txt(
                shape2d, fp, np.float32, '(free)')
        # Recharge outside catchment subset remains the same, but remove inner
        outer_rch = np.abs((self.catchment_weights.sum(2) - 1)) * orig_rch
        # combine inner and outer portions to cover full area
        full_rch = outer_rch + inner_rch
        self.mf.rch.rech[0] = full_rch

    def update_N_loss(self):
        """Update N loss from catchmentNutrients.db (steady).

        Also store cathcment recharge to cover regions not covered by GW
        """
        if self.timeword == 'dynamic':
            # This was suppose to be from APSIM (Rogerio from Plant & Food)
            self.logger.error('update_N_loss: dynamic not supported')
            return
        elif self.timeword == 'steady':
            db_fname = 'catchmentNutrients.db'
            db_path = os.path.join(self.common_dir, db_fname)
            if not os.path.isfile(db_path):
                self.logger.error(
                    'update_N_loss: steady %s not found in %s',
                    db_fname, self.common_dir)
                return
            self.logger.info('update_N_loss with %s', db_fname)
            db = sqlite3.connect(db_path)
            df = pd.read_sql('''\
                SELECT REC3_ID, N_loss_tot
                FROM Results_S3;
                ''', db).set_index('REC3_ID', verify_integrity=True)
            db.close()
            if len(df) != len(self.catchments):
                raise ValueError(
                    'number of catchments are different: {} vs {}'
                    .format(len(df), len(self.catchments)))
            elif set(df.index) != set(self.catchments.index):
                raise ValueError('catchment IDs are different')
            # Ignore the 'basarea' attribute in S3_Results which has a zero
            df['area'] = self.catchments['catchment_area']
            # From NIWA: N_loss_tot units are kg/yr, convert to kg/day/m2
            df['N_loss'] = df['N_loss_tot'] / df['area'] / 365.25
            self.catchments['N_loss'] = df['N_loss']
        else:
            raise ValueError(
                'timeword {!r} not recognised'.format(self.timeword))

        # translate to grid for MT3D-USGS SSM CRCH
        N_loss = self.convert_values_to_grid(self.catchments['N_loss'].values)
        # TODO: consider recharge rate?
        self.mt.ssm.crch[0][0] = N_loss
        return

    def run(self):
        """Run model and process results

        Results are stored in the netCDF file and used for initial conditions
        for the next run (if any)
        """
        # Get dimensions
        nrow = self.mf.dis.nrow
        ncol = self.mf.dis.ncol

        # Model time iteration
        it = self.time_df.index[-1]
        self.logger.info('running time index %d\n%s', it, self.time_df.loc[it])
        assert it + 1 == len(self.time_df), (it, len(self.time_df))
        if it == 0:
            # Ensure first simulation is steady state
            np.testing.assert_array_equal(self.mf.dis.steady.array, [True])
            # also make sure the BMI timestep size is 1 hr (convert to days)
            self.mf.dis.perlen = 1.0 / 24.0
        elif self.timeword == 'dynamic':
            # Configure transient models
            # convert 'dt' from hours to days
            perlen = self.time_df.loc[it, 'dt'] / 24.0
            nstp = 5  # or 1
            tsmult = 1.2
            self.mf.dis.steady = False
            self.mf.dis.perlen = perlen
            self.mt.btn.perlen = perlen
            self.mt.btn.ttsmax = perlen
            self.mf.dis.nstp = nstp
            self.mt.btn.nstp = nstp
            self.mf.dis.tsmult = tsmult
            self.mt.btn.tsmult = tsmult
            self.mf.oc.stress_period_data = {
                # (0, 0): ['save head', 'save budget'],
                (0, nstp - 1): ['save head', 'save budget'],
            }
            # TODO: are there better values to use for aquifer storage?
            self.mf.upw.sy = 0.2
            self.mf.upw.ss = 1e-3
            # Switch transport model from finite-difference to TVD scheme
            self.mt.adv.mixelm = -1

        # Get external updates to model
        self.update_recharge()
        self.update_N_loss()

        # MODFLOW groundwater flow model
        if self.process_only:
            self.logger.info('process-only option skips model runs')
        else:
            self.clean()

            self.logger.info('Writing MODFLOW inputs')
            self.mf.write_input()

            self.logger.info('Writing MT3D-USGS inputs')
            self.mt.write_input()

            run_model_args = {'silent': False, 'report': False}
            if self.verbose < 1:
                run_model_args['silent'] = True

            self.logger.info('Running MODFLOW groundwater flow model')
            success, _ = self.mf.run_model(**run_model_args)

            if not success:
                self.logger.error('flow model did not converge')

        # Head file
        hds_fname = self.prefix + '.hds'
        self.logger.info('reading groundwater heads from %s', hds_fname)
        hds_path = os.path.join(self.mf.model_ws, hds_fname)
        try:
            b = flopy.utils.HeadFile(hds_path)
            kstpkper = b.get_kstpkper()
            times = b.get_times()
            hds = np.ma.array(b.get_data())
        finally:
            b.close()
        hds.mask = ((hds.data == self.mf.upw.hdry) |
                    (hds.data == self.mf.bas6.hnoflo))
        hds.fill_value = self.mf.bas6.hnoflo
        if self.timeword == 'dynamic':
            # Use as initial conditions for next run
            self.mf.bas6.strt = hds.data.copy()
            # self.mf.bas6.write_file()
        # Determine water table
        top_dry = hds.mask.argmin(axis=0)
        k, j = np.meshgrid(np.arange(ncol), np.arange(nrow))
        top = self.mf.dis.top.array
        bot = self.mf.dis.botm[-1].array
        gw_level = np.ma.empty((nrow, ncol), dtype=hds.dtype)
        gw_level[:] = np.clip(
            hds[top_dry, j, k], bot, top)

        # build a ts DataFrame to check time
        mfts = pd.DataFrame(kstpkper, columns=['kstp', 'kper'])
        mfts['time'] = times

        # Cell-by-cell budget file
        cbc_fname = self.prefix + '.cbc'
        self.logger.info('reading groundwater budget from %s', cbc_fname)
        cbc_path = os.path.join(self.mf.model_ws, cbc_fname)
        try:
            b = flopy.utils.CellBudgetFile(cbc_path)
            np.testing.assert_array_almost_equal(mfts['time'], b.get_times())
            rech = b.get_data(text='RECHARGE')[0][1]
            drn = b.get_data(text='DRAIN')[0]['q']
        finally:
            b.close()
        gw_flux = np.ma.array(rech / 86400.0, mask=self.ibound_mask[0])

        # Net catchment fluxes
        rd = self.reaches[['segnum', 'row', 'col', 'fraclen']].copy()
        # reverse sign, correct -0.0 to 0.0
        drn *= -1.0
        drn[drn == 0.0] = 0.0
        assert drn.min() >= 0.0, drn.min()
        # Take only first nreach values -- remaining are outside cells
        rd['drain'] = drn[0:len(self.reaches)]
        # sum drain flux, then convert from m3/day to m3/s
        self.catchments['drain'] = \
            rd.groupby('segnum')['drain'].sum() / 86400.0
        gw_sw_flow = np.ma.masked_invalid(self.catchments['drain'])
        # Use LUCI catchment recharge rates to determine GW->SW flux outside
        if (self.timeword == 'dynamic' and
                len(self.time_df) > 1 and self.luci_nc_path):
            # convert to non-masked array
            gw_sw_flow = gw_sw_flow.filled(0.0)
            out_flux = (
                (1.0 - self.catchments['grid_coverage']) *
                self.catchments['recharge'])
            sel = self.catchments['grid_coverage'] < 1.0
            gw_sw_flow[sel] += out_flux[sel]

        # MT3D groundwater transport model
        if not self.process_only:
            self.logger.info('Running MT3D-USGS groundwater transport model')

            # custom runner
            success = False
            args = [self.mt.exe_name, self.mt.namefile]
            proc = Popen(args, stdout=PIPE, stderr=PIPE, cwd=self.run_dir)
            while True:
                stdout = proc.stdout.readline().decode('ascii')
                if stdout == '' and proc.poll() is not None:
                    break
                line = stdout.rstrip()
                if 'Program completed.' in line:
                    success = True
                if self.verbose > 1:
                    print(line)
                elif self.verbose == 1:
                    if not (line.startswith(' Outer Iter') or line == ''):
                        print(line)
            if proc.returncode != 0:
                success = False
                self.logger.error('non-zero return code: %s', proc.returncode)

            if not success:
                self.logger.error('transport model did not converge')

        # Unformatted concentration file
        ucn_fname = self.prefix + '.ucn'
        self.logger.info('reading groundwater concentrations from %s',
                         ucn_fname)
        ucn_path = os.path.join(self.mt.model_ws, ucn_fname)
        cinact = self.mt.btn.cinact
        try:
            b = flopy.utils.UcnFile(ucn_path)
            kstpkper = b.get_kstpkper()
            times = b.get_times()
            ucn = np.ma.masked_equal(b.get_data(), cinact)
        finally:
            b.close()
        ucn.fill_value = cinact
        if self.timeword == 'dynamic':
            # Use as initial conditions for next run
            for ilay in range(self.mt.btn.nlay):
                # write each layer, becuase of odd flopy behaviour
                self.mt.btn.sconc[0][ilay] = ucn[ilay].filled(0.0)
            # self.mt.btn.write_file()
        # Determine upper-most concentration surface
        k, j = np.meshgrid(np.arange(ncol), np.arange(nrow))
        top_mask = ucn.mask.argmin(axis=0)
        gw_N_conc = ucn[top_mask, j, k]
        gw_N_conc.fill_value = self.mt.btn.cinact

        # build a ts DataFrame
        mtts = pd.DataFrame(kstpkper, columns=['kstp', 'kper'])
        mtts['time'] = times

        # Base-0 index on 2D array
        rd['idx'] = np.ravel_multi_index(
            (np.array(rd['row']), np.array(rd['col'])), (nrow, ncol))

        # Calculate concentration flux from aquifer concentration
        rd['gw_N_conc'] = ucn.flatten()[rd['idx']].filled(0.0)
        # multipy drain flux by concentration
        rd['gw_sw_N_transport'] = rd['drain'] * rd['gw_N_conc'] / 86400.0
        # sum flux, then convert from m3/day to m3/s
        self.catchments['gw_sw_N_transport'] = \
            rd.groupby('segnum')['gw_sw_N_transport'].sum()

        gw_sw_N_transport = np.ma.masked_invalid(
            self.catchments['gw_sw_N_transport'])

        # Re-open netCDF file to store results
        with Dataset(self.nc_path, 'a') as nc:
            # Extend time dimension
            time_nc = nc.variables['time']
            if self.timeword == 'dynamic':
                time_nc[it] = date2num(
                    self.time_df['end'].iloc[-1].to_pydatetime(),
                    time_nc.units)
            else:
                # time is only the index
                time_nc[it] = float(it)

            # Write data variables along new time dimension
            nc.variables['gw_level'][it] = gw_level
            nc.variables['gw_flux'][it] = gw_flux
            nc.variables['gw_sw_flow'][it] = gw_sw_flow
            nc.variables['gw_N_conc'][it] = gw_N_conc
            nc.variables['gw_sw_N_transport'][it] = gw_sw_N_transport

        self.logger.info('wrote netCDF file %s', self.nc_path)

        if self.sqlite_fname:
            db = sqlite3.connect(self.sqlite_path)
            db_table = 'groundwater'
            self.catchments.to_sql(db_table, db, if_exists='replace')
            self.logger.info('wrote SQLite table %r to file %s',
                             db_table, self.nc_path)


if __name__ == '__main__':
    """Command-line interface"""
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--timeword', required=True, choices=['steady', 'dynamic'],
        help='mode of simulation')
    parser.add_argument(
        '--run-dir', default='.',
        help='internal simulation run directory with MODFLOW files; '
        'if None, use the absolute directory to this Python file; '
        "default is '.', the current directory")
    parser.add_argument(
        '--common-dir',
        help='common directory with exchange files read/written for BMI run; '
        'default is same as run_dir')
    parser.add_argument('--nc-fname', help='output a new netCDF file name')
    parser.add_argument('--sqlite-fname', help='output SQLite DB file name')
    parser.add_argument(
        '--luci-nc-fname', default='LUCI_output.nc',
        help='input LUCI netCDF file name')
    parser.add_argument(
        '--process-only', action='store_true',
        help='process output files using a previous model run')
    parser.add_argument(
        '--num-update', type=int,
        help='number of update(-1) calls; default 1 for dynamic, 0 for steady')
    parser.add_argument(
        '--verbose', type=int, default=1, choices=[0, 1, 2],
        help='verbose level; default 1')
    args = vars(parser.parse_args())
    num_update = args.pop('num_update')
    # remove arguments with no value provided
    for key, value in list(args.items()):
        if value is None:
            del args[key]
    gm = GroundwaterModel(**args)
    if num_update is None:
        if args['timeword'] == 'dynamic':
            num_update = 1
        else:
            num_update = 0
    for un in range(num_update):
        gm.update(-1)
    gm.finalize()
