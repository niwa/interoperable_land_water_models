from netCDF4 import Dataset
import time
from datetime import datetime, timedelta
from netCDF4 import num2date, date2num
import yaml
import shutil

####################################################
### Copy input netCDF file to output netCDF file ###
####################################################
in_netCDF = r'C:\LUCI_data\Aparima\TopNet\streamq_2005010122_2005040121_utc_topnet_15440662_strahler3-E4-Interoperability-Test.nc'
out_netCDF = r'C:\LUCI_data\Aparima\netCDF\LUCI_output_netCDF_template.nc'

# shutil.copyfile(in_netCDF, out_netCDF)

rootgrpIn = Dataset(in_netCDF, "r", format="NETCDF4")
reaches = rootgrpIn.variables['rchid']
reach_dim_in = rootgrpIn.dimensions['nrch']

###############################################################
### Open netCDF file, read in variables and set up new ones ###
###############################################################
rootgrp = Dataset(out_netCDF, "w", format="NETCDF4", clobber=True)
rootgrp.title = "Output from rainfall runoff model"
rootgrp.institution = "Victoria University"
rootgrp.Conventions = "CF-1.7"
rootgrp.source = "LUCI"
rootgrp.comments = "Output from the Victoria University LUCI rainfall runoff model. The model outputs groundwater recharge and overland flow."

# Copy dimension for nrch
for dname, the_dim in rootgrpIn.dimensions.items():
    if dname == 'nrch':
        rootgrp.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

# Copy dimension for time
for dname, the_dim in rootgrpIn.dimensions.items():
    if dname == 'time':
        rootgrp.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

# Copy rchid variable
for v_name, varin in rootgrpIn.variables.items():
    if v_name == 'rchid':
        print(v_name)
        print(varin)
        outVar = rootgrp.createVariable(v_name, varin.datatype, varin.dimensions)
        
        # Copy variable attributes
        outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
        
        outVar[:] = varin[:]

# Copy selection from time variable
for v_name, varin in rootgrpIn.variables.items():
    if v_name == 'time':
        print(v_name)
        print(varin)
        outVar = rootgrp.createVariable(v_name, varin.datatype, varin.dimensions)
        
        # Copy variable attributes
        outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
        
        outVar[:] = varin[:]

'''
# Create new variables
time = rootgrp.createVariable("time", "i4", dimensions=("time"))
time.standard_name = "time"
time.long_name = "hours since ..."
time.units = "hours"
'''

'''
rainfall = rootgrp.createVariable("rainfall", "f8", dimensions=("time"))
rainfall.standard_name = "precipitation"
rainfall.long_name = "precipitation amount (mm)"
rainfall.units = "mm"

pot_evap = rootgrp.createVariable("pot_evap", "f8", dimensions=("time"))
pot_evap.standard_name = "potential evaporation"
pot_evap.long_name = "potential evaporation (mm)"
pot_evap.units = "mm"

act_evap = rootgrp.createVariable("act_evap", "f8", dimensions=("time"))
act_evap.standard_name = "actual evaporation"
act_evap.long_name = "actual evaporation (mm)"
act_evap.units = "mm"

'''
gw_recharge = rootgrp.createVariable("ecotope_groundwater_recharge", "f8", dimensions=("time", "nrch"))
gw_recharge.description = "Groundwater recharge is baseflow plus interflow"
gw_recharge.standard_name = "(no standard name)"
gw_recharge.long_name = "Groundwater recharge (m3 s-1)"
gw_recharge.cdsm_name = "soil_water_sat-zone_top__recharge_volume_flux"
gw_recharge.units = "m3 s-1"

fast_flow = rootgrp.createVariable("ecotope_fast_flow", "f8", dimensions=("time", "nrch"))
fast_flow.description = "The fast flow is throughflow plus overland flow"
fast_flow.standard_name = "(no standard name)"
fast_flow.cdsm_name = "land_surface_water__runoff_volume_flux"
fast_flow.long_name = "Fast flow (m3 s-1)"
fast_flow.units = "m3 s-1"

gw_flow = rootgrp.createVariable("ecotope_gw_flow", "f8", dimensions=("time", "nrch"))
gw_flow.description = "The groundwater flow is baseflow plus interflow"
gw_flow.standard_name = "(no standard name)"
gw_flow.cdsm_name = "soil_water_unsat-zone__runoff_volume_flux"
gw_flow.long_name = "Groundwater flow (m3 s-1)"
gw_flow.units = "m3 s-1"

rootgrp.close()
rootgrpIn.close()

'''
in_netCDF = r'C:\LUCI_data\Aparima\Climate_TimeSeries\rain_council_vclim_clidb_1972010100_2016123100_south-island_p05_daily_Aparima.nc'

rootgrp = Dataset(in_netCDF, "r", format="NETCDF4")
time_series = rootgrp.variables['time']
print(rootgrp.variables['time'])

rootgrp.close()
'''