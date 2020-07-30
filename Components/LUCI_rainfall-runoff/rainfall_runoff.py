from netCDF4 import Dataset
import os, time
from datetime import datetime, timedelta, date
from netCDF4 import num2date, date2num, date2index
import yaml
import shutil
import numpy as np

debug = True

class RainfallRunoff:

    def __init__(self, in_netCDF=None, out_netCDF=None, prev_reaches_tsd=None,start_time=None,end_time=None,time_step=0,current_time=None,nc_time_offset=0):
        #self.in_netCDF = os.path.abspath(in_netCDF)
        #self.out_netCDF = os.path.abspath(out_netCDF)
        self.in_netCDF = in_netCDF
        self.out_netCDF = out_netCDF

        self.prev_reaches_tsd = prev_reaches_tsd

        self.start_time = start_time
        self.end_time = end_time
        self.time_step = time_step
        self.current_time = start_time
        self.time_units_in = "hours since 1970-01-01 0:00"
        self.time_units_out = "hours since 1970-01-01 0:00"
        self.nc_time_offset = nc_time_offset

    def initialize(self,config_file):
        # get data file names and time paramters from config file
        configData = yaml.safe_load(open(config_file))

        common_dir = configData['common_dir']
        self.in_netCDF = os.path.join(common_dir,configData['in_netCDF'])
        self.out_netCDF = os.path.join(common_dir,configData['out_netCDF'])

        dtst = str(configData['start_time']) # dtst is date-time string
        self.start_time = datetime(int(dtst[6:10]),int(dtst[3:5]),int(dtst[0:2]),int(dtst[11:13]),int(dtst[14:16]),int(dtst[17:19]))
        dtst = str(configData['end_time'])
        self.end_time =  datetime(int(dtst[6:10]),int(dtst[3:5]),int(dtst[0:2]),int(dtst[11:13]),int(dtst[14:16]),int(dtst[17:19]))
        self.time_step = configData['time_step']
        if (debug):
            print("Start time: " + self.start_time.isoformat()) # for debugging/checking
            print("End time: " + self.end_time.isoformat()) # for debugging/checking
            print("Time step: {}".format(self.time_step))

        # To run parallel models, DIMR needs to run from a start time of zero. To work 
        # with this, we'll reset self.start_time to zero, calculate an offset in ouput time
        # units and subtract the offset from the end time to get the length of the run.
        self.nc_time_offset = date2num(self.start_time, self.time_units_out)
        if (debug):
            print("Time offset = ", self.nc_time_offset)

        # initial value of current time is the same as start_time
        self.current_time = self.start_time
        #datetime.date.ctime(datetime.datetime(2000,5,5,12,3,3))
        out_netCDF_template = os.path.join(common_dir,configData['out_netCDF_template'])

        # Open input netCDF file
        rootgrpIn = Dataset(self.in_netCDF, "r+", format="NETCDF4")

        # Copy output netCDF template into actual output file
        shutil.copyfile(out_netCDF_template, self.out_netCDF)

        # Open output netCDF file
        rootgrpOut = Dataset(self.out_netCDF, "r+", format="NETCDF4")

        # Read in variables from input netCDF file
        rain = np.array(rootgrpIn.variables['rain'][0])
        pet = np.array(rootgrpIn.variables['pet'][0])
        time_in = rootgrpIn.variables['time']
        self.time_units_in = time_in.units
        if debug:
            print ("Time units netCDF in: {}".format(self.time_units_in))
        baseflow_proportion = np.array(rootgrpIn.variables['baseflow_proportion'])
        interflow_proportion = np.array(rootgrpIn.variables['interflow_proportion'])
        throughflow_proportion = np.array(rootgrpIn.variables['throughflow_proportion'])
        baseflow_res_time = np.array(rootgrpIn.variables['baseflow_res_time'])
        interflow_res_time = np.array(rootgrpIn.variables['interflow_res_time'])
        throughflow_res_time = np.array(rootgrpIn.variables['throughflow_res_time'])
        field_capacity = np.array(rootgrpIn.variables['field_capacity'])
        not_RAW = np.array(rootgrpIn.variables['not_RAW'])
        max_drainage = np.array(rootgrpIn.variables['max_drainage'])
        max_infiltration = np.array(rootgrpIn.variables['max_infiltration'])
        rainmult = np.array(rootgrpIn.variables['rainmult'])
        vol_soil_init = np.array(rootgrpIn.variables['vol_soil_init'])
        vol_baseflow_init = np.array(rootgrpIn.variables['vol_baseflow_init'])
        vol_interflow_init = np.array(rootgrpIn.variables['vol_interflow_init'])
        vol_throughflow_init = np.array(rootgrpIn.variables['vol_throughflow_init'])

        # Need to take in some information from stream model re. surface water irrigation availability

        # Read in variables from output netCDF file
        gw_recharge = rootgrpOut.variables['ecotope_groundwater_recharge']
        fast_flow = rootgrpOut.variables['ecotope_fast_flow']
        gw_flow = rootgrpOut.variables['ecotope_gw_flow']
        # irri_take_surface = rootgrpOut.variables['ecotope_irri_take_surface']
        time_out = rootgrpOut.variables['time']
        self.time_units_out = time_out.units
        if debug:
            print ("Time units netCDF out: {}".format(self.time_units_out))

        time_out[0] = date2num(self.start_time, self.time_units_out)
        start_time_index_in = date2index(self.start_time, time_in, select='nearest')

        ###################################################################
        ### Set up first RainfallRunoffTimeStep obj (for 0th time step) ###
        ###################################################################

        tsd = RainfallRunoffTimeStep(prev_tsd=None,
                                     time_step=start_time_index_in,
                                     time_value=time_out[0],
                                     not_RAW=not_RAW,
                                     rain=rain[:],
                                     irri=0,
                                     sw_available=None,
                                     pet=pet[:],
                                     aet=None,
                                     gw_abstraction=None,
                                     mod_flow=None,
                                     vol_soil=vol_soil_init,
                                     vol_throughflow=vol_throughflow_init,
                                     vol_interflow=vol_interflow_init,
                                     vol_baseflow=vol_baseflow_init,
                                     overland_flow=None,
                                     inf_overland_flow=None,
                                     sat_overland_flow=None,
                                     fast_flow=None,
                                     gw_flow=None,
                                     gw_recharge=None,
                                     rainmult=rainmult,
                                     max_infiltration=max_infiltration,
                                     max_drainage=max_drainage,
                                     field_capacity=field_capacity,
                                     vol_soil_saturated=None,
                                     throughflow_res_time=throughflow_res_time,
                                     interflow_res_time=interflow_res_time,
                                     baseflow_res_time=baseflow_res_time,
                                     throughflow_proportion=throughflow_proportion,
                                     interflow_proportion=interflow_proportion,
                                     baseflow_proportion=baseflow_proportion,
                                     infiltration_flux=None,
                                     drainage=None)

        #####################################################################
        ### Run methods to populate RainfallRunoffTimeStep obj properties ###
        #####################################################################

        tsd.vol_soil_saturated = vect_calc_vol_soil_sat(tsd.field_capacity)

        tsd.aet = vect_calc_aet(tsd.vol_soil,
                                tsd.not_RAW,
                                tsd.pet)

        tsd.inf_overland_flow, tsd.infiltration_flux = vect_calc_inf_overland_flow(tsd.rain,
                                                                                   tsd.rainmult,
                                                                                   tsd.irri,
                                                                                   tsd.aet,
                                                                                   tsd.max_infiltration,
                                                                                   tsd.infiltration_flux)

        tsd.drainage, tsd.sat_overland_flow, tsd.vol_soil = vect_calc_drainage(tsd.vol_soil,
                                                                               tsd.field_capacity,
                                                                               tsd.max_drainage,
                                                                               tsd.vol_soil_saturated)

        tsd.overland_flow = vect_calc_overland_flow(tsd.inf_overland_flow,
                                                    tsd.sat_overland_flow)

        tsd.gw_recharge = vect_calc_gw_recharge(tsd.baseflow_proportion,
                                                tsd.interflow_proportion,
                                                tsd.drainage)
        
        tsd.mod_flow = vect_calc_mod_flow(tsd.vol_throughflow,
                                          tsd.throughflow_res_time,
                                          tsd.vol_interflow,
                                          tsd.interflow_res_time,
                                          tsd.vol_baseflow,
                                          tsd.baseflow_res_time,
                                          tsd.overland_flow)
        
        tsd.fast_flow = vect_calc_fast_flow(tsd.vol_throughflow,
                                            tsd.throughflow_res_time,
                                            tsd.overland_flow)

        tsd.gw_flow = vect_calc_gw_flow(tsd.vol_interflow,
                                        tsd.interflow_res_time,
                                        tsd.vol_baseflow,
                                        tsd.baseflow_res_time)
    
        ########################################################
        ### Write RainfallRunoffTimeStep data to netCDF file ###
        ########################################################

        fast_flow[0, :] = list(tsd.fast_flow)
        gw_flow[0, :] = list(tsd.gw_flow)
        gw_recharge[0, :] = list(tsd.gw_recharge)

        rootgrpIn.close()
        rootgrpOut.close()
        ##########################################################################################
        ### Assign RainfallRunoffTimeStep obj to prevTSD so can be used in next time step ###
        ##########################################################################################

        self.prev_reaches_tsd = tsd
        return self.current_time


    def update(self, dt):
        if dt == -1:
            dt = self.time_step
        timedelta(hours=dt)
        update_end_time = self.current_time + timedelta(hours=dt)
        next_time = self.current_time + timedelta(hours=self.time_step)
        while next_time <= update_end_time:
            self.updateTo(next_time)
            next_time = self.current_time + timedelta(hours=self.time_step)
        
    def updateTo(self, next_time):

        # Open input netCDF file
        rootgrpIn = Dataset(self.in_netCDF, "r+", format="NETCDF4")

        # Open output netCDF file
        rootgrpOut = Dataset(self.out_netCDF, "r+", format="NETCDF4")

        # Read in variables from input netCDF file

        # driving input variables - required at each time step 
        time_in = rootgrpIn.variables['time']
        curr_time_step = date2index(next_time, time_in, select='nearest')
        if (debug):
            print('Time: {} (index = {})'.format(next_time.isoformat(), curr_time_step))
        rain = np.array(rootgrpIn.variables['rain'][curr_time_step])
        pet = np.array(rootgrpIn.variables['pet'][curr_time_step])
  
        # these are time invariant parameters and just need to be set at initiation
        baseflow_proportion = np.array(rootgrpIn.variables['baseflow_proportion'])
        interflow_proportion = np.array(rootgrpIn.variables['interflow_proportion'])
        throughflow_proportion = np.array(rootgrpIn.variables['throughflow_proportion'])
        baseflow_res_time = np.array(rootgrpIn.variables['baseflow_res_time'])
        interflow_res_time = np.array(rootgrpIn.variables['interflow_res_time'])
        throughflow_res_time = np.array(rootgrpIn.variables['throughflow_res_time'])
        field_capacity = np.array(rootgrpIn.variables['field_capacity'])
        not_RAW = np.array(rootgrpIn.variables['not_RAW'])
        max_drainage = np.array(rootgrpIn.variables['max_drainage'])
        max_infiltration = np.array(rootgrpIn.variables['max_infiltration'])
        rainmult = np.array(rootgrpIn.variables['rainmult'])

        # Read in variables from output netCDF file
        gw_recharge = rootgrpOut.variables['ecotope_groundwater_recharge']
        fast_flow = rootgrpOut.variables['ecotope_fast_flow']
        gw_flow = rootgrpOut.variables['ecotope_gw_flow']
        time_out = rootgrpOut.variables['time']

        # Get previous time step data
        prev_tsd = self.prev_reaches_tsd

        tsd = RainfallRunoffTimeStep(prev_tsd=prev_tsd,
                                     time_step=curr_time_step,
                                     time_value=time_in[curr_time_step],
                                     not_RAW=not_RAW,
                                     rain=rain[:],
                                     irri=0,
                                     sw_available=None,
                                     pet=pet[:],
                                     aet=None,
                                     gw_abstraction=None,
                                     mod_flow=prev_tsd.mod_flow,
                                     vol_soil=prev_tsd.vol_soil,
                                     vol_throughflow=prev_tsd.vol_throughflow,
                                     vol_interflow=prev_tsd.vol_interflow,
                                     vol_baseflow=prev_tsd.vol_baseflow,
                                     overland_flow=None,
                                     inf_overland_flow=None,
                                     sat_overland_flow=None,
                                     fast_flow=None,
                                     gw_flow=None,
                                     gw_recharge=None,
                                     rainmult=rainmult,
                                     max_infiltration=max_infiltration,
                                     max_drainage=max_drainage,
                                     field_capacity=field_capacity,
                                     vol_soil_saturated=None,
                                     throughflow_res_time=throughflow_res_time,
                                     interflow_res_time=interflow_res_time,
                                     baseflow_res_time=baseflow_res_time,
                                     throughflow_proportion=throughflow_proportion,
                                     interflow_proportion=interflow_proportion,
                                     baseflow_proportion=baseflow_proportion,
                                     infiltration_flux=prev_tsd.infiltration_flux,
                                     drainage=prev_tsd.drainage)


        #####################################################################
        ### Run RainfallRunoffTimeStep methods to populate obj properties ###
        ### (using prev_tsd rather than reading from out_netCDF)          ###
        #####################################################################

        tsd.vol_soil = vect_calc_vol_soil(tsd.vol_soil,
                                          tsd.infiltration_flux,
                                          tsd.drainage)

        tsd.vol_throughflow = vect_calc_vol_throughflow(tsd.vol_throughflow,
                                                        tsd.drainage,
                                                        tsd.throughflow_proportion,
                                                        tsd.throughflow_res_time)
        
        tsd.vol_interflow = vect_calc_vol_interflow(tsd.vol_interflow,
                                                    tsd.drainage,
                                                    tsd.interflow_proportion,
                                                    tsd.interflow_res_time)

        tsd.vol_baseflow = vect_calc_vol_baseflow(tsd.vol_baseflow,
                                                  tsd.drainage,
                                                  tsd.baseflow_proportion,
                                                  tsd.baseflow_res_time)

        tsd.vol_soil_saturated = vect_calc_vol_soil_sat(tsd.field_capacity)

        tsd.aet = vect_calc_aet(tsd.vol_soil,
                                tsd.not_RAW,
                                tsd.pet)

        tsd.inf_overland_flow, tsd.infiltration_flux = vect_calc_inf_overland_flow(tsd.rain,
                                                                                   tsd.rainmult,
                                                                                   tsd.irri,
                                                                                   tsd.aet,
                                                                                   tsd.max_infiltration,
                                                                                   tsd.infiltration_flux)

        tsd.drainage, tsd.sat_overland_flow, tsd.vol_soil = vect_calc_drainage(tsd.vol_soil,
                                                                               tsd.field_capacity,
                                                                               tsd.max_drainage,
                                                                               tsd.vol_soil_saturated)


        tsd.overland_flow = vect_calc_overland_flow(tsd.inf_overland_flow,
                                                    tsd.sat_overland_flow)


        tsd.gw_recharge = vect_calc_gw_recharge(tsd.baseflow_proportion,
                                                tsd.interflow_proportion,
                                                tsd.drainage)
        
        tsd.mod_flow = vect_calc_mod_flow(tsd.vol_throughflow,
                                          tsd.throughflow_res_time,
                                          tsd.vol_interflow,
                                          tsd.interflow_res_time,
                                          tsd.vol_baseflow,
                                          tsd.baseflow_res_time,
                                          tsd.overland_flow)
        
        tsd.fast_flow = vect_calc_fast_flow(tsd.vol_throughflow,
                                            tsd.throughflow_res_time,
                                            tsd.overland_flow)

        tsd.gw_flow = vect_calc_gw_flow(tsd.vol_interflow,
                                        tsd.interflow_res_time,
                                        tsd.vol_baseflow,
                                        tsd.baseflow_res_time)

        ########################################################
        ### Write RainfallRunoffTimeStep data to netCDF file ###
        ########################################################

        time_out[time_out.size] = date2num(next_time, self.time_units_out)
        fast_flow[-2:-1, :] = list(tsd.fast_flow)
        gw_flow[-2:-1, :] = list(tsd.gw_flow)
        gw_recharge[-2:-1, :] = list(tsd.gw_recharge)

        rootgrpIn.close()
        rootgrpOut.close()
        self.current_time = next_time

        ###########################################################################################
        ### Assign RainfallRunoffTimeStep obj to prev_tsd so can be used in next time step ###
        ###########################################################################################

        self.prev_reaches_tsd = tsd
        return self.current_time

    def get_start_time(self):
        #return date2num(self.start_time, self.time_units_out)
        return 0
    def get_end_time(self):
        return date2num(self.end_time, self.time_units_out) - self.nc_time_offset
    def get_time_step(self):
        return self.time_step
    def get_current_time(self):
        return date2num(self.current_time, self.time_units_out) - self.nc_time_offset
def calc_vol_soil(vol_soil, infiltration_flux, drainage):

    # Calculate the soil water content
    return vol_soil + infiltration_flux - drainage

def calc_vol_throughflow(vol_throughflow, drainage, throughflow_proportion, throughflow_res_time):

    # Calculate volume of water in each reservoir (max(0, add) is to get around numerical errors when reservoir close to zero)
    # Each reservoir contains inital storage plus the excess drained and apportioned to quick and slow flow accordingly
    return max(0, vol_throughflow
                  + (drainage * throughflow_proportion)
                  - (vol_throughflow / throughflow_res_time))

def calc_vol_interflow(vol_interflow, drainage, interflow_proportion, interflow_res_time):

    return max(0, vol_interflow
                  + (drainage * interflow_proportion)
                  - (vol_interflow / interflow_res_time))

def calc_vol_baseflow(vol_baseflow, drainage, baseflow_proportion, baseflow_res_time):

    return max(0, vol_baseflow
                  + (drainage * baseflow_proportion)
                  - (vol_baseflow / baseflow_res_time))

def calc_vol_soil_sat(field_capacity):

    return field_capacity * 1.5

def calc_aet(vol_soil, not_RAW, pet):

    if vol_soil < not_RAW:
        return (vol_soil / not_RAW) * pet
    else:
        return pet

def calc_inf_overland_flow(rain, rainmult, irri, aet, max_infiltration, infiltration_flux):

    # Infiltration flux is the balance of rainfall minus aet.
    # A positive value shows overall infiltration, while a negative value shows overall evap loss
    effective_rain = (rain * rainmult) + irri - aet
    infiltration_flux = min(effective_rain, max_infiltration) # infiltrated rain in mm

    # Hortonian overland flow occurs when rain exceeds pe and ir (otherwise this eqn always gives zero)
    inf_overland_flow = effective_rain - infiltration_flux

    return inf_overland_flow, infiltration_flux

def calc_drainage(vol_soil, field_capacity, max_drainage, vol_soil_saturated):

    # Determine amount of water allowed to drain to subsurface given volume of water in the soil
    # If vol_soil is below min release vol, then drainage is zero
    # print('vol_soil: ' + str(vol_soil))
    # print('field_capacity: ' + str(field_capacity))
    if vol_soil < field_capacity:
        drainage = 0
        sat_overland_flow = 0
    else:
        drainage = min(vol_soil - field_capacity, max_drainage)

        if vol_soil > vol_soil_saturated:
          sat_overland_flow = vol_soil - vol_soil_saturated
          vol_soil = vol_soil_saturated
        else:
            sat_overland_flow = 0

    return drainage, sat_overland_flow, vol_soil

def calc_overland_flow(inf_overland_flow, sat_overland_flow):

   return inf_overland_flow + sat_overland_flow

def calc_gw_recharge(baseflow_proportion, interflow_proportion, drainage):

    return (baseflow_proportion + interflow_proportion) * drainage

def calc_mod_flow(vol_throughflow, throughflow_res_time,
                  vol_interflow, interflow_res_time,
                  vol_baseflow, baseflow_res_time,
                  overland_flow):
    
    return ((vol_throughflow / throughflow_res_time)
          + (vol_interflow / interflow_res_time)
          + (vol_baseflow / baseflow_res_time)
          + overland_flow)

def calc_fast_flow(vol_throughflow, throughflow_res_time, overland_flow):
    
    return ((vol_throughflow / throughflow_res_time) + overland_flow)

def calc_gw_flow(vol_interflow, interflow_res_time, vol_baseflow, baseflow_res_time):
    
    return (vol_interflow / interflow_res_time) + (vol_baseflow / baseflow_res_time)

# Vectorise the functions so they can be broadcast using numpy
vect_calc_vol_soil = np.vectorize(calc_vol_soil, otypes=[np.float], cache=False)
vect_calc_vol_throughflow = np.vectorize(calc_vol_throughflow, otypes=[np.float], cache=False)
vect_calc_vol_interflow = np.vectorize(calc_vol_interflow, otypes=[np.float], cache=False)
vect_calc_vol_baseflow = np.vectorize(calc_vol_baseflow, otypes=[np.float], cache=False)
vect_calc_vol_soil_sat = np.vectorize(calc_vol_soil_sat, otypes=[np.float], cache=False)
vect_calc_aet = np.vectorize(calc_aet, otypes=[np.float], cache=False)
vect_calc_inf_overland_flow = np.vectorize(calc_inf_overland_flow, otypes=[np.float, np.float], cache=False)
vect_calc_drainage = np.vectorize(calc_drainage, otypes=[np.float, np.float, np.float], cache=False)
vect_calc_overland_flow = np.vectorize(calc_overland_flow, otypes=[np.float], cache=False)
vect_calc_gw_recharge = np.vectorize(calc_gw_recharge, otypes=[np.float], cache=False)
vect_calc_mod_flow = np.vectorize(calc_mod_flow, otypes=[np.float], cache=False)
vect_calc_fast_flow = np.vectorize(calc_fast_flow, otypes=[np.float], cache=False)
vect_calc_gw_flow = np.vectorize(calc_gw_flow, otypes=[np.float], cache=False)

class RainfallRunoffTimeStep:

    def __init__(self, prev_tsd, time_step, time_value, not_RAW, rain, irri, sw_available, pet, aet, gw_abstraction,
                 mod_flow, vol_soil, vol_throughflow, vol_interflow, vol_baseflow, overland_flow, inf_overland_flow, sat_overland_flow,
                 fast_flow, gw_flow, gw_recharge, rainmult,
                 max_infiltration, max_drainage, field_capacity, vol_soil_saturated, throughflow_res_time, interflow_res_time, baseflow_res_time,
                 throughflow_proportion, interflow_proportion, baseflow_proportion,
                 infiltration_flux, drainage):

        self.prev_tsd = prev_tsd
        self.time_step = time_step
        self.time_value = time_value
        self.not_RAW = not_RAW
        self.rain = rain
        self.irri = irri
        self.sw_available = sw_available
        self.pet = pet
        self.aet = aet
        self.gw_abstraction = gw_abstraction
        self.mod_flow = mod_flow
        self.vol_soil = vol_soil
        self.vol_throughflow = vol_throughflow
        self.vol_interflow = vol_interflow
        self.vol_baseflow = vol_baseflow
        self.overland_flow = overland_flow
        self.inf_overland_flow = inf_overland_flow
        self.sat_overland_flow = sat_overland_flow
        self.fast_flow = fast_flow
        self.gw_flow = gw_flow
        self.gw_recharge = gw_recharge
        self.rainmult = rainmult
        self.max_infiltration = max_infiltration
        self.max_drainage = max_drainage
        self.field_capacity = field_capacity
        self.vol_soil_saturated = vol_soil_saturated
        self.throughflow_res_time = throughflow_res_time
        self.interflow_res_time = interflow_res_time
        self.baseflow_res_time = baseflow_res_time
        self.throughflow_proportion=throughflow_proportion
        self.interflow_proportion=interflow_proportion
        self.baseflow_proportion=baseflow_proportion
        self.infiltration_flux = infiltration_flux
        self.drainage = drainage


if __name__== "__main__":
    rr = RainfallRunoff()
    current_time = rr.initialize('C:\Models\dev\lucitools\SMA\config.yaml')
    print(current_time)

    for i in range(1, 2160):
        current_time = rr.update(i)
        print(current_time)
