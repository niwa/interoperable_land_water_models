from netCDF4 import Dataset
import time
from datetime import datetime, timedelta
from netCDF4 import num2date, date2num
import yaml
import shutil
import random

in_netCDF = r'C:\LUCI_data\Aparima\TopNet\streamq_2005010122_2005040121_utc_topnet_15440662_strahler3-E4-Interoperability-Test.nc'
out_netCDF = r'C:\LUCI_data\Aparima\netCDF\LUCI_netCDF_input_climate_and_soil_data.nc'

rootgrpIn = Dataset(in_netCDF, "r", format="NETCDF4")
reaches = rootgrpIn.variables['rchid']
reach_dim_in = rootgrpIn.dimensions['nrch']

###############################################################
### Open netCDF file, read in variables and set up new ones ###
###############################################################
rootgrp = Dataset(out_netCDF, "w", format="NETCDF4", clobber=True)
rootgrp.title = "Input data for rainfall runoff model"
rootgrp.institution = "Victoria University"
rootgrp.Conventions = "CF-1.7"
rootgrp.source = "LUCI"
rootgrp.comments = "Input data for the Victoria University LUCI rainfall runoff model."

# Create time dimension
# time_dim = rootgrp.createDimension('time')

# Copy dimension for nrch
for dname, the_dim in rootgrpIn.dimensions.items():
    if dname == 'nrch':
        rootgrp.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)
        noReaches = len(the_dim)

# Copy dimension for time
for dname, the_dim in rootgrpIn.dimensions.items():
    if dname == 'time':
        rootgrp.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)
        noTime = len(the_dim)

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

# Create new variables
# time = rootgrp.createVariable("time", "i4", dimensions=("time"))
# time = rootgrp.variables['time']
'''
time.standard_name = "time"
time.long_name = "days since ..."
time.units = "days"
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
rain = rootgrp.createVariable("rain", "f8", dimensions=("time", "nrch"))
rain.description = "Daily rainfall per reach"
rain.standard_name = "(no standard name)"
rain.long_name = "Daily rainfall per reach (mm)"
rain.units = "mm"

# Copy selection from aprecip variable
for v_name, varin in rootgrpIn.variables.items():
    if v_name == 'aprecip':
        print(v_name)
        print(varin)

        rain[:] = varin[:] * 1000 # metres to mm

pet = rootgrp.createVariable("pet", "f8", dimensions=("time", "nrch"))
pet.description = "Daily PET per reach"
pet.standard_name = "(no standard name)"
pet.long_name = "Daily PET per reach (mm)"
pet.units = "mm"
pet[:] = float(0)

# Copy selection from aprecip variable
for v_name, varin in rootgrpIn.variables.items():
    if v_name == 'potevap':
        print(v_name)
        print(varin)

        pet[:] = varin[:] * 1000 # metres to mm

rainmult = rootgrp.createVariable("rainmult", "f8", dimensions=("nrch"))
rainmult.description = "Rain multiplier"

field_capacity = rootgrp.createVariable("field_capacity", "f8", dimensions=("nrch"))
field_capacity.description = "Field capacity"

not_RAW = rootgrp.createVariable("not_RAW", "f8", dimensions=("nrch"))
not_RAW.description = "Not Readily Available Water"

throughflow_res_time = rootgrp.createVariable("throughflow_res_time", "f8", dimensions=("nrch"))
throughflow_res_time.description = "Throughflow residence time"

interflow_res_time = rootgrp.createVariable("interflow_res_time", "f8", dimensions=("nrch"))
interflow_res_time.description = "Interflow residence time"

baseflow_res_time = rootgrp.createVariable("baseflow_res_time", "f8", dimensions=("nrch"))
baseflow_res_time.description = "Baseflow residence time"

throughflow_proportion = rootgrp.createVariable("throughflow_proportion", "f8", dimensions=("nrch"))
throughflow_proportion.description = "throughflow proportion"

interflow_proportion = rootgrp.createVariable("interflow_proportion", "f8", dimensions=("nrch"))
interflow_proportion.description = "interflow proportion"

baseflow_proportion = rootgrp.createVariable("baseflow_proportion", "f8", dimensions=("nrch"))
baseflow_proportion.description = "Baseflow proportion"

max_drainage = rootgrp.createVariable("max_drainage", "f8", dimensions=("nrch"))
max_drainage.description = "Max drainage"

max_infiltration = rootgrp.createVariable("max_infiltration", "f8", dimensions=("nrch"))
max_infiltration.description = "Max infiltration"

vol_soil_init = rootgrp.createVariable("vol_soil_init", "f8", dimensions=("nrch"))
vol_soil_init.description = "Initial soil volume"

vol_throughflow_init = rootgrp.createVariable("vol_throughflow_init", "f8", dimensions=("nrch"))
vol_throughflow_init.description = "Initial throughflow volume"

vol_interflow_init = rootgrp.createVariable("vol_interflow_init", "f8", dimensions=("nrch"))
vol_interflow_init.description = "Initial interflow volume"

vol_baseflow_init = rootgrp.createVariable("vol_baseflow_init", "f8", dimensions=("nrch"))
vol_baseflow_init.description = "Initial baseflow volume"

# Populate rain and pet with random values
for t in range(0, (5539 - 5511)):
    dailyRain = random.random() * 50
    dailyPet = random.random() * 5

    '''
    for rch in range(0, noReaches):
        rain[t, rch] = dailyRain * random.uniform(0.75, 1.25)
        pet[t, rch] = dailyPet * random.uniform(0.75, 1.25)
    '''

for rch in range(0, noReaches):

    # Populate rainmult with random values
    rainmultmin = 1.9
    rainmultmax = 2.3
    rainmult[rch] = rainmultmin + (random.random() * (rainmultmax - rainmultmin))  

    # Populate field capacity with random values
    fcapacitymin = 200
    fcapacitymax = 250
    field_capacity[rch] = fcapacitymin + (random.random() * (fcapacitymax - fcapacitymin))  

    # Populate not readily available water
    not_RAW[rch] = 0.5 * field_capacity[rch]

    # Populate throughflow residence time with random values
    Tfmin = 0.5
    Tfmax = 5
    throughflow_res_time[rch] = Tfmin + (random.random() * (Tfmax - Tfmin))

    # Populate interflow residence time with random values
    Timin = 3
    Timax = 40
    interflow_res_time[rch] = Timin + (random.random() * (Timax - Timin))

    # Populate baseflow residence time with random values
    Tbmin = 200
    Tbmax = 800
    baseflow_res_time[rch] = Tbmin + (random.random() * (Tbmax - Tbmin))

    # Set flow proportion max/mins
    Bpmin = 0.5
    Bpmax = 0.75
    Ipmin = 0.2
    Ipmax = 0.3
    Fpmin = 0.05
    Fpmax = 0.2

    baseflow_proportion[rch] = Bpmin + (random.random() * (Bpmax - Bpmin))  

    if baseflow_proportion[rch] + Fpmin + Ipmax > 1:
        Temp_Ipmax  =  1 - baseflow_proportion[rch] - Fpmin
    else:
        Temp_Ipmax = Ipmax

    # Make sure randomly selected interflow proportion (baseflow, min interflow) is less than 1)    
    interflow_proportion[rch] = min(1 - baseflow_proportion[rch], Ipmin + (random.random() * (Temp_Ipmax - Ipmin)))
    
    # Make sure randomly selected fastflow proportion (+baseflow and interflow) is less than 1.
    throughflow_proportion[rch] = 1 - baseflow_proportion[rch] - interflow_proportion[rch]

    # Populate max drainage with random values
    maxdrainmin = 100 #mm
    maxdrainmax = 200
    max_drainage[rch] = (maxdrainmin + (random.random() * (maxdrainmax - maxdrainmin))) / 24

    # Populate rainmult with random values
    maxinfmin = 200
    maxinfmax = 250
    max_infiltration[rch] = (maxinfmin + (random.random() * (maxinfmax - maxinfmin))) / 24

    # Populate volumes with constant values
    # vol_soil_init[rch] = 55.2627
    vol_soil_init[rch] = 220
    vol_throughflow_init[rch] = 0
    vol_interflow_init[rch] = 0
    vol_baseflow_init[rch] = 2664

rootgrp.close()
rootgrpIn.close()

'''
in_netCDF = r'C:\LUCI_data\Aparima\Climate_TimeSeries\rain_council_vclim_clidb_1972010100_2016123100_south-island_p05_daily_Aparima.nc'

rootgrp = Dataset(in_netCDF, "r", format="NETCDF4")
time_series = rootgrp.variables['time']
print(rootgrp.variables['time'])

rootgrp.close()
'''