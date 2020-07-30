from netCDF4 import Dataset
import time
from datetime import datetime, timedelta
from netCDF4 import num2date, date2num
import yaml
import shutil

####################################################
### Copy input netCDF file to output netCDF file ###
####################################################
in_netCDF = r'C:\LUCI_data\Aparima\netCDF\streamq_5440662.nc'
rain_netCDF = r'C:\LUCI_data\Aparima\VCSN\vcsn_rain_aparima_19900101_20181015.nc'
out_netCDF = r'C:\LUCI_data\Aparima\netCDF\netCDF-template.nc'

# shutil.copyfile(in_netCDF, out_netCDF)

rootgrpIn = Dataset(in_netCDF, "r", format="NETCDF4")
reaches = rootgrpIn.variables['rchid']
reach_dim_in = rootgrpIn.dimensions['nrch']

rootgrpRain = Dataset(rain_netCDF, "r", format="NETCDF4")
time = rootgrpRain.variables['time']
time_dim_in = rootgrpRain.dimensions['time']

###############################################################
### Open netCDF file, read in variables and set up new ones ###
###############################################################
rootgrp = Dataset(out_netCDF, "w", format="NETCDF4", clobber=True)
rootgrp.title = "Output from rainfall runoff model"
rootgrp.institution = "Victoria University"
rootgrp.Conventions = "CF-1.7"
rootgrp.source = "LUCI"
rootgrp.comments = "Output from the Victoria University LUCI rainfall runoff model. The model outputs groundwater recharge and overland flow."

# Create time dimension
# time_dim = rootgrp.createDimension('time')

# Copy dimension for nrch
for dname, the_dim in rootgrpIn.dimensions.items():
    if dname == 'nrch':
        rootgrp.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

# Copy dimension for time
for dname, the_dim in rootgrpRain.dimensions.items():
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
for v_name, varin in rootgrpRain.variables.items():
    if v_name == 'time':
        print(v_name)
        print(varin)
        outVar = rootgrp.createVariable(v_name, varin.datatype, varin.dimensions)
        
        # Copy variable attributes
        outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})
        
        outVar[:] = varin[5511:5539]

# Create new variables
# time = rootgrp.createVariable("time", "i4", dimensions=("time"))
time = rootgrp.variables['time']
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
gw_flow = rootgrp.createVariable("ecotope_groundwater_recharge", "f8", dimensions=("time", "nrch"))
gw_flow.description = "Groundwater recharge is baseflow plus interflow"
gw_flow.standard_name = "(no standard name)"
gw_flow.long_name = "Groundwater recharge (m3 s-1)"
gw_flow.cdsm_name = "soil_water_sat-zone_top__recharge_volume_flux"
gw_flow.units = "m3 s-1"
gw_flow[:] = float(1)

quick_flow = rootgrp.createVariable("ecotope_overlandflow", "f8", dimensions=("time", "nrch"))
quick_flow.description = "The overland flow is throughflow plus overland flow"
quick_flow.standard_name = "(no standard name)"
quick_flow.cdsm_name = "land_surface_water__runoff_volume_flux"
quick_flow.long_name = "Overland flow (m3 s-1)"
quick_flow.units = "m3 s-1"
quick_flow[:] = float(1)

rootgrp.close()
rootgrpIn.close()
rootgrpRain.close()

'''
in_netCDF = r'C:\LUCI_data\Aparima\Climate_TimeSeries\rain_council_vclim_clidb_1972010100_2016123100_south-island_p05_daily_Aparima.nc'

rootgrp = Dataset(in_netCDF, "r", format="NETCDF4")
time_series = rootgrp.variables['time']
print(rootgrp.variables['time'])

rootgrp.close()
'''