import xarray as xr

rain_netCDF = r'C:\LUCI_data\Aparima\VCSN\vcsn_rain_aparima_19900101_20181015.nc'
pet_netCDF = r'C:\LUCI_data\Aparima\VCSN\vcsn_pet_aparima_19900101_20181015.nc'

ds_rain = xr.open_dataset(rain_netCDF)
rain = ds_rain['rain']

ds_pet = xr.open_dataset(pet_netCDF)
pet = ds_pet['pet']

rain_time = []
for t in range(0, 10470):
	rain_time.append((t, float(rain[t][10][10])))

rain_time.sort(key=lambda tup: tup[1])
print(rain_time)
'''

pet_time = []
for t in range(0, 10470):
	pet_time.append((t, float(pet[t][0][0])))

pet_time.sort(key=lambda tup: tup[1])
print(pet_time)
'''

ds_rain.close()
ds_pet.close()


