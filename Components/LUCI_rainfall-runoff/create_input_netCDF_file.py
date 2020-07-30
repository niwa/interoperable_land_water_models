from netCDF4 import Dataset
import arcpy
import os
import sys
import datetime
from datetime import date

arcpy.env.overwriteOutput = True

def GetYear(dimension_value):
    lst1 = dimension_value.split(' ')
    if '/' in dimension_value:
        year = lst1[0].split('/')[2]
    elif '-' in dimension_value:
        year = lst1[0].split('-')[2]
    else:
        year = '1899'
    return int(year)

def GetMonth(dimension_value):
    lst1 = dimension_value.split(' ')
    if '/' in dimension_value:
        month = lst1[0].split('/')[1]
    elif '-' in dimension_value:
        month = lst1[0].split('-')[1]
    else:
        month = '13'
    return int(month)

def GetDay(dimension_value):
    lst1 = dimension_value.split(' ')
    if '/' in dimension_value:
        day = lst1[0].split('/')[0]
    elif '-' in dimension_value:
        day = lst1[0].split('-')[0]
    else:
        day = '32'
    return int(day)

def daterange(d1, d2):
    return (d1 + datetime.timedelta(days=i) for i in range((d2 - d1).days + 1))

'''
# Open Jing's netCDF files to get the reach ids - put into list
jing_netCDF = r'C:\LUCI_data\Aparima\netCDF\streamq_5440662.nc'
root_grp_jing = Dataset(jing_netCDF, "r", format="NETCDF4")
reaches = root_grp_jing.variables['rchid']
reach_ids = list(reaches[:])
root_grp_jing.close()
'''

# Set time bounds
start_date = date(2005, 2, 10)
end_date = date(2005, 2, 13)

# Define rainfall and pet netCDF files
rain_netCDF = r'C:\LUCI_data\Aparima\VCSN\vcsn_rain_aparima_19900101_20181015.nc'
pet_netCDF = r'C:\LUCI_data\Aparima\VCSN\vcsn_pet_aparima_19900101_20181015.nc'

nc_FP = arcpy.NetCDFFileProperties(rain_netCDF)
print(nc_FP)

nc_Dim = nc_FP.getDimensions()
print(nc_Dim)

print(arcpy.env.scratchFolder)



'''
for dimension in nc_Dim:

    if dimension == "time":
        top = nc_FP.getDimensionSize(dimension)
        for i in range(int(start_date - epoch_date), int(end_date - epoch_date) + 1):

            dimension_values = nc_FP.getDimensionValue(dimension, i)
            currentDate = date(GetYear(dimension_values),
                                   GetMonth(dimension_values),
                                   GetDay(dimension_values))

            if currentDate >= start_date and currentDate <= end_date:
                print(currentDate)

'''


# Loop through the time steps
for d in daterange(start_date, end_date):

    dimension_values = "time '" + d.strftime("%d/%m/%Y") + " 09:00:00 AM'"
    filenameSuffix = d.strftime('%Y-%m-%d')

    rain_raster = os.path.join(arcpy.env.scratchFolder, 'rain_raster_' + filenameSuffix + '.tif')
    pet_raster = os.path.join(arcpy.env.scratchFolder, 'pet_raster_' + filenameSuffix + '.tif')
    out_raster_layer = "out_raster_layer"
    print(rain_raster)
    print(dimension_values)

    # arcpy.MakeNetCDFRasterLayer_md(in_netCDF_file="C:\\LUCI_data\\Aparima\\VCSN\\rain_vclim_clidb_1972010100_2018101500_south-island_p05_daily_Aparima.nc", variable="rain", x_dimension="longitude", y_dimension="latitude", out_raster_layer=out_raster_layer, band_dimension="", dimension_values=dimension_values, value_selection_method="BY_VALUE")

    # Create raster layers from netCDF files
    arcpy.MakeNetCDFRasterLayer_md(in_netCDF_file=rain_netCDF,
                                   variable="rain",
                                   x_dimension="longitude",
                                   y_dimension="latitude",
                                   out_raster_layer=out_raster_layer,
                                   dimension_values=dimension_values,
                                   value_selection_method="BY_VALUE")

    print('Created raster layer')
    arcpy.CopyRaster_management(out_raster_layer, rain_raster)
    print('Saved tiff')


sys.exit()

# Define Strahler 1 shp
reaches_shp = r'C:\LUCI_data\Aparima\GIS\NIWA\Strahler1_Watershed_DN3_Aparima.shp'

# Project rasters to same coordinate system as reaches shapefile
spat_ref = arcpy.Describe(reaches_shp).spatialReference
rain_raster_proj = os.path.join(arcpy.env.scratchFolder, 'rain_raster_proj.tif')
pet_raster_proj = os.path.join(arcpy.env.scratchFolder, 'pet_raster_proj.tif')
arcpy.ProjectRaster_management(in_raster=rain_raster, out_raster=rain_raster_proj, out_coor_system=spat_ref)
arcpy.ProjectRaster_management(in_raster=pet_raster, out_raster=pet_raster_proj, out_coor_system=spat_ref)

# Open Strahler 1 shp and loop through reach ids list
with arcpy.da.SearchCursor(reaches_shp, ['nzseg_v3', 'SHAPE@']) as search_cursor:

    for row in search_cursor:
        reach_id = row[0]
        shape = row[1]

        if reach_id in reach_ids:

            # Find rainfall value from closest grid square to polygon centrepoint
            rasterValueAtPoint = arcpy.GetCellValue_management(rain_raster_proj, shape.centroid).getOutput(0)
            print(rasterValueAtPoint)

            # Use rain and pet data from this square
            # Generate soil params for reach

            # Write data to input netCDF file

# Close off netCDF files
