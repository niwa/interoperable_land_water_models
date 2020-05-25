import urllib
from Libraries.NetCdfFunctions import *
# import standard scripting library
from Libraries.StandardFunctions import *
from Libraries.MapFunctions import *
from System import Array
import csv
import os

# import our model
from DeltaShell.Plugins.VolumeModel.Models import VolumeModel
 
# import some DeltaShell (C#) libraries
from NetTopologySuite.Extensions.Features import Feature
from NetTopologySuite.Extensions.Geometries import GeometryHelper
 
# import .Net library
from System import DateTime
 
# create an instance of our model and set the name
model = VolumeModel()
model.Name = "My test model3"
 
# Generate Precipitation data
startDate = DateTime.Now

newFeatures = []
for feature in GetShapeFileFeatures("D:\\bkp_1017094_10marc2020\DeltaShell\Aparima\Aparima_catchments.shp"):
    feature = Feature(Geometry = feature.Geometry)
    model.Basin.Catchments.Add(feature)

 
#for i in range(0,10):
 #   currentDateTime = startDate.AddHours(i)
 #   model.Precipitation[currentDateTime] = i * 1.0 + 1000 # convert i to double

file = NetCdfFile.OpenExisting("D:\\bkp_1017094_10marc2020\DeltaShell\Aparima\SMA.Jan.2005.nc")

# show all variables
for var in file.GetVariables():
    print file.GetVariableName(var)

# Create a CSV file
path = os.path.expanduser('~')+'\AppData\Local\Temp'
filepath = path + r'\test2.csv'
print filepath 
if os.path.exists(filepath):
	os.remove(filepath)

val=[]
gflow = file.Read(file.GetVariableByName("ecotope_fast_flow"))

# Create a CSV file
with open(filepath, 'w') as csvfile: # create the file test2.csv
    writer = csv.writer(csvfile, delimiter=',',lineterminator='\n') # create writer
    for k in range(532):
    		writer.writerow(GetArrayFromSecondDimention(gflow, k, 15))

#read down a matrix - rows
val[:] = GetArrayFromSecondDimention(gflow, 0, 15)
for i in range(15):
	currentDateTime = startDate.AddHours(i)
	model.Precipitation[currentDateTime] = val[i:i+1]	
   
# Add model to project
AddToProject(model)
 
# Run the model
RunModel(model, True)
 
# Open a view for the model
OpenView(model)
