import sys
import clr
import System

# Computer- and Project-specific settings
pathToDeltaShell = r"C:\Models\Programs\DeltaShell\1.3.0.40607\DeltaShell"
catchmentLayerName = "Aparima_catchments"
reachLayerName = "Aparima_segments"


sys.path.append(pathToDeltaShell)
clr.AddReference("NetTopologySuite.dll")
clr.AddReference("NetTopologySuite.Extensions.dll")
from NetTopologySuite.Extensions import Coverages, Features

DEBUG = True

if DEBUG:
    print "DeltaShell Version: " + Gui.Application.Version
    print "Running from: " + Gui.Application.WorkDirectory

rootItems = Gui.Application.Project.RootFolder.DataItems
reachCoverage = Coverages.FeatureCoverage("Reach")
catchmentCoverage = Coverages.FeatureCoverage("Catchment")
activeCoverage = None

for item in rootItems:
    if DEBUG:
        print item.Name + ': ' + item.ValueType.ToString()
    if item.ValueType.ToString() == "SharpMap.Map":
        theMap = item.Value
        theLayers = theMap.Layers
        print "{} has {} layers.".format(item.Name, len(theLayers)) 
        for l in theLayers:
            nStr = str(l.Name)
            print nStr
            if nStr.strip().upper() == catchmentLayerName.strip().upper():
                catchmentCoverage.Features.Clear()
                activeCoverage = catchmentCoverage
                print "Catchment coverage active"
            elif nStr.strip().upper() == reachLayerName.strip().upper():
                reachCoverage.Features.Clear()
                activeCoverage = reachCoverage
                print "Reach coverage active"
            else:
                activeCoverage = None

            count = 0
            for feature in l.GetFeatures(l.Envelope):
                if count < 2:
                    print type(feature) #catchmentCoverage.Features.Add(feature)
                    print feature.Attributes
                    print type(activeCoverage.Features)
                    print type(feature)
                #if activeCoverage is not None:
                    #print "Adding features to " + activeCoverage.Name
                    #newFeature = Features.Feature.Feature(feature)
                    #Feature()
                    #newFeature.Geometry.Clone(feature.Geometry)
                    #newFeature.Attributes.Clone(feature.Attributes)
                    #activeCoverage.Features.Add(feature)
                count += 1
            print "{} has {} features.".format(activeCoverage.Name, count) 

catchmentCoverage.IsTimeDependent = False
print catchmentCoverage.Name + " has " #
count = 0
for thing in catchmentCoverage.Attributes:
    count += 1
print count
print catchmentCoverage.Features.ToString()
#reachCoverage.
print catchmentCoverage.CoordinateSystem