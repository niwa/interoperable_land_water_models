## AparimaOptimisation
### Contents ###

- **AparimaBatchScenarios** - A folder containing the scenario settings files for exploring the impact of N leaching and sediment yield reduction payments on economic and environmental indicators.
- **los_files** - A folder containing the single objective optimisation scenarios files, e.g. [Aparima_minNLeach.los](los_files/Aparima_minNLeach.los).
- **AparimaOptimisation.yaml** - The configuration file required by the BMI-compliant LUMASS engine.
- **combParcLU.img** An ERDAS Imagine file showing an overlay of LINZ primary parcels, and land-use / land-cover information. Additional information has been summarised for the spatial units resulting from this overlay that is being used in the optimisation scenarios. Please refer to `combParcLU.ldb` item below for an overview of the provided information. 
- **combParcLU.img.aux.xml** - Auxilliary file containing GDAL metadata for this layer.
- **compParcLU.ldb** - The LUMASS SQLite-based raster attribute table (RAT) for this layer. Please find below a list of columns and their meaning. Column values represent values for individual spatial units (map categories or zones). The optimisation allocates land uses to those zones.
    - **rowidx** (int) - unique identifier for zones created through overlay process
    - **zone_id** (int) - same as rowidx
    - **count** (int) - number of pixels
    - **min** (int) - minimum rowidx value
    - **max** (int) - maximum rowidx value
    - **mean** (int) - mean rowidx value
    - **stddev** (int) - standard deviation for rowidx values
    - **sum** (int) - sum of rowidx values
    - **minX** (int) - mininum x-coordinate (in pixel coordinates, i.e. rows & columns) 
    - **minY** (int) - minimum y-coordinate
    - **maxX** (int) - maximum x-coordinate
    - **maxY** (int) - maximum y-cooridnate
    - **CurLu** (real) - mean value of (current) input land use identifer
    - **LuID** (int) - rounded land use identifier (round(CurLu))
    - **LuOpt** (text) - (current) land use short name 
    - **LuDescr** (text) - (current) land use description
    - **AreaHA** (real) - size in hectares
    - **ForRev** (real) - revenue under exotic forest
    - **NatEro** (real) - sediment yield (t/ha/yr) under native woody vegetation
    - **ForEro** (real) - sediment yield (t/ha/yr) under exotic forest
    - **OthEro** (real) - sediment yield (t/ha/yr) under under non-woody vegetation
    - **DaiESNl** (real) - spatially variable nitrate leaching (kg N/ha/yr); based on NZLRI carrying capacity and N leaching factor per cow based on Dymond et al. (2013)
    - **Dai3Nl..Dai5Nl** (real) - avg N leaching (kg N/ha/yr) per dairy system
    - **MeanSlCl** (real) - mean slope class (based on Annette's slope class layer)
    - **SnbEsNl** (real) - spatially variable nitrate leaching (kg N/ha/hr) for avg. stocking rate and N leaching factor per animal type based on Dymond et al. (2013)
    - **SnbHNl..SnbIfNl** (real) - avg. N leaching (kg N/ha/yr) for different SNB systems
    - **DaiAvgMs** (real) - avg. milk solids production (kg Ms/ha/yr) across dairy systems
    - ... ***TBC***
    