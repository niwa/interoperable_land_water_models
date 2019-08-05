## MakeEcotopes

### Contents
LUMASS model files

- **EcotopesPara.xls** -- Table with all configurable parameters of the model. You need to edit this table to adapt it to different datasets.
- **MakeEcotopes.lmv** -- Visual representation of the model. Drag this into the LUMASS desktop modelling environment to edit the structure of the model.
- **MakeEcotopes.lmx** -- The XML representation of the model as required by the LUMASS Engine.
- **MakeEcotopes.xml** -- An attempt at a DIMR configuration for the model.
- **MakeEcotopes.yaml** -- The YAML configuration file. 

#### EcotopesPara.xls

Each row in this table configures an individual input layer. To add more input layers to the 
model, just add more (up to 10 max!) rows to the table. 

*Parameters*
- **FileLable** -- Identifier used to refer to the original unique IDs of the individual input layers in the final output table.
- **ShapeFileName** -- If the input layer is a shapfile, this columns holds its name (without path!).
- **RaterizationAttribute** -- The shapfile attribute used to rasterize the layer. Please note that the pre-configured shapefiles have been altered to contain appropriated IDS, a manual step that is not covered by this model. The IDs must start with '1' (one!), since '0' is used to identify any 'nodata' values, i.e. outside the polygon boundaries.
- **RasterName** -- The name used to stored the rasterized shapefiles. If an input raster layer is configured, this should be its proper name.
- **Reclass** -- Continuous raster layers need to be reclassed using a nested ternary if-then-else expression. Thereby, `img` refers to the pixel value of the input image; the class values follow behind the `?`. Note that a `0` in the output image refers to a nodata pixel and values greater than zero to proper classes. 
- **CellSize** -- The cellsize in X and Y direction of the output raster. Note: all rows must contain the same values!
- **Extent** -- The physical extent of the region of interest, i.e. the target ouput region. Note: all input layers must have the same projection. The target extent must be the same for all layers!
- **Rasterize** -- Whether or not the given layer is a shape and needs to be rasterized `=1` or not `=0`. 
- **Reclassify** -- Whether or not the input layer needs to be reclassified `=1` or not `=0`. Note: Categorical input raster do not necessarily be re-classified, but most likely 'normalised', i.e. have 0-based pixel values. This could be done with the classification step. However, as of now attributes of those are not joined ot the final output table. 
- **Nodata** -- Nodata value of input layer.
- **JoinAttributes** -- Whether or not all attributes of a given original input layer should be joined to the final output table. Note: currently that's only applied to rasterized shapefiles.
- **EPSG** -- The EPSG code used during rasterization. Here: assumes all inputs and outputs are NZTM.