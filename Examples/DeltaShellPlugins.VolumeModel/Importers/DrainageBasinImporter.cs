using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using DelftTools.Shell.Core;
using DeltaShell.Plugins.VolumeModel.Models;
using GeoAPI.Extensions.Feature;
using NetTopologySuite.Extensions.Features;
using SharpMap.Data.Providers;
namespace DeltaShell.Plugins.VolumeModel.Importers
{
    public class DrainageBasinImporter : IFileImporter
    {
        public string Name { get { return "Shape file importer"; } }

        public string Category { get { return "General"; } }

        public Bitmap Image { get; private set; }
        public IEnumerable<Type> SupportedItemTypes
        {
            get { yield return typeof(DrainageBasin); }
        }
        public bool CanImportOnRootLevel { get { return false; } }
        public string FileFilter { get { return "Shape file|*.shp"; } }
        public string TargetDataDirectory { get; set; }
        public bool ShouldCancel { get; set; }
        public ImportProgressChangedDelegate ProgressChanged { get; set; }
        public bool OpenViewAfterImport { get { return false; } }
        public bool CanImportOn(object targetObject)
        {
            return targetObject is DrainageBasin;
        }
        public object ImportItem(string path, object target = null)
        {
            var basin = target as DrainageBasin;
            if (basin == null)
            {
                throw new Exception("Can only import on drainage basins");
            }

            basin.Catchments.Clear();

            var shapeFile = new ShapeFile(path);

            foreach (var feature in shapeFile.Features.OfType<IFeature>())
            {
                basin.Catchments.Add(new Feature
                {
                    Geometry = feature.Geometry,
                    Attributes = feature.Attributes
                });
            }
            basin.CoordinateSystem = shapeFile.CoordinateSystem;
            return basin;
        }
    }
}
