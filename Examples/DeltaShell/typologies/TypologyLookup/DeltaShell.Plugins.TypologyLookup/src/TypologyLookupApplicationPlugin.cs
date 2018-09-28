using DelftTools.Shell.Core;
using Mono.Addins;
using System.Collections.Generic;
using System.Drawing;

using DeltaShell.Plugins.TypologyLookup.Importers;

namespace DeltaShell.Plugins.TypologyLookup
{
    [Extension(typeof(IPlugin))]
    public class TypologyLookupApplicationPlugin : ApplicationPlugin
    {
        public override string Name
        {
            get { return "NIWA Web Service"; }
        }
        public override string DisplayName
        {
            get { return "NIWA Web Service application"; }
        }
        public override string Description
        {
            get { return "Application plugin for NIWA web service"; }
        }
        public override string Version
        {
            get { return "1.0"; }
        }
        public override string FileFormatVersion
        {
            get { return "1.0"; }
        }
        public override IEnumerable<ModelInfo> GetModelInfos()
        {
            yield return new ModelInfo
                {
                    Name = "Typology Lookup",
                    Category = "Lookup Tables",
                    CreateModel = o => new Models.Lookup()
                };
        }
        public override IEnumerable<IFileImporter> GetFileImporters()
        {
            yield return new CsvTableImporter();
        }
        public override Image Image
        {
            get
            {
                return new Bitmap(Properties.Resources.plugin_icon32);
            }
        }
    }
}
