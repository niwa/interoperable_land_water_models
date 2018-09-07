using DelftTools.Shell.Core;
using Mono.Addins;
using System.Collections.Generic;

namespace DeltaShell.Plugins.NiwaWebService
{
    [Extension(typeof(IPlugin))]
    public class NiwaWebServiceApplicationPlugin : ApplicationPlugin
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
                Name = "NIWA (Aquarius) Hydromet Retrieval",
                Category = "Web Services",
                CreateModel = o => new Models.NiwaWebService()
            };
        }
    }
}
