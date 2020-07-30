using DelftTools.Shell.Core;
using Mono.Addins;
using System.Collections.Generic;

namespace DeltaShell.Plugins.SosService
{
    [Extension(typeof(IPlugin))]
    public class SosServiceApplicationPlugin : ApplicationPlugin
    {
        public override string Name
        {
            get { return "SosServiceApplication"; }
        }
        public override string DisplayName
        {
            get { return "Sos Service application"; }
        }
        public override string Description
        {
            get { return "Application plugin to query SOS NIWA service"; }
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
                Name = "Sos Service",
                Category = "Web services",
                CreateModel = o => new Models.SosService()
            };

            yield return new ModelInfo
            {
                Name = "Sos Service Pre Query",
                Category = "Web services",
                CreateModel = o => new Models.SosServicePreQuery()
            };
        }
    }
}