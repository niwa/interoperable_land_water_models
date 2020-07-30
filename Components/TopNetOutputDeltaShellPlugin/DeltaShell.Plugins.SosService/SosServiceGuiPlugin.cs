using DelftTools.Shell.Core;
using DelftTools.Shell.Gui;
using Mono.Addins;
using System.Collections.Generic; // IEnumerable

namespace DeltaShell.Plugins.SosService
{
    [Extension(typeof(IPlugin))]
    public class SosServiceGuiPlugin : GuiPlugin
    {
        public override string Name
        {
            get { return "SosServiceApplicationUI"; }
        }
        public override string DisplayName
        {
            get { return "Sos Service application (UI)"; }
        }
        public override string Description
        {
            get { return "Gui plugin to query SOS NIWA service"; }
        }
        public override string Version
        {
            get { return "1.0"; }
        }
        public override IEnumerable<PropertyInfo> GetPropertyInfos()
        {
            yield return new PropertyInfo<Models.SosService, ObjectProperties.VolumeModelProperties>();
        }
        public override string FileFormatVersion
        {
            get { return "1.0"; }
        }
    }
}