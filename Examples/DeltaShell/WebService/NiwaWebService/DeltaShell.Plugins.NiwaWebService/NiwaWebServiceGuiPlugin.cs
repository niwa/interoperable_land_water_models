using DelftTools.Shell.Core;
using DelftTools.Shell.Gui;
using Mono.Addins;
using System.Drawing;

namespace DeltaShell.Plugins.NiwaWebService
{

    [Extension(typeof(IPlugin))]
    public class NiwaWebServiceGuiPlugin : GuiPlugin
    {
        public override string Name
        {
            get { return "NiwaWebServiceApplicationUI"; }
        }
        public override string DisplayName
        {
            get { return "NIWA Web Service application (UI)"; }
        }
        public override string Description
        {
            get { return "Gui plugin of the  Web Service Model application"; }
        }
        public override string Version
        {
            get { return "1.0"; }
        }
        public override string FileFormatVersion
        {
            get { return "1.0"; }
        }
        public override Image Image
        {
            get
            {
                return new Bitmap(Properties.Resources.net32);
            }
        }
    }
}
