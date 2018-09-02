using System.Drawing;
using DelftTools.Shell.Core;
using DelftTools.Shell.Gui;
using Mono.Addins;
namespace DeltaShell.Plugins.TypologyLookup
{
    [Extension(typeof(IPlugin))]
    public class TypologyLookupGuiPlugin : GuiPlugin
    {
        public override string Name
        {
            get { return "TypologyLookupApplicationUI"; }
        }
        public override string DisplayName
        {
            get { return "Typology lookup application (UI)"; }
        }
        public override string Description
        {
            get { return "Gui plugin of the typology lookup application"; }
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
                return new Bitmap(Properties.Resources.plugin32);
            }
        }
    }
}