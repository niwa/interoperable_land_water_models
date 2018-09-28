using DelftTools.Controls;
using DelftTools.Shell.Core;
using DelftTools.Shell.Gui;
using Mono.Addins;
using System.Collections.Generic;
using System.Drawing;

using DeltaShell.Plugins.TypologyLookup.DataObjects;
using DeltaShell.Plugins.TypologyLookup.NodePresenters;
using DeltaShell.Plugins.TypologyLookup.ObjectProperties;
using DeltaShell.Plugins.TypologyLookup.Views;
using DelftTools.Utils.Collections.Generic;

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
            get { return "Typology Lookup application (UI)"; }
        }
        public override string Description
        {
            get { return "Gui plugin of the Typology Lookup application"; }
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
                return new Bitmap(Properties.Resources.plugin_icon32);
            }
        }
        public override IEnumerable<ViewInfo> GetViewInfoObjects()
        {
            yield return new ViewInfo<TypologyTable, TypologyTableView>
            {
                GetViewName = (v, o) => "Typology Table"
            };
            yield return new ViewInfo<LookupKey, LookupKeyView>
            {
                GetViewName = (v, o) => "Typology Table"
            };
        }
        public override IEnumerable<ITreeNodePresenter> GetProjectTreeViewNodePresenters()
        {
            yield return new TypologyTableNodePresenter();
        }
        public override IEnumerable<PropertyInfo> GetPropertyInfos()
        {
            yield return new PropertyInfo<LookupKey, LookupKeyProperties>();
            yield return new PropertyInfo<LookupValue, LookupValueProperties>();
            yield return new PropertyInfo<TypologyTable, TypologyTableProperties>();
        }
    }
}
