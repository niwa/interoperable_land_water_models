using System.Drawing;
using System.Linq;
using DelftTools.Controls;
using DelftTools.Controls.Swf.TreeViewControls;
using DeltaShell.Plugins.VolumeModel.Models;
namespace DeltaShell.Plugins.VolumeModel.Nodepresenters
{
    public class DrainageBasinTreeViewNodePresenter : TreeViewNodePresenterBase<DrainageBasin>
    {
        private static readonly Image DrainageBasinImage = DeltaShellPlugins.VolumeModel.Properties.Resources.basin;

        public override void UpdateNode(ITreeNode parentNode, ITreeNode node, DrainageBasin nodeData)
        {
            var hasCatchments = nodeData.Catchments.Any();
            node.Text = string.Format("Drainage basin{0}", hasCatchments ? "" : " (Empty)");
            node.Image = DrainageBasinImage;
        }
    }
}