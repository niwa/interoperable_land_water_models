using System.Collections.Generic;
using System.Drawing;
using DelftTools.Controls;
using DelftTools.Controls.Swf.TreeViewControls;
using DelftTools.Utils.Collections.Generic;
using DeltaShell.Plugins.TypologyLookup.DataObjects;

namespace DeltaShell.Plugins.TypologyLookup.NodePresenters
{
    class TypologyTableNodePresenter : TreeViewNodePresenterBase<TypologyTable>
    {
        private static readonly Image _image = Properties.Resources.table16;

        public override void UpdateNode(ITreeNode parentNode, ITreeNode node, TypologyTable nodeData)
        {
            bool hasData = nodeData.Rows.Count > 0;
            node.Text = string.Format("Table {0}", hasData ? "" : "Empty");
            node.Image = _image;
        }
    }
}