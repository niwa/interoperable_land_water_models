using System.Drawing;
using System.Windows.Forms;
using DelftTools.Shell.Gui;
using DeltaShell.Plugins.VolumeModel.Importers;
namespace DeltaShell.Plugins.VolumeModel.Commands
{
    internal class AddInputDataToVolumeModelCommand : IGuiCommand
    {
        /// <summary>
        /// The name of the gui command
        /// </summary>
        public string Name
        {
            get { return "Add input data"; }
        }
        /// <summary>
        /// Ensures the gui command is enabled for volume models only
        /// </summary>
        public bool Enabled
        {
            get { return Gui != null && Gui.Selection is Models.VolumeModel; }
        }
        /// <summary>
        /// The image of the gui command
        /// </summary>
        public Image Image { get; set; }
        /// <summary>
        /// Whether or not the gui command is checked
        /// </summary>
        /// <remarks>Not relevant in this tutorial</remarks>
        public bool Checked { get; set; }
        /// <summary>
        /// A reference to the Delta Shell gui (automatically set by Delta Shell logic)
        /// </summary>
        public IGui Gui { get; set; }
        /// <summary>
        /// The action that should be performed while executing the gui command
        /// </summary>
        public void Execute(params object[] arguments)
        {
            // Obtain the selected volume model
            var volumeModel = (Models.VolumeModel)Gui.SelectedModel;
            // Try to obtain a precipitation file via a file dialog
            var fileDialog = new OpenFileDialog
            {
                Title = "Select precipitation time series",
                Filter = "WaterML2 files|*.XML",
                Multiselect = false
            };
            if (fileDialog.ShowDialog() != DialogResult.OK)
            {
                return;
            }
            // Create a WaterML2 time series importer
            var waterML2TimeSeriesImporter = new WaterML2TimeSeriesImporter();
            // Import the data from the precipitation file
            waterML2TimeSeriesImporter.ImportItem(fileDialog.FileName, volumeModel.Precipitation);
            // Try to obtain a shape file via a file dialog
            fileDialog = new OpenFileDialog
            {
                Title = "Select basin shape file",
                Filter = "Shape files|*.shp",
                Multiselect = false
            };
            if (fileDialog.ShowDialog() != DialogResult.OK)
            {
                return;
            }
            // Create a DrainageBasinImporter importer
            var drainageBasinImporter = new DrainageBasinImporter();
            // Import the data from the shape file
            drainageBasinImporter.ImportItem(fileDialog.FileName, volumeModel.Basin);
        }
        /// <summary>
        /// The action that should be performed in order to undo execute actions of the gui command
        /// </summary>
        /// <remarks>Not relevant in this tutorial</remarks>
        public void Unexecute()
        {
        }
        
    }
}
 