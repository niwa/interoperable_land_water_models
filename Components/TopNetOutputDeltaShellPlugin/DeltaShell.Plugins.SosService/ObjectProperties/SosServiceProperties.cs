using System.ComponentModel;
using DelftTools.Shell.Gui;
using System;

namespace DeltaShell.Plugins.SosService.ObjectProperties
{
    [DisplayName("SOS Query model information")]
    public class VolumeModelProperties : ObjectProperties<Models.SosService>
    {
        [Category("General")]
        [DisplayName("Name")]
        [Description("Name of this Web Service Model")]
        public string Name
        {
            get { return data.Name; }
            set { data.Name = value; }
        }

        [Category("Input")]
        [DisplayName("Property")]
        [Description("Selected property (Needs to be QR or HG)")]
        public string Property
        {
            get { return data.Property; }
            set { data.Property = value; }
        }

        [Category("Input")]
        [DisplayName("Station ID")]
        [Description("Selected station ID")]
        public string Station
        {
            get { return data.Station; }
            set { data.Station = value; }
        }

        [Category("Input")]
        [DisplayName("Starting Date")]
        [Description("Starting date in format YYYY-MM-DD")]
        public string StartDate
        {
            get { return data.StartDate; }
            set { data.StartDate = value; }
        }
        [Category("Input")]
        [DisplayName("End Date")]
        [Description("End date in format YYYY-MM-DD")]
        public string EndDate
        {
            get { return data.EndDate; }
            set {
                data.EndDate = value;
            }
        }

    }
}