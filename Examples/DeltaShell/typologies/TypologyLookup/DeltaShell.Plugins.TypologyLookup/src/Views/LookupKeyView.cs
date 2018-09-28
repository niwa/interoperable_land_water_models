using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using DelftTools.Controls;
using DeltaShell.Plugins.TypologyLookup.DataObjects;

namespace DeltaShell.Plugins.TypologyLookup.Views
{
    public partial class LookupKeyView : UserControl, IView
    {
        private LookupKey _data;
        private BindingSource _bSource;

        public LookupKeyView()
        {
            InitializeComponent();

            _bSource = new BindingSource();

            // Bind the BindingSource to the data type.
            _bSource.DataSource = typeof(string);

            // Bind the TableView control to the BindingSource.
            listBox1.DataSource = _bSource;
        }

        public object Data
        {
            get { return _data; }
            set
            {
                if (_data != null)
                {
                    listBox1.SelectedIndexChanged -= HandleSelectedIndexChanged;
                }

                _data = (LookupKey)value;

                if (_data == null)
                {
                    _bSource.DataSource = null;
                }
                else
                {
                    List<string> possible_values = _data.GetPossibleValues();
                    _bSource.DataSource = possible_values;
                    if (possible_values.Contains(_data.Value))
                    {
                        listBox1.SetSelected(possible_values.IndexOf(_data.Value), true);
                    }
                    listBox1.SelectedIndexChanged += HandleSelectedIndexChanged;
                }
                
            }
        }

        public Image Image { get; set; }
        public ViewInfo ViewInfo { get; set; }
        public void EnsureVisible(object item) { }

        private void HandleSelectedIndexChanged(object sender, System.EventArgs e)
        {
            if (listBox1.SelectedValue == null) return;
            _data.Value = (string)listBox1.SelectedValue;
        }
    }
}
