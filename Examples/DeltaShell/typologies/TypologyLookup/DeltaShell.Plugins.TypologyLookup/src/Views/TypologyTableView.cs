using DelftTools.Controls;
using System.Drawing;
using DelftTools.Utils.Collections.Generic;
using System.Windows.Forms;

using DeltaShell.Plugins.TypologyLookup.DataObjects;
using System.Collections.Generic;

namespace DeltaShell.Plugins.TypologyLookup.Views
{
    public partial class TypologyTableView : UserControl, IView
    {
        private TypologyTable _table;
        private List<TypologyTableRow> _rows;
        private BindingSource _bSource;

        public TypologyTableView()
        {
            InitializeComponent();
            _bSource = new BindingSource();

            // Bind the BindingSource to the data type.
            _bSource.DataSource = typeof(TypologyTableRow);

            // Bind the TableView control to the BindingSource.
            tableView1.Data = _bSource;
        }

        public object Data
        {
            get { return _table; }
            set
            {
                if (_table != null)
                {
                    //_data.CollectionChanging -= SourceEmissionTypesCollectionChanging;
                    //_data.CollectionChanged -= refreshTableView;
                    //((INotifyPropertyChanged)_data).PropertyChanged -= refreshTableView;
                }

                _table = (TypologyTable)value;

                if (_table == null)
                {
                    _bSource.DataSource = null;
                }
                else
                {
                    _rows = new List<TypologyTableRow>();
                    foreach (var row in _table.Rows)
                    {
                        _rows.Add(row);
                    }

                    _bSource.DataSource = _rows;

                    //_data.CollectionChanging += SourceEmissionTypesCollectionChanging;
                    //_data.CollectionChanged += refreshTableView;
                    //((INotifyPropertyChanged)_data).PropertyChanged += refreshTableView;

                    tableView1.BestFitColumns(false);
                }
            }
        }

        public Image Image { get; set; }
        public ViewInfo ViewInfo { get; set; }
        public void EnsureVisible(object item) { }
    }
}
