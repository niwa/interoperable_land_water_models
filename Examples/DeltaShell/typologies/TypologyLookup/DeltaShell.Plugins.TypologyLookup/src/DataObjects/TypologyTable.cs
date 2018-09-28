using DelftTools.Utils.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeltaShell.Plugins.TypologyLookup.DataObjects
{
    class TypologyTable
    {
        // column labels (same for all tables)
        private static readonly List<string> _substances = new List<string> { "N", "P" };
        // row labels
        private List<string> _typologies;
        // row collections, same order as _typologies
        private List<TypologyTableRow> _table;

        public TypologyTable()
        {
            _typologies = new List<string>();
            _table = new List<TypologyTableRow>();
        }

        public ReadOnlyCollection<string> Typologies
        {
            get { return _typologies.AsReadOnly(); }
        }

        public ReadOnlyCollection<string> Substances
        {
            get { return _substances.AsReadOnly(); }
        }

        public List<TypologyTableRow> Rows
        {
            get { return _table; }
        }

        public TypologyTableRow GetRow(string typology)
        {
            TypologyTableRow row = null;
            if (_typologies.Contains(typology))
            {
                row = _table[_typologies.IndexOf(typology)];
            }
            return row;
        }

        public void AddRow(string typology, double n, double p)
        {
            TypologyTableRow row = GetRow(typology);

            if (row is null)
            {
                row = new TypologyTableRow();
                _typologies.Add(typology);
                _table.Add(row);
            }
            row.Typology = typology;
            row.N = n;
            row.P = p;
        }

        public void Clear()
        {
            _typologies.Clear();
            _table.Clear();
        }
    }
}
