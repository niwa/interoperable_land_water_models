using DelftTools.Utils.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeltaShell.Plugins.TypologyLookup.DataObjects
{
    class TypologyTableRow
    {
        private string _typology;
        private double _n;
        private double _p;

        public TypologyTableRow()
        {

        }

        public TypologyTableRow(string typology, double n, double p)
        {
            _typology = typology;
            _n = n;
            _p = p;
        }

        public string Typology
        {
            get { return _typology;  }
            set { _typology = value;  }
        }

        public double N
        {
            get { return _n; }
            set { _n = value; }
        }

        public double P
        {
            get { return _p; }
            set { _p = value; }
        }
    }
}
