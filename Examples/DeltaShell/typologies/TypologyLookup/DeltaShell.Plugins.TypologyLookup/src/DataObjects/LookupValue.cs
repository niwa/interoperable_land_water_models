using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeltaShell.Plugins.TypologyLookup.DataObjects
{
    class LookupValue
    {
        private string _name;
        private string _long_name;
        private string _units;

        public LookupValue(string name, string long_name, string units)
        {
            _name = name;
            _long_name = long_name;
            _units = units;

        }

        public string Name { get { return _name; } }
        public string LongName { get { return _long_name; } }
        public string Units { get { return _units; } }
        public double Value { get; set; }
    }
}
