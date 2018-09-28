using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeltaShell.Plugins.TypologyLookup.DataObjects
{
    class LookupKey
    {
        private Models.Lookup _parent;
        private string _name;
        private string _value;

        public LookupKey(Models.Lookup parent, string name)
        {
            _parent = parent;
            _name = name;
            _value = "";
        }

        public List<string> GetPossibleValues()
        {
            return _parent.GetRowLabels();
        }

        public string Name { get { return _name; } }
        public string Value
        {
            get { return _value; }
            set
            {
                _value = value;
                _parent.Update();
            }
        }
    }
}
