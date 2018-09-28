using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.ComponentModel;
using DelftTools.Shell.Gui;

using DeltaShell.Plugins.TypologyLookup.DataObjects;
using DelftTools.Utils.Collections.Generic;

namespace DeltaShell.Plugins.TypologyLookup.ObjectProperties
{
    [DisplayName("Typology table information")]
    class LookupValueProperties : ObjectProperties<LookupValue>
    {
        [Category("General")]
        [DisplayName("Long Name")]
        [Description("Full name of looked up variable")]
        public string LongName
        {
            get { return data.LongName; }
        }

        [Category("General")]
        [DisplayName("Units")]
        [Description("Units of looked up variable")]
        public string Units
        {
            get { return data.Units; }
        }

        [Category("General")]
        [DisplayName("Value")]
        [Description("Value of looked up variable")]
        public double Value
        {
            get { return data.Value; }
        }
    }
}
