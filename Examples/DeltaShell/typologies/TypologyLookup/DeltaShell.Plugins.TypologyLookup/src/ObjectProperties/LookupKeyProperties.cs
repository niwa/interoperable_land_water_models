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
    class LookupKeyProperties : ObjectProperties<LookupKey>
    {
        [Category("General")]
        [DisplayName("Value")]
        [Description("Value to look up")]
        public string Value
        {
            get { return data.Value; }
        }
    }
}
