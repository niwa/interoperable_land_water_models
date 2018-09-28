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
    class TypologyTableProperties : ObjectProperties<TypologyTable>
    {
        [Category("General")]
        [DisplayName("Number of typologies")]
        [Description("Number of typology rows in typology table")]
        public int NumberOfTypologies
        {
            get { return data.Typologies.Count; }
        }

        [Category("General")]
        [DisplayName("Number of substances")]
        [Description("Number of substance columns in typology table")]
        public int NumberOfSubstances
        {
            get { return data.Substances.Count; }
        }
    }
}
