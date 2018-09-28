using DelftTools.Shell.Core.Workflow;
using DelftTools.Shell.Core.Workflow.DataItems;
using DelftTools.Utils.Collections.Generic;
using DeltaShell.Plugins.TypologyLookup.DataObjects;
using System.Collections.Generic;
using log4net;
using System;

namespace DeltaShell.Plugins.TypologyLookup.Models
{
    /// <summary>
    /// Provides N and P loss values for different typologies.
    /// </summary>
    public class Lookup : ModelBase
    {
        private TypologyTable _table;
        private LookupKey _typology;
        private LookupValue _n;
        private LookupValue _p;
        private static readonly ILog Log = LogManager.GetLogger(typeof(Lookup));

        public Lookup()
        {
            _table = new TypologyTable();
            _typology = new LookupKey(this, "Typology");
            _n = new LookupValue("N", "Nitrogen", "kg/ha");
            _p = new LookupValue("P", "Phosphorus", "kg/ha");

            DataItems.Add(new DataItem(_table, "Table", typeof(TypologyTable),
                DataItemRole.Input, "TypologyTableTag"));
            DataItems.Add(new DataItem(_typology, "Typology", typeof(LookupKey),
                DataItemRole.Input, "TypologyTag"));
            DataItems.Add(new DataItem(_n, "N", typeof(LookupValue),
                DataItemRole.Output, "NTag"));
            DataItems.Add(new DataItem(_p, "P", typeof(LookupValue),
                DataItemRole.Output, "PTag"));
        }

        public List<string> GetRowLabels()
        {
            return new List<string>(_table.Typologies);
        }


        protected override void OnInitialize()
        {
            Log.Debug("Initializing");
        }


        protected override void OnExecute()
        {
            Log.Debug("Executing");
            this.Status = ActivityStatus.Done;
            Log.Debug("Done");
        }

        public void Update()
        {
            TypologyTableRow row = _table.GetRow(_typology.Value);
            if (row is null)
            {
                // TODO: Handle this
                throw new Exception();
            }
            _n.Value = row.N;
            _p.Value = row.P;
        }
    }
}
