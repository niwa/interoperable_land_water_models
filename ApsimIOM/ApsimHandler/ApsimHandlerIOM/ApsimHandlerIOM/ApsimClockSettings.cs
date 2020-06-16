using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;

namespace ApsimHandlerIOM
{
    /// <summary>Holds the information to set up Apsim clock, defining the start and/or end of a simulation.</summary>
    class ApsimClockSettings
    {
        /// <summary>Name identifying a clock settings.</summary>
        public string Name = "?";

        /// <summary>Date to start an Apsim simulation (yyyy-mm-dd).</summary>
        public string StartDate = "";

        /// <summary>Date to end an Apsim simulation (yyyy-mm-dd).</summary>
        public string EndDate = "";

        /// <summary>Number of days between start and end dates.</summary>
        public int Duration
        {
            get
            {
                int span = -1;
                if ((StartDate.Length > 7) && (EndDate.Length > 7))
                {
                    DateTime iniDate = DateTime.Parse(StartDate);
                    DateTime finDate = DateTime.Parse(EndDate);
                    span = (finDate - iniDate).Days + 1;
                    span = Math.Max(0, span); // returns 0 if endDate is prior to startDate
                }

                return span;
            }
        }

        /// <summary>List of clock parameters' names.</summary>
        public List<string> ParametersName = new List<string> { "StartDate", "EndDate" };

        /// <summary>List of clock parameters' values.</summary>
        public List<string> ParametersValue
        {
            get
            {
                List<string> myParameters = new List<string> { StartDate, EndDate };
                return myParameters;
            }
        }
    }
}
