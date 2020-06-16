using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;

namespace ApsimHandlerIOM
{
    /// <summary>Holds the information that can be used to set up an Apsim model within a simulation.</summary>
    /// <remarks>
    /// This assumes that all models of a given type within a simulation will be set up identically, using the same 
    /// structure: identifier for the type of model, followed by a list of parameter names and respective values.
    /// The exception are manager scripts, which have an extra value, identifying their names, besides type and the
    /// parameter settings. Furthermore, it is assumed that models and parameters have only one level each
    /// (i.e. model:parameter, not model:model:parameter:parameter)
    /// </remarks>
    public class ApsimModelSettings
    {
        /// <summary>Identifier for type of the Apsim model to be set up.</summary>
        public string Type;

        /// <summary>Identifier for name of the Apsim model to be set up.</summary>
        /// <remarks>used for manager scripts only.</remarks>
        public string Name;

        /// <summary>List of names of the parameters to be set up.</summary>
        public List<string> ParametersName;

        /// <summary>List of values to assign to the named parameters.</summary>
        public List<string> ParametersValue;
    }
}
