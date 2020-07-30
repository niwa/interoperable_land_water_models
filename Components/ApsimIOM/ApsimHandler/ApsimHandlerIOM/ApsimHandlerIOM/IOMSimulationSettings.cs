using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;

namespace ApsimHandlerIOM
{
    /// <summary>Holds the information that can be used to set up an Apsim simulation.</summary>
    /// <remarks>
    /// Tailored to work with the OLW Inter-Operable Models project, though it should not be restricted to that.
    /// It stores the ID and basic settings defining a simulation, which in turn are linked to an ecotype.
    /// </remarks>
    public class IOMSimulationSettings
    {
        /// <summary>Identifier for the simulation setup or ecotype.</summary>
        public string ID;

        /// <summary>Name for the simulation setup or ecotype.</summary>
        public string Name;

        /// <summary>Identifier for the slope of ecotype.</summary>
        public string SlopeDescription;

        /// <summary>Identifier for the mean land-use of ecotype.</summary>
        public string LandUseDescription;

        /// <summary>Identifier for the soil order of ecotype.</summary>
        public string SoilOrderDescription;

        /// <summary>Identifier for the SMap soil sibling of ecotype.</summary>
        public string SoilSibling;

        /// <summary>Identifier for the temperature class of ecotype.</summary>
        public string MeanTemperatureClass;

        /// <summary>Identifier for the rainfall class of ecotype.</summary>
        public string MeanRainfallClass;

        /// <summary>Identifier for the latitude of VCSN data of ecotype.</summary>
        public float LatitudeVCSN;

        /// <summary>Identifier for the longitude of VCSN data of ecotype.</summary>
        public float LongitudeVCSN;

        /// <summary>Name of location to be used for setting up the simulation.</summary>
        public string LocationName;

        /// <summary>Name of clock setting to be used for setting up the simulation.</summary>
        public string ClockSettingName;

        /// <summary>Name of farm system to be used for setting up the simulation.</summary>
        public string FarmSystemsName;
    }

    /// <summary>Holds the information that can be used to set up an Apsim simulation.</summary>
    /// <remarks>
    /// Tailored to work with the OLW Inter-Operable Models project, though it should not be restricted to that.
    /// It stores the ID and name of settings defining a simulation, which in turn are linked to an ecotype.
    /// </remarks>
    public class IOMSimulationSetting
    {
        public IOMSimulationSetting(string simName, string siteName, string clockName, string farmSystemName)
        {
            Name = simName;
            LocationName = siteName;
            ClockSettingName = clockName;
            FarmingSystemName = farmSystemName;
        }

        /// <summary>Name for the simulation setup or ecotype.</summary>
        public string Name;

        /// <summary>Name of location to be used for setting up the simulation.</summary>
        public string LocationName;

        /// <summary>Name of clock setting to be used for setting up the simulation.</summary>
        public string ClockSettingName;

        /// <summary>Name of farming system to be used for setting up the simulation.</summary>
        public string FarmingSystemName;
    }
}
