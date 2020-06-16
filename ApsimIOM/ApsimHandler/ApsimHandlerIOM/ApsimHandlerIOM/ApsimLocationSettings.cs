using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using Newtonsoft.Json.Linq;

namespace ApsimHandlerIOM
{
    /// <summary>Holds the information about a given location to be used in an Apsim simulation.</summary>
    /// <remarks>The information encompasses references to weather and soil data, plus the geographic coordinates.</remarks>
    class ApsimLocationSettings
    {
        /// <summary>Name identifier for the location.</summary>
        public string Name;

        /// <summary>Latitude of location (decimal degrees).</summary>
        public float Latitude
        {
            get
            {
                return myLatitude;
            }
            set
            {
                if (checkCoordinate(value, 90F))
                {
                    myLatitude = value;
                }
                else
                {
                    throw new Exception("The value for latitude is outside its bounds (-90 to 90 degrees).");
                }
            }
        }

        /// <summary>Longitude of location (decimal degrees).</summary>
        public float Longitude
        {
            get
            {
                return myLongitude;
            }
            set
            {
                if (checkCoordinate(value, 180F))
                {
                    myLongitude = value;
                }
                else
                {
                    throw new Exception("The value for longitude is outside its bounds (-180 to 180 degrees).");
                }
            }
        }

        /// <summary>Name of file with the weather data for the location.</summary>
        public string MetFileName = null;

        /// <summary>Name of soil for the location.</summary>
        public string SoilName = "";

        /// <summary>Location's latitude (decimal degrees).</summary>
        private float myLatitude;
        /// <summary>Location's longitude (decimal degrees).</summary>
        private float myLongitude;

        /// <summary>Checks that a given coordinate is within its bounds.</summary>
        /// <param name="coordinateValue">The value of the coordinate being tested.</param>
        /// <param name="coordinateBound">The value of the bounds for the coordinate.</param>
        /// <remarks>Assumes that the bounds have the same magnitude, but are positive and negative.</remarks>
        /// <returns>True if the value is within bounds, false otherwise.</returns>
        private bool checkCoordinate(float coordinateValue, float coordinateBound)
        {
            return Math.Abs(coordinateValue) <= Math.Abs(coordinateBound);
        }
    }
}
