using System;
using System.Collections.Generic;

namespace ApsimWeatherConverter
{
    /// <summary>Contains the latitude and longitude of a location.</summary>
    public class Coordinates
    {
        /// <summary>Location's latitude (decimal degrees).</summary>
        private float myLatitude;
        /// <summary>Location's longitude (decimal degrees).</summary>
        private float myLongitude;

        /// <summary>Initialises the coordinates of a new location.</summary>
        /// <param name="latitude">Location's latitude (decimal degrees).</param>
        /// <param name="longitude">Location's longitude (decimal degrees).</param>
        public Coordinates(float latitude, float longitude)
        {
            if (checkCoordinate(latitude, 90F))
            {
                Latitude = latitude;
            }
            else
            {
                throw new Exception("The value for latitude is outside its bounds (-90 to 90 degrees).");
            }

            if (checkCoordinate(longitude, 180F))
            {
                Longitude = longitude;
            }
            else
            {
                throw new Exception("The value for longitude is outside its bounds (-180 to 180 degrees).");
            }
        }

        /// <summary>Gets or sets the latitude (decimal degrees).</summary>
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

        /// <summary>Gets or sets the latitude (decimal degrees).</summary>
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

    /// <summary>Comparison of locations based on their geographic coordinates.</summary>
    public class LocationComparer : IEqualityComparer<Coordinates>
    {
        /// <summary>Determines whether two locations are equal.</summary>
        /// <param name="location1">Coordinates for the first location.</param>
        /// <param name="location2">Coordinates for the second location.</param>
        /// <returns>True if the locations are equal, false otherwise.</returns>
        public bool Equals(Coordinates location1, Coordinates location2)
        {
            if ((Math.Abs(location1.Latitude - location2.Latitude) < coordinatesTolerance) &&
                (Math.Abs(location1.Longitude - location2.Longitude) < coordinatesTolerance))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>Gets the hash code for a given location.</summary>
        /// <param name="location">Coordinates for a location.</param>
        /// <returns>The hash code for the location given.</returns>
        public int GetHashCode(Coordinates location)
        {
            if (location == null)
            {
                throw new Exception("location given has null coordinates.");
            }
            else
            {
                return (int)(location.Latitude * location.Longitude);
            }
        }

        /// <summary>Minimum value to distinguish coordinates (decimal degrees).</summary>
        private float coordinatesTolerance = 0.001F;
    }
}
