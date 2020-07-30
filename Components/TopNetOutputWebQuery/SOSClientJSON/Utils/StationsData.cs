using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SOSClientJSON.Utils
{
    public struct StationData
    {
        public double latitude;
        public double longitude;
        public List<string> capabilities;

        public override string ToString()
        {
            return " ( " + longitude + "," + latitude + " ) " + capabilities.Count + " capabilities";
        }
    }

    public class StationsData
    {
        private Dictionary<string, StationData> _dictionary;

        public StationsData() {
            _dictionary = new Dictionary<string, StationData>();
        }

        
        public void AddNewStation(string name, double lat, double lon, string capability)
        {
            if (!_dictionary.Keys.Contains<string>(name))
            {
                StationData newData = new StationData();
                newData.latitude = lat;
                newData.longitude = lon;
                newData.capabilities = new List<string>();
                newData.capabilities.Add(capability);
                _dictionary.Add(name, newData);
            }
        }

        public void AddCapabilityToExistingStation(string name, string capability)
        {
            if (_dictionary.Keys.Contains<string>(name))
            {
                _dictionary[name].capabilities.Add(capability);

            }
        }

        public bool HasStation(string name)
        {
            return _dictionary.Keys.Contains<string>(name);
        }

        public override string ToString()
        {
            string result = "Stations Data: ";
            foreach (var key in _dictionary.Keys)
            {
                result += " " + key + ": "  + _dictionary[key].ToString() + ";";
            }
            return result;
        }

    }

}
