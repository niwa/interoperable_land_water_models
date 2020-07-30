using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ApsimWeatherConverter
{
    /// <summary>Structure to hold the data required for an Apsim met file.</summary>
    public class MetData
    {
        /// <summary>Initialise the met data structure.</summary>
        /// <param name="variablesIndex">List of indices of the variables being initialised.</param>
        /// <param name="numData">Length of the data arrays (number of days recorded).</param>
        public MetData(int[] variablesIndex, int numData)
        {
            TimeRecorded = new short[numData];
            DateRecorded = new DateTime[numData];
            TheDateClassic = new string[numData];
            TheDateISO = new string[numData];
            TheYear = new int[numData];
            TheMonth = new int[numData];
            TheDayOfYear = new int[numData];
            foreach (var varIndex in variablesIndex)
            {
                if(varIndex==0)
                    Rain = new float[numData];
                else if (varIndex==1)
                Tmin = new float[numData];
                else if (varIndex == 2)
                    Tmax = new float[numData];
                else if (varIndex == 3)
                    Radn = new float[numData];
                else if (varIndex == 4)
                    Wind = new float[numData];
                else if (varIndex == 5)
                    PET = new float[numData];
                else if (varIndex == 6)
                    VP = new float[numData];
                else if (varIndex == 7)
                    RH = new float[numData];
            }
        }

        /// <summary>Name of the dataset.</summary>
        public string Name;
        /// <summary>Latitude of nominal location for the data (decimal degrees).</summary>
        public float LatitudeNominal;
        /// <summary>Longitude of nominal location for the data (decimal degrees).</summary>
        public float LongitudeNominal;
        /// <summary>Latitude of origin location for the data (decimal degrees).</summary>
        public float LatitudeOrigin;
        /// <summary>Longitude of origin location for the data (decimal degrees).</summary>
        public float LongitudeOrigin;
        /// <summary>Number of days since 31/Dec/1959 for each record.</summary>
        public short[] TimeRecorded;
        /// <summary>Date of each record.</summary>
        public DateTime[] DateRecorded;
        /// <summary>Formatted date of each record (dd/mm/yyyy).</summary>
        public string[] TheDateClassic;
        /// <summary>Formatted date of each record (yyyy-mm-dd).</summary>
        public string[] TheDateISO;
        /// <summary>Year for each record.</summary>
        public int[] TheYear;
        /// <summary>Month for each record.</summary>
        public int[] TheMonth;
        /// <summary>Day of year for each record.</summary>
        public int[] TheDayOfYear;
        /// <summary>Total daily rainfall (mm).</summary>
        public float[] Rain;
        /// <summary>Minimum daily temperature (oC).</summary>
        public float[] Tmin;
        /// <summary>Maximum daily temperature (oC).</summary>
        public float[] Tmax;
        /// <summary>Total Solar radiation (MJ/m2).</summary>
        public float[] Radn;
        /// <summary>Mean daily wind speed (m/s).</summary>
        public float[] Wind;
        /// <summary>Total daily potential evapotranspiration (mm).</summary>
        public float[] PET;
        /// <summary>Mean daily atmospheric vapour pressure (mBar = hPa).</summary>
        public float[] VP;
        /// <summary>Mean daily atmospheric relative humidity (-).</summary>
        public float[] RH;
        /// <summary>Value of the annual mean temperature.</summary>
        public float Tav;
        /// <summary>Value of the amplitude in monthly mean temperature.</summary>
        public float Amp;

        /// <summary>List of variable considered.</summary>
        private string[] variablesList = new string[] { "rain", "mint", "maxt", "radn", "wind", "evap", "vp", "rh" };

        /// <summary>Computes the date-time variables needed for Apsim.</summary>
        public void SetDateRecords()
        {
            for (int i = 0; i < DateRecorded.Length; i++)
            {
                TheDateClassic[i] = DateRecorded[i].ToString("dd/MM/yyyy");
                TheDateISO[i] = DateRecorded[i].ToString("yyyy-MM-dd");
                TheYear[i] = DateRecorded[i].Year;
                TheMonth[i] = DateRecorded[i].Month;
                TheDayOfYear[i] = DateRecorded[i].DayOfYear;
            }
        }

        /// <summary>Checks the temperature and computes the values of tav and amp, as needed by Apsim.</summary>
        /// <remarks>
        /// If Tmax is smaller than Tmin then:
        ///  - Tmin = Tavg - 0.1;
        ///  - Tmax = Tavg + 0.1;
        /// The two parameters calculated are:
        ///  - tav: annual average of daily temperatures;
        ///  - amp: annual amplitude of monthly averages in daily temperature;
        ///  
        /// Assumption: there is data for all months and the quantity is enough to compute the params reliably.
        /// </remarks>
        public void CheckApsimTemperatureParams()
        {
            if (Tmin != null && Tmax != null)
            {
                float Tavg;
                double[] avgTemperature = new double[12];
                int[] dataCount = new int[12];

                // check temperature get the sum of daily average temperature for each month across the entire period available
                for (int t = 0; t < DateRecorded.Length; t++)
                {
                    Tavg = 0.5F * (Tmin[t] + Tmax[t]);
                    if (Tmin[t] >= Tmax[t])
                    {
                        Tmin[t] = Tavg - 0.1F;
                        Tmax[t] = Tavg + 0.1F;
                    }

                    avgTemperature[TheMonth[t] - 1] += Tavg;
                    dataCount[TheMonth[t] - 1] += 1;
                }

                // compute the averages
                for (int m = 0; m < 12; m++)
                    avgTemperature[m] /= dataCount[m];

                // compute the values of tav and amp
                Tav = (float)(avgTemperature.Sum() / 12.0);
                Amp = (float)(avgTemperature.Max() - avgTemperature.Min());
            }
            else
            {
                Tav = float.NaN;
                Amp = float.NaN;
            }
        }

        /// <summary>Checks the values of wind speed, filling values before 1997 with monthly averages.</summary>
        public void CheckApsimWindspeed()
        {
            if (Wind != null)
            {
                float[] avgWindSpeed = new float[12];
                int[] dataCount = new int[12];

                // get the sum and count of wind speed
                for (int t = 0; t < DateRecorded.Length; t++)
                {
                    if (!float.IsNaN(Wind[t]))
                    {
                        avgWindSpeed[TheMonth[t] - 1] += Wind[t];
                        dataCount[TheMonth[t] - 1] += 1;
                    }
                }

                // calculate the averages
                for (int m = 0; m < 12; m++)
                    avgWindSpeed[m] /= dataCount[m];

                // fill in the values only for dates before 01/Jan/1997
                DateTime baseDate = DateTime.Parse("01/Jan/1997");
                for (int t = 0; t < DateRecorded.Length; t++)
                {
                    if (DateRecorded[t].Date < baseDate.Date)
                    {
                        if (float.IsNaN(Wind[t]))
                            Wind[t] = avgWindSpeed[TheMonth[t] - 1];
                    }
                    else
                    {
                        t = DateRecorded.Length;
                    }
                }
            }
            else
            {
                throw new Exception("Cannot check wind speed as no values were given.");
            }
        }

        /// <summary>Gets the reference to one of the weather variables, based on its index.</summary>
        /// <remarks>Follows the order: "Rain", "Tmin", "Tmax", "Radn", "Wind", "PET", "VP", "RH".</remarks>
        /// <param name="varIndex">Index of the variable to be returned.</param>
        /// <returns>The reference to the variable corresponding to the index given.</returns>
        public float[] GetVariable(int varIndex)
        {
            if (varIndex == 0)
                return Rain;
            else if (varIndex == 1)
                return Tmin;
            else if (varIndex == 2)
                return Tmax;
            else if (varIndex == 3)
                return Radn;
            else if (varIndex == 4)
                return Wind;
            else if (varIndex == 5)
                return PET;
            else if (varIndex == 6)
                return VP;
            else if (varIndex == 7)
                return RH;
            else
                return null;
        }

        /// <summary>Gets the reference to one of the weather variables, given its name.</summary>
        /// <remarks>Names should follow the list: "Rain", "Tmin", "Tmax", "Radn", "Wind", "PET", "VP", "RH".</remarks>
        /// <param name="varName">Name of the variable to be returned.</param>
        /// <returns>The reference to the variable corresponding to the name given.</returns>
        public float[] GetVariable(string varName)
        {
            int varIndex = Array.IndexOf(variablesList, varName.ToLower());
            return GetVariable(varIndex);
        }

        /// <summary>Gets the value of a weather variable at a given time, based on their index.</summary>
        /// <remarks>Follows the order: "Rain", "Tmin", "Tmax", "Radn", "Wind", "PET", "VP", "RH".</remarks>
        /// <param name="varIndex">Index of the variable to be returned.</param>
        /// <param name="timeIndex">Index of the time for which the value is required.</param>
        /// <returns>The value of the variable corresponding to the indices given.</returns>
        public float GetValue(int varIndex, int timeIndex)
        {
            if (varIndex == 0)
                return Rain[timeIndex];
            else if (varIndex == 1)
                return Tmin[timeIndex];
            else if (varIndex == 2)
                return Tmax[timeIndex];
            else if (varIndex == 3)
                return Radn[timeIndex];
            else if (varIndex == 4)
                return Wind[timeIndex];
            else if (varIndex == 5)
                return PET[timeIndex];
            else if (varIndex == 6)
                return VP[timeIndex];
            else if (varIndex == 7)
                return RH[timeIndex];
            else
                return float.NaN;
        }

        /// <summary>Gets the value of a weather variable at a given date, given its name.</summary>
        /// <remarks>
        /// Names should follow the list: "Rain", "Tmin", "Tmax", "Radn", "Wind", "PET", "VP", "RH".
        /// Date given in format 'dd/mm/yyyy' or 'yyyy-mm-dd'.
        /// </remarks>
        /// <param name="varName">Name of the variable to be returned.</param>
        /// <param name="timeIndex">Date for which the value is required.</param>
        /// <returns>The value of the named variable at the date given.</returns>
        public float GetValue(string varName, string date)
        {
            int varIndex = Array.IndexOf(variablesList, varName.ToLower());
            int timeIndex = Array.IndexOf(TheDateClassic, date);
            if (timeIndex == -1)
                timeIndex = Array.IndexOf(TheDateISO, date);
            return GetValue(varIndex, timeIndex);
        }
    }
}
