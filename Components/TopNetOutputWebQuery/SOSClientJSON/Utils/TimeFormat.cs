using System;

namespace SOSClientJSON.Utils
{
    /// <summary>
    /// Utils to create the expected time formats for the SOS request
    /// </summary>
    public class TimeFormat
    {
        /// <summary>
        /// Create a standard ISO format to perform a query to SOS. Note that we need exactly three zeros for the milliseconds
        /// Otherwise, the server will error.
        /// </summary>
        /// <param name="year">Selected time year</param>
        /// <param name="month">Selected time month</param>
        /// <param name="day">Selected time day</param>
        /// <param name="hour">Selected time hour (optional, defaults to 0)</param>
        /// <param name="minutes">Selected time minutes (optional, defaults to 0)</param>
        /// <param name="seconds">Selected time seconds (optional, defaults to 0)</param>
        /// <returns></returns>
        public static string GetTimeFormatForQuery(int year, int month, int day, int hour=0, int minutes=0, int seconds=0)
        {
            DateTime date = new DateTime(year, month, day, hour, minutes, seconds, DateTimeKind.Local);
            return date.ToString("yyyy-MM-ddTHH:mm:ss.fffK");
        }
    }
}
