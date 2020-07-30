using System;


namespace SOSClientJSON
{
    class Program
    {
        static void Main(string[] args)
        {
            JSONClient client = new JSONClient("http://localhost:8080/52n-sos-webapp/service");
            var startTime = Utils.TimeFormat.GetTimeFormatForQuery(2012, 3, 1);
            var endTime = Utils.TimeFormat.GetTimeFormatForQuery(2012, 3, 10);
            
            var property = "Discharge"; //
            var station = "15440550"; // ID of the station
            var result = client.PerformTimeSeriesRequest(property, station, startTime, endTime);
            Console.WriteLine(result);
            Console.ReadKey();
            
        }
    }
}
