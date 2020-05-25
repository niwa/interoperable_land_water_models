from Libraries.StandardFunctions import *
from Libraries.ChartFunctions import *

# Create 4 types of chart series
lineSeries = CreateLineSeries([[1,3],[2,5],[3,4],[4,1],[5,3],[6, 4]])
barSeries = CreateBarSeries([[1, 0.3],[2, 0.5],[3, 0.4],[4, 0.1],[5, 0.3]])
pointSeries = CreatePointSeries([[1, 4],[2,3],[3,1],[4,4],[5,5],[6, 3]])
areaSeries = CreateAreaSeries([[1, 2],[2, 3.8],[3, 4.6],[4,2.4],[5,1.2],[6, 2]])

# Configure the bar series
barSeries.Color = Color.LightBlue
barSeries.LineVisible = True
barSeries.LineColor = Color.BlueViolet
barSeries.LineWidth = 3

# Configure the line series
lineSeries.Color = Color.Red
lineSeries.Width = 3
lineSeries.PointerVisible = True
lineSeries.PointerSize = 5
lineSeries.PointerColor = Color.Red
lineSeries.PointerLineVisible = True
lineSeries.PointerLineColor = Color.DarkRed
lineSeries.Transparency = 50

# Configure the point series
pointSeries.Color = Color.DarkGreen
pointSeries.LineVisible = True
pointSeries.LineColor = Color.Red
pointSeries.Size = 5 

# Configure the area series
areaSeries.Color = Color.Yellow
areaSeries.Transparency = 50 # %
areaSeries.LineColor = Color.Green
areaSeries.LineWidth = 2
areaSeries.PointerVisible = True
areaSeries.PointerSize = 3
areaSeries.PointerColor = Color.Red
areaSeries.PointerLineVisible = False

chart = CreateChart([areaSeries, lineSeries, barSeries, pointSeries])

# Configure the chart
chart.TitleVisible = True
chart.Title = "Test chart"
chart.BackGroundColor = Color.White
chart.Legend.Visible = True

# Configure the bottom axis
chart.BottomAxis.Automatic = False
chart.BottomAxis.Minimum = 1
chart.BottomAxis.Maximum = 6
chart.BottomAxis.Title = "index"

# Configure the left axis
chart.LeftAxis.Title = "Value"

# Show the chart
OpenView(chart)

# Export the chart as an image (width:1000 height: 1000)
chart.ExportAsImage("D:\\testImage.jpg", 1000,1000)