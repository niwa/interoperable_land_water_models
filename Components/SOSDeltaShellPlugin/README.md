# SosService
Plugin to query the NIWA SOS Webservice

## Requirements
You will need to get the JSONClient project first.

## Building

- Open the solution
- Add the JSONClient as an existing project
- You will need to fix a couple of things here: namely adding System.Json so that it compiles
- Add the JSONClient as external reference from DeltaShell.Plugins.SosService
- Fix broken references with local ones if needed. You will need to open the actual .sln file and correct the path to the JSonClient.
- Build and execute on DeltaShell as usual

## Using it

You add a new model of the type SOS Service. You can change the property, station and dates where you request the time series.
If you change something and you re-execute, you will preserve older results.

## TODO

- Add a list of stations as input
- Add a map to the output
- Make it more robust
