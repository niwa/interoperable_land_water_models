# Typology Lookup Plugin

This simple plugin exposes N and P losses for various Typologies.

## Concept

We want our Lookup model to have two input items:

1. The data table used to perform lookups
2. The typology to retrieve values for

and two output items:

1. The N loss value
2. The P loss value

After loading a data table (e.g. from a csv file), we should be able to set the input typology to the desired value and read the corresponding values from the N and P output items.