# Components  

This directory contains the main components as well as utilities.

## Contents

### Sparrow Router

A contaminant transport model that takes input fluxes to stream segments in a drainage network, and accumulates the flux down the network with decay.
Programmed in Python with a C++ wrapper to provide OE BMI compliance.

### Aggregator

A weighted-averaging program for rolling up results from small areas to large ones. Inputs and outputs are in SQLite databases.

### GroundwaterBMI

A generic groundwater model BMI implementation, written in C++, but
extensive calls are made to a Python module named run.py

### APSIM loss


### TopNet stream routing

Kinematic wave lagrangian routing for the interoperable models. It is based on the TopNet routing.

### TopNet Output Web Query

Library to query a SOS webservice that contains results of TopNet simulations.

### TopNet Output Delta Shell Plugin

Plugin that uses the TopNet Output Web Query to plot time-series extracted on a SOS webserver within
DeltaShell.

### LUCI 

The LUCI model is a rainfall-runoff model used for surface water calculations on the interoperable models.




