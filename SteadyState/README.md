# Steady-State Models 

Models to be joined in a loosly coupled system to represent steady-state, annualized, or average conditions in a basin.

## Contents

### Sparrow Router

A contaminant transport model that takes input fluxes to stream segments in a drainage network, and accumulates the flux down the network with decay.
Programmed in Python with a C++ wrapper to provide OE BMI compliance.

### Aggregator

A weighted-averaging program for rolling up results from small areas to large ones. Inputs and outputs are in SQLite databases.

### GroundwaterBMI

A generic groundwater model BMI implementation, written in C++, but
extensive calls are made to a Python module named run.py
