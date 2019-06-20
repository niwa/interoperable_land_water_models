""" 
Sparrow router. Takes input fluxes to stream segments in a drainage network, and accumulates the flux down the network with decay
The inputs are read in from a sql database containing segment information, as described in a configuration file, and outputs are placed
into a further sql database. A BMI interface is provided for running the model, but input and output are file-based rather than through memory mapping.
Sandy Elliott 2019
"""
#region Imports and settings 
import os
import sys
import numpy as np
import pandas as pd
import yaml  # for reading in parameter files
from inspect import getsourcefile
import time
import sqlite3
import math
from math import exp

pd.set_option('display.width', 150)
#pylint:disable=E1101     
# above disables the pylint no member error, as it was coming up inappropriately

#endregion
#************************************************************
#region           Initialise          
#************************************************************

# Read control and parameter files
ProjectDir=''

# Read parameters as yaml file
with open(ProjectDir + 'Parameters.yaml', 'r') as f:
    Parameters = yaml.load(f)

# Obtain initial input variables for segments


#From sqlite. (Note that the affinity types of the columns must be specified in the database, otherwise general 'object' dtypes are generated leading to later errors working with the
# variables, requiring specific type coercion later).
conn = sqlite3.connect("Sparrow.db")
Segments = pd.read_sql_query("select * from Segments;", conn)
conn.close()
#From CSV
#Segments = pd.read_csv("Segments.csv")


# Validate inputs (not implemented yet)

# sort by hydseq, first setting up the original sort order from row id
Segments['OriginalOrder'] = range(len(Segments))
Segments.sort_values('Hydseq',inplace=True)


#endregion
#************************************************************
#region             Run calculations          
#************************************************************

nsegments = len(Segments)

#convert dataframe columns to vectors for efficient calculations
FromNodeVect = Segments.FromNode.values
ToNodeVect = Segments.ToNode.values

# Generate dictionary of Nodes. key is node identifier, value is load
NodeLoad = {}
# build up dictionary based on the from and to nodes, with intial value of 0
for isegment in range(nsegments):
    fromnode = FromNodeVect[isegment]
    if not(fromnode in NodeLoad):
        NodeLoad[fromnode] = 0. 
    tonode = ToNodeVect[isegment]
    if not(tonode in NodeLoad):
        NodeLoad[tonode] = 0. 

Segments['SourceTotal'] = Segments.SourceDirect + Segments.SourceGroundwater + Segments.SourcePoint

# Calculate decay fraction
def StreamDecay(a,b,Length,Flow):
    Flow = np.maximum(Flow,0.001)
    DecayCoefficient = a * Flow**b
    StreamCarry = np.exp(-DecayCoefficient*Length)
    return StreamCarry
#** pretty ugly using the np math functions above to enable vectorised calculations. But the function does work OK with scalars too, so no harm. Could use Numba to create own universal function ufunc with a scalar-based function


Segments['StreamCarry'] = StreamDecay(Parameters['StreamDecayCoefficient'],Parameters['StreamDecayExponent'],Segments.Length.values,Segments.Flow.values)

# Route the loads

SourceTotalVect = Segments.SourceTotal.values  
StreamCarryVect = Segments.StreamCarry.values
LoadVec = np.zeros(nsegments)

for segment in range(nsegments):
    streamcarry = StreamCarryVect[segment]
    load = NodeLoad[FromNodeVect[segment]] * streamcarry + SourceTotalVect[segment] * streamcarry**0.5
    tonode = ToNodeVect[segment]
    NodeLoad[tonode] += load # Add the segment outlet load to the node load.
    LoadVec[segment] = load

Segments['SegmentLoad'] = LoadVec
#endregion
#************************************************************
#region                 Finalise          
#************************************************************

# Output the variables

Segments.sort_values('OriginalOrder',inplace=True)

# check that the sqlite can be opened in write mode (not implemented yet)


# write the table

conn = sqlite3.connect('Sparrow.db')

SegmentLoad = Segments[['SegmentID','SegmentLoad']]
conn.execute('DROP TABLE if exists SegmentLoad')
SegmentLoad.to_sql('SegmentLoad', conn) 

conn.execute('DROP TABLE if exists SegmentsNew')
conn.execute('Create table SegmentsNew as Select Segments.*, SegmentLoad.SegmentLoad from Segments LEFT JOIN SegmentLoad Using(SegmentID)')

conn.close()

#endregion

