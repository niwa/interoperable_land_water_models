# Interoperable Models Programme - APSIM simulations' handler  

### Aim: 

>**The programme aims to develop a modelling system, populated with existing models and drawing on national datasets, implemented in an interoperable modelling framework.**  
>The task for PFR is to develop the methodology and tools that can be used to modify and run APSIM simulations in order to supply the interoperable framework with estimates of yield, water usage, and N losses from cropping farms.  The farm system is set up by a given crop rotation, management, and environmental parameters.  
>In addition, PFR will liaise with AgResearch to include pastoral systems can be run in tandem and using the same tools as cropping systems.  

### Includes:  
1. Library of base farm systems simulations:  
> These consist of simulations describing generic archetypical farming systems, may have variations of a system describing different crop rotations, irrigation type, etc.;
2. Land-use interface:  
> It is the tool that manages the input parameters passed by the interoperable framework, decides which base simulations will be used, and pass these to the model handler.  
3. APSIM Handler component:  
> This is the tool that can modify and run a base simulation, changing the soil type, weather file, and a few management parameters to produce more simulations more specific to a given situation/condition.  

### Notes:
> The 'land-use interface' is used here as a concept and is not being developed by PFR. The Interoperable Framework machinery incorporates this as a component or as a series of procedures...

#### Team:  
Rogerio Cichota, Edith Khaembah, Iris Vogeler, Rob Zyskowski (PFR); Mark Lieffering, Ronald Vibart (AgResearch)  


#### Contact:  
Rogerio Cichota - APSIM and APSIMHandler
Sandy Elliot (NIWA) - Interoperable Framework