import rainfall_runoff as rr

### Initialise variables ###

AE = PE # If volume of water in soil is greater than not_RAW then AE = PE; otherwise code later on will adjust AE.
not_RAW = 0.5 * pars.field_capacity # not_RAW is 'not readily available water' and is equivalent to 0.5 of field capacity
vol_soil = [pars.vol_soil] # volume of water in soil

effective_rain = []
infiltration_flux = []
overland_flow = []
drainage = []
mod_flow = []

# Loop through each time step
for t in range(0, len(time_series)):

	(mod_flow[t], vol_soil[t],
	 vol_fastflow[t], vol_interflow[t], vol_baseflow[t],
	 overland_flow[t], AE[t]) = rr.rainfall_runoff(t, rain, irri, PE, gw_abstraction, pars)

	
