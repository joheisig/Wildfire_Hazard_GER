## Fire Modeling with RandIG

RandIG is a command line tool that implements fire spread simulation in the same way as the more commonly used [FlamMap 6](https://research.fs.usda.gov/firelab/projects/flammap) software with the difference that it is not openly available. It may be obtained upon request from the Missoula Fire Sciences Laboratory. 

Our wildfire hazard study was computed in chunks to optimize performance. Germany's landscape was split into 195 chunks, each with a size of about 50 x 50 km and a spatial resolution of 100 m. Several thousand fires (depending on the proportion of burnable landscape types) were simulated for 9 wind speed / fuel moisture scenarios and 4 cardinal wind directions. 

## Sample Analysis
This directory contains inputs and outputs for one sample landscape chunk located south of Berlin in the northeastern fire occurrence area. Input data and parameter can technically be used with FlamMap 6 to mimic the simulation runs from our study and obtain the same results.

### Inputs
- `LCP_NE_16.tif` stores the relevant landscape variables related to terrain (elevation, slope, aspect) and vegetation fuels (surface fuel model, canopy cover, -height, -base height, -bulk density). 
- `D1L1_97_0_NE_16_randig.input` is an input file, which RandIG requires, and lists all simulation parameters including wind and moisture settings, number of ignitions, and desired outputs.

### Outputs
- `_RandigOutputs.tif` stores conditional burn probability and conditional flame length probabilities, which represent the main outputs. They need to be post-processed to obtain integrated wildfire hazard using a classification scheme.
- `_FireSizeList.txt` lists all ignition locations and the corresponding final fire perimeter size.
