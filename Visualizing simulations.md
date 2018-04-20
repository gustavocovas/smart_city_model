Visualizing simulations
===

InterSCity input and output files are compatible with [MATSim](https://www.matsim.org/) formats, so users can take advantage of the extensive tooling already developed by MATSim contributors. The following tutorial shows how to use [OTFVis](https://www.matsim.org/extension/otfvis) for visualization.

### Get MATsim and OTFvis ###
1. Download the standalone zip for the latest stable release from https://www.matsim.org/downloads/. Download the zip for OTFVis from https://github.com/matsim-org/matsim/releases/tag/matsim-0.9.0.

2. Extract:
```
unzip matsim-0.9.0.zip 
unzip otfvis-0.9.0.zip 
```

### Create .mvi file for your scenario ###
1. After running the simulation, create a copy of `events.xml` from your scenario and compress it:
```
cd mock-simulators/smart_city_model/scenario
cp events.xml scenario.events.xml
gzip scenario.events.xml
``` 

2. Use OTFvis to generate a .mvi file from your events and network file:
```
java -cp matsim-0.9.0/matsim-0.9.0.jar:otfvis-0.9.0/otfvis-0.9.0.jar org.matsim.contrib.otfvis.RunOTFVis --convert <path_to_scenario.events.xml.gz> <path_to_network.xml> scenario.mvi <time_resolution_in_seconds>
```

### Visualize the simulation ###
Finally, open the .mvi file with OTFvis:
```
java -cp matsim-0.9.0/matsim-0.9.0.jar:otfvis-0.9.0/otfvis-0.9.0.jar org.matsim.contrib.otfvis.RunOTFVis scenario.mvi
```