#!/bin/bash

target_scenario=random_walk_dr_algo_*

# Compile InterSCity simulator:
make all

for scenario in `ls -d ~/interscsimulator/scenarios/digital-rails-av-paulista/$target_scenario/`;
do
  /usr/games/cowsay "Running "$scenario
  echo $scenario"config.xml" > ../interscsimulator.conf
  make smart_city_run CMD_LINE_OPT="--batch"
  # notify-send "Done "$target_scenario
done

# Remove all Sim-Diasca traces (lol): 
rm -rf Sim-Diasca_Smart_City_Integration_Test-*
