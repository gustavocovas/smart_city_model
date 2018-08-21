#!/bin/bash

target_scenario=$1

# Compile InterSCity simulator:
make all

for scenario in `ls -d ~/interscsimulator/scenarios/digital-rails-av-paulista/$target_scenario/`;
do
  digital_rails_lanes=`echo $scenario | rev | cut -d '_' -f 2 | rev`
  sed -i "s/DigitalRailsFactor = [0-9]\/3,/DigitalRailsFactor = $digital_rails_lanes\/3,/g" class_Car.erl

  for r in `seq 0 9`;
  do
    /usr/games/cowsay "Running "$scenario", round "$r". DR lanes: "$digital_rails_lanes
    echo $scenario"config"$r".xml" > ../interscsimulator.conf

    make smart_city_run CMD_LINE_OPT="--batch"
  done

  notify-send "Done "$target_scenario
done

# Remove all Sim-Diasca traces (lol): 
rm -rf Sim-Diasca_Smart_City_Integration_Test-*
