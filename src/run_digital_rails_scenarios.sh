#!/bin/bash

# Compile InterSCity simulator:
make all

for scenario in `ls -d ../digital-rails-scenarios/*/`;
do
  digital_rails_lanes=`echo $scenario | rev | cut -d '_' -f 3 | rev`
  sed -i "s/DigitalRailsFactor = [0-9]\/3,/DigitalRailsFactor = $digital_rails_lanes\/3,/g" class_Car.erl

  /usr/games/cowsay "Running "$scenario". DR lanes: "$digital_rails_lanes
  echo $scenario"config.xml" > ../interscsimulator.conf

  make smart_city_run CMD_LINE_OPT="--batch"
done

rm -rf Sim-Diasca_Smart_City_Integration_Test-*
