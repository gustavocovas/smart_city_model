#!/bin/bash

# Compile InterSCity simulator:
make all

for scenario in `ls -d ../digital-rails-scenarios/*/`;
do
  /usr/games/cowsay "Running "$scenario"..."
  echo $scenario"config.xml" > ../interscsimulator.conf

  digital_rails_lanes=`echo $scenario | rev | cut -d '_' -f 2 | rev`
  sed -i "s/DigitalRailsFactor = [0-9]\/3,/DigitalRailsFactor = $digital_rails_lanes\/3,/g" class_Car.erl

  make smart_city_run CMD_LINE_OPT="--batch"
done

rm -rf Sim-Diasca_Smart_City_Integration_Test-*
