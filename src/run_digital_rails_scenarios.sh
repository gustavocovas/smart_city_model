#!/bin/bash

make all

for scenario in `ls -d ../digital-rails-scenarios/*/`;
do
  echo Running $scenario
  echo $scenario"config.xml" > ../interscsimulator.conf
  make smart_city_run CMD_LINE_OPT="--batch"
done
