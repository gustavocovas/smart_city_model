#!/bin/bash

for scenario in `ls -d ~/interscsimulator/scenarios/digital-rails-av-paulista/*/`;
do
  echo Filtering $scenario events
  for r in `seq 0 9`;
  do
    cat $scenario"events"$r".xml" | grep -E "xml|events|arrival" > $scenario"arrivals"$r".xml"
  done
done

