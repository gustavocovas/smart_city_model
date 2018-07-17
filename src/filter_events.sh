#!/bin/bash

for scenario in `ls -d ../digital-rails-scenarios/*/`;
do
  echo Filtering $scenario events
  cat $scenario"events.xml" | grep -E "xml|events|arrival" > $scenario"arrivals.xml"
done

