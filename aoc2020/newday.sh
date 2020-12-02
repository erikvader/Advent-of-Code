#!/bin/bash

set -e

sed -i -E '/^ *\/\/<day-marker>$/ i \
    mod day'"$1"';' src/main.rs

mkdir src/day"$1"
cp src/day0/sol.rs src/day"$1"
