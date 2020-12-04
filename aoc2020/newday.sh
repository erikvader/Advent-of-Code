#!/bin/bash

set -e

sed -i -E '/^ *\/\/<day-marker>$/ i \
    mod day'"$1"';' src/main.rs

mkdir src/day"$1"
cp day_template.rs src/day"$1"/sol.rs
