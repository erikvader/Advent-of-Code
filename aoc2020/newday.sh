#!/bin/bash

set -e

# add new module to main.rs
sed -i -E '/^ *\/\/<day-marker>$/ i \
    mod day'"$1"';' src/main.rs

# create the module file and folder
mkdir src/day"$1"
cp day_template.rs src/day"$1"/sol.rs

# copy the problem input from the clipboard to an input file
xclip -selection clipboard -o > src/day"$1"/input
