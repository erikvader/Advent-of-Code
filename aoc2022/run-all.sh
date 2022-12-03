#!/bin/bash

set -e

[[ -f ./aoc.core ]] || sbcl --load init.lisp --eval '(sb-ext:save-lisp-and-die "./aoc.core")'
while IFS='' read -r line; do
    (
        cd "$(dirname "$line")" || exit 1
        echo "Running $line"
        sbcl --core ../../aoc.core --script ./solution.lisp
        echo
    )
done < <(find . -type f -name solution.lisp | sort -V)

rm ./aoc.core
