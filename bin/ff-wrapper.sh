#!/bin/bash

domain="../planner-haskell/resources/shrdlu-dom.pddl"
problem_file=$1

ff_out=$(../bin/ff -o $domain -f $problem_file)

case $ff_out in
    *"goal can be simplified to FALSE"*)
        echo "Goal can be simplifiead to FALSE!"
        exit 1
        ;;
esac

actions=$(echo -e "$ff_out" | egrep "((PICK|DROP)-(IN|ON))")

# Todo error handling
echo "$actions" | cut -b 12- | sed -re 's/^(PICK|DROP).*F(\w*)$/\L\1 \2/'
