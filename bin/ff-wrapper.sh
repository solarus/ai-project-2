#!/bin/bash

domain="../planner-haskell/resources/shrdlu-dom.pddl"
problem_file=$1

ff_out=$(../bin/ff -o $domain -f $problem_file)


actions=$(echo -e "$ff_out" | egrep "((PICK|DROP)-(IN|ON))")

# Todo error handling
echo "$actions" | cut -b 12- | sed -re 's/^(PICK|DROP).*F(\w*)$/\L\1 \2/'
