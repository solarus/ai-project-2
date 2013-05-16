#!/bin/bash

domain="../planner-haskell/resources/shrdlu-dom.pddl"
problem_file=$1

PATH=$PATH:/usr/local/bin/

ff_out=$(../bin/ff -o $domain -f $problem_file)

if [[ $? != 0 ]] ; then
    echo "Some error occured. Output from ff:"
    echo ""
    echo "$ff_out"
    exit 1
fi

actions=$(echo -e "$ff_out" | egrep "((PICK|DROP)-(IN|ON))")

# Todo error handling
# fix for BSD, use 'gsed' if present...
sed_bin="sed"
hash gsed 2> /dev/null && sed_bin="gsed"

echo "$actions" | cut -b 12- | $sed_bin -re 's/^(PICK|DROP).*F(\w*)$/\L\1 \2/'
