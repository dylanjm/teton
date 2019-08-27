#!/bin/bash

server=${1:-mcdodyla@falcon1}
basedir=${2:-/projects/bison/git/CASL19_robustness/timestepping_newparams/run2/assessment/LWR}

while read -r line; do        ## read line at a time from find output on server
    dname="${line%/*}" 
    lname=$(echo $dname | cut -d'/' -f6-)
    mkdir -p "$lname"  || {
	printf "error: unable to create '%s'.\n", "$lname" >&2
	continue
    }
    echo $line
    rsync -uavq "$server:$line" "$lname" ## rsync file to correct directory
done < <(ssh -qn "$server" "find $basedir -type f -name '*_metrics.csv' ! -path './*/doc/*' ! -path '*/gold/*' 2>/dev/null")

