#!/bin/bash

while read -r line; do        ## read line at a time from find output on server
    dname="${line%/*}" 
    lname=$(echo $dname | cut -d'/' -f6-)
    mkdir -p "$lname"  || {
	printf "error: unable to create '%s'.\n", "$lname" >&2
	continue
    }
    echo $line
done < <(ls ~/Documents/projects/bison/assessment/LWR/validation/)
