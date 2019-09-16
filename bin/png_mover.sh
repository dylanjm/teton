#!/bin/bash


while read -r line; do        ## read line at a time from find output on server
    dname=$(dirname $line |
		xargs dirname |
		xargs dirname |
		xargs basename)
    newplace="/Users/mcdodj/OneDrive - Idaho National Laboratory/assessment plot update/updated_plots/$dname/"
    oldplace="/Users/mcdodj/Documents/projects/bison/$line"

    cp "$oldplace" "$newplace"
done < <(cd ~/Documents/projects/bison; git diff --cached --diff-filter=MA --name-only -- '*.png')
