#!/bin/bash
RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
dry_run=0
yesterday=1

# Establish -n flag means to do a dry run.
while getopts "ny:f:" opt; do
    case $opt in
	n) dry_run=1 ;;
	y) yesterday=${OPTARG} ;;
	f) folder=${OPTARG} ;;
	*) echo 'error in command line parsing' >&2
	   exit 1
    esac
done

# Grab the most recently updated bison file off of Falcon1; return as string
# in the form of bison_XXXXXXXX/assessment/LWR/validation/
NEWEST_BISON_RUN=$(
    ssh -qn mcdodyla@falcon1 'find /projects/bison/git/* -mindepth 0 -maxdepth 0 -type d -printf "%T@\t%f\n"' |
	sort -t$'\t' -r -nk1,2 |
	sed -n "$yesterday"p |
	cut -f2- |
	awk '{print "/projects/bison/git/"$1"/assessment/LWR/validation/"}'
		)

# Concat with folder of interest passed in from CLI
NEW_FOLDER="$NEWEST_BISON_RUN$folder/*"

# Take $newest and find all associated *_out.csv files beneath that directory
NEW_RFILEP=($(ssh -qn mcdodyla@falcon1 \
		  "find $NEW_FOLDER -type f -name '*_out.csv' -not -path '*/doc/*' 2>/dev/null"))

# Maninpulate file paths to match the local machine directory
LOCAL_FILEP=($(for i in "${NEW_RFILEP[@]}"; do
		   echo $i |
		       sed -E "s#/projects/bison/git/bison_[0-9]{8}#/Users/mcdodj/Documents/projects/bison#"
	       done
	     ))

# Check to see if dry-run, if not proceed with copying the files over. 
if [ "$dry_run" -eq 1 ]; then
    printf "${YELLOW}Initiating Dry-Run (${yesterday} Days Old):${NC}\n"
    for ((i=0; i<${#NEW_RFILEP[@]}; i++)); do
	printf "${RED}Remote File ->${NC} ${NEW_RFILEP[i]}\n"
	printf "${GREEN}Local File ->${NC} ${LOCAL_FILEP[i]}\n"
	printf "\n"
    done
else
    printf "${YELLOW}Fetching Data from Remote Server...${NC}\n"
    for ((i=0; i<${#NEW_RFILEP[@]}; i++)) do
	scp -qp mcdodyla@falcon1:"${NEW_RFILEP[i]}" "${LOCAL_FILEP[i]}"
	if [ ${PIPESTATUS[0]} -eq 0 ]; then
            printf "${GREEN}File Created/Updated at:${NC} ${LOCAL_FILEP[i]}\n"
	else
            printf "${RED}Error Fetching File:${NC} ${NEW_RFILEP[i]}\n"
	fi
    done
	printf "${YELLOW}Bison Remote Fetch Complete!${NC}\n"
    fi
									 
