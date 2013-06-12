#!/bin/bash

# Clean up the log files and reset the aladin2camx counter
# Use flag -f to avoid prompting

PROMPT="-i"  # ask at each delete

# if -f given
if [ "$1" == "-f" ]; then
    PROMPT=""
fi

rm -v $PROMPT ./aladin2camx_*.log*
rm -v $PROMPT ./.aladin2camx.counter



