#!/bin/bash

# Make symbolic links for aladin2camx


# Args:
#      1  directory where the raw gribs are
#      2  directory where to put the links
#      3 (optional) the date as YYYY-MM-DD (default is today)
#      4 (optional) the first hour (0-23, UTC, default is 0)
#      5 (optional) the  number of hours (default is 49)
#-------------------------------------------------------------


# Inputs
today=`date -u -d "today" +%Y-%m-%d`
FROMDIR=${1:-"."}
TODIR=${2:-"."}
SD=${3:-$today} # YYYY-MM-DD
SH=${4:-0} #start hour
NH=${5:-49} #number of hours


# Functions
source ./FILE_FROM_DATE.sh


#CHeck inputs
echo $FROMDIR $TODIR $SD $SH $NH


# Symbolic links
EH=$(($SH+$NH)) # end hour
echo $EH
for h in $(seq $SH $EH); do
  grb=$(aladinGribName_00fc $SD $h)
  a2c=$(aladin2camxGribName $SD $h)
  ln -v -s "${FROMDIR}/${grb}" "${TODIR}/${a2c}"
done


