#!/bin/bash

#Basic parameters
aladin2camx="/home/linton/work/aladin2camx/aladin2camx/aladin2camx_MAIN.exe"
curdir="/home/linton/work/aladin2camx/aladin2camx/examples/4710_ophelia"


# Input parameters
SD=`date --utc --date yesterday +%Y-%m-%d` #"2013-06-06"
#SD="2013-06-06" # start date
SH=12 # start hour, must point to a startStep=1 file. We automatically use startStep=0 for file 0.
NH=6 # number of hours of data in outputs
NG=2 # number of grid domains
GRIBDIR="${curdir}/TMPGRIB"  # where the gribs will be found
CAMXDIR="${curdir}/CAMXINP"  # where the camx inputs will be put
NML="${curdir}/INFO_RUN.nml" # name of the namelist to be created
if [ ! -d $GRIBDIR ] ; then mkdir $GRIBDIR ; fi
if [ ! -d $CAMXDIR ] ; then mkdir $CAMXDIR ; fi


# Write the info_run namelist
write_info="${curdir}/WRITE_INFO_RUN.sh"
$write_info $SD $SH $NH $NG $GRIBDIR $CAMXDIR $NML
echo "$NML written."


# Ensure the gribs are in position
rm -f ${GRIBDIR}/*
link_gribs="${curdir}/LINK_GRIBS.sh"
rawgribdir="/mnt/storage1/aq/data/aladin/operativa/current"
$link_gribs $rawgribdir $GRIBDIR $SD $SH $NH
 

# Ensure the other namelists are in position
nmldir=`dirname "$NML"`
if [ ! -f ${nmldir}/INFO_ALADIN_GRIBS.nml ] ; then 
  echo "${nmldir}/INFO_ALADIN_GRIBS.nml not found!"
  exit 1
fi
if [ ! -f ${nmldir}/INFO_CAMx_GRID.nml ] ; then 
  echo "${nmldir}/INFO_CAMx_GRID.nml not found!"
  exit 1
fi


# Run aladin2camx
$aladin2camx

