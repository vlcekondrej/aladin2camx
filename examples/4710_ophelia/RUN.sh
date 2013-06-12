#!/bin/bash

aladin2camx="/home/linton/work/aladin2camx/aladin2camx/aladin2camx_MAIN.exe"
curdir="/home/linton/work/aladin2camx/aladin2camx/examples/4710_ophelia"

SD=`date --utc --date yesterday +%Y-%m-%d` #"2013-06-06"
SH=1
NH=7
NG=2
GRIBDIR="${curdir}/TMPGRIB"
CAMXDIR="${curdir}/CAMXINP"
NML="${curdir}/INFO_RUN.nml"
if [ ! -d $GRIBDIR ] ; then mkdir $GRIBDIR ; fi
if [ ! -d $CAMXDIR ] ; then mkdir $CAMXDIR ; fi


# Write the info_run namelist
write_info="${curdir}/write_info_run.sh"
$write_info $SD $SH $NH $NG $GRIBDIR $CAMXDIR $NML
echo "$NML written."


# Ensure the gribs are in position
rm -f ${GRIBDIR}/*
i=0
while [ $i -le $NH ] ; do
  rawstamp=`date -u -d "$SD +$[$SH+$i-1]hours" +%Y%m%d`  #the date of the data
  hr=`date -u -d "$SD +$[$SH+$i-1]hours" +%H`            #the hour of the data
  hrint=`date -u -d "$SD +$[$SH+$i-1]hours" +%-H`

  aladrun=`printf "%02d" $[($hrint-1)/6*6]`            #aladin run id (00, 06, 12 or 18)
  step=`printf "%02d" $[$hrint - $aladrun]`        #timestep starting from that run id
  rawgribdir="/mnt/storage1/aq/data/aladin/operativa/current"
  rawgrib="${rawgribdir}/ALAD4camx_${rawstamp}${aladrun}_${step}.grb"
  ln -s ${rawgrib} ${GRIBDIR}/ALAD4camx_${rawstamp}_${hr}.grb
  i=$[$i + 1]
done

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

