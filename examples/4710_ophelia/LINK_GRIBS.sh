#!/bin/bash

# Inputs
FROMDIR=$1
TODIR=$2
SD=$3 # YYYY-MM-DD
SH=$4 #start hour
NH=$5 #number of hours

# The 0th hour file (hidden from user, used for solar radiation calculation)
f0_YYYYMMDD=`date -u -d "$SD +$[$SH-1]hours" +%Y%m%d`
f0_HH=`date -u -d "$SD +$[$SH-1]hours" +%H`
f0_HHint=`date -u -d "$SD +$[$SH-1]hours" +%-H`
aladrun=`printf "%02d" $[($f0_HHint)/6*6]`
step=`printf "%02d" $[$f0_HHint - $aladrun]`
rawgrib="${FROMDIR}/ALAD4camx_${f0_YYYYMMDD}${aladrun}_${step}.grb"
linkedgrib="${TODIR}/ALAD4camx_${f0_YYYYMMDD}_${f0_HH}.grb"
echo ln -sv $rawgrib $linkedgrib


# Remaining Hours
while [ $i -le $[$NH] ] ; do
  hrint=`date -u -d "$SD +$[$SH+$i-1]hours" +%-H`
  aladrun=`printf "%02d" $[($hrint-1)/6*6]`
  hr=`printf "%02d" $hrint`
  step=`printf "%02d" $[$hrint - $aladrun]`
  rawgrib="${FROMDIR}/ALAD4camx_${f0_YYYYMMDD}${aladrun}_${step}.grb"
  linkedgrib="${TODIR}/ALAD4camx_${f0_YYYYMMDD}_${hr}.grb"
  echo ln -sv $rawgrib $linkedgrib
  i=$[$i+1]


done

