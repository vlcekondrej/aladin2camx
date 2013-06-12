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
ln -sv $rawgrib $linkedgrib


# Remaining Hours
i=1
while [ $i -le $[$NH] ] ; do
  rundt=`date -u -d "$SD +$[$SH+$i-1]hours" +%Y%m%d` #the real date!
  hrint=`date -u -d "$SD +$[$SH+$i-1]hours" +%-H`
  rundtm1=`date -u -d "$SD +$[$SH+$i-2]hours" +%Y%m%d` #one hour less that real date!
  hrintm1=`date -u -d "$SD +$[$SH+$i-2]hours" +%-H`
  runid=`printf "%02d" $[($hrintm1)/6*6]`
  step=`printf "%02d" $[$hrintm1 +1 - $runid]`
  hr=`printf "%02d" $hrint`
  rawgrib="${FROMDIR}/ALAD4camx_${rundtm1}${runid}_${step}.grb"
  linkedgrib="${TODIR}/ALAD4camx_${rundt}_${hr}.grb"
  ln -sv $rawgrib $linkedgrib 
  i=$[$i+1]


done

