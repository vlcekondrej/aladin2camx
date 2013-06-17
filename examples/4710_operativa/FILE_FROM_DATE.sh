#!/bin/bash
# source "this file"
# then use the functions


aladinGribName_00fc () {
    # Use this if you want files from the 00UTC forecast run.
    # note,
    # for hours -1 and 0, we have no choice but to 
    # use the previous forecast's fields.
    date=$1 # YYYY-MM-DD format
    hour=$2 # integer value from 00UTC, can be 48 if you want that hour's forecast.
    case $hour in
      "-1") 
           grbdt=`date -u -d "$date -1hour" +%Y%m%d`
	   fname="ALAD4camx_${grbdt}18_05.grb"
	   ;;
       "0")  
           grbdt=`date -u -d "$date -1hour" +%Y%m%d` 
           fname="ALAD4camx_${grbdt}18_06.grb"
           ;;
       *)
           grbdt=`date -u -d "$date" +%Y%m%d`
    	   step=`printf "%02d" $hour`
           fname="ALAD4camx_${grbdt}00_${step}.grb"
           ;;
    esac
    echo "${fname}"
}

aladin2camxGribName () {
    # Makes the file name as recognised by aladin2camx
    date=$1 # YYYY-MM-DD format
    hour=$2 # integer value from 00UTC
    dt=`date -u -d "$date +${hour}hours" +%Y%m%d_%H`
    echo "ALAD4camx_${dt}.grb"
}


aladinGribName_cont () {
    # Use this if you want only the most recent forecasts
    # Continuous use of updated 6hrly forecasts
    # $1 # YYYY-MM-DD format
    # $2 # integer value from 00UTC     
    date=`date -u -d "$1 +${2}hours" +%Y-%m-%d`
    hour=`date -u -d "$1 +${2}hours" +%-H`
    step=$[${hour}%6] 
    if [ $step -eq 0 ] ; then
      prevdt=`date -u -d "$date +$[${hour}-1]hours" +%Y%m%d`
      prevhr=`date -u -d "$date +$[${hour}-1]hours" +%-H`
      aladrun=`printf "%02d" $[(${prevhr})/6*6]`
      aladstep=`printf "%02d" $[$prevhr - $aladrun +1]`
      fname=`printf "ALAD4camx_%8d%02d_%02d.grb" ${prevdt} $aladrun $aladstep`
    else
      dt=`date -u -d "$date +${hour}hours" +%Y%m%d`
      run=`printf "%02d" $[($hour)/6*6]`
      fname=`printf "ALAD4camx_%8d%02d_%02d.grb" $dt $run $step`
    fi
    echo $fname
}

#camx file names not yet implemented.


#for testing
wasSourced () {
  local -i rc=0
  [[ 'source' == ${FUNCNAME[1]} ]] && rc=1
  return $rc
}

echo $((wasSourced))

if [[ "$(wasSourced)" == "0" ]] ; then
  echo not sourced
 
fi
