#!/bin/bash

#echo -e "\n*** startuji aladin2camx ..."

#      # neprepisuji jiz existujici, protoze v nich mohou byt nastaveny parametry pro konkretni beh
#      [ -f aladin2camx_control.nml ] || cp $SRCDIR/cases/$AQ_RUN_NAME/namelists/aladin2camx/aladin2camx_control.nml .
#      [ -f INFO_ALADIN_GRIBS.nml   ] || cp $SRCDIR/cases/$AQ_RUN_NAME/namelists/aladin2camx/INFO_ALADIN_GRIBS.nml   .
#      [ -f INFO_CAMx_GRID.nml      ] || $SRCDIR/cases/$AQ_RUN_NAME/namelists/aladin2camx/WRITE_INFO_CAMx_GRID.sh  $PROCESSORSROOT/aladin2camx/INFO_CAMx_GRID.nml
#      dt1=$(date --date="$mystartspec_alt $START_HOUR" +"%s")
#      dt2=$(date --date="$myendspec_alt   $END_HOUR"   +"%s")
#      diffs=$(( $(( $(($dt2-$dt1))/3600 )) + 1 )) # rozdil casu v hodinach
#      $SRCDIR/cases/$AQ_RUN_NAME/namelists/aladin2camx/WRITE_INFO_RUN.sh \
#        $mystartspec_alt $START_HOUR $diffs $ngridnumber $aladin_outputs $camx_run $PROCESSORSROOT/aladin2camx/INFO_RUN.nml.$mystartspec

beg_date=2015-01-01
end_date=2015-01-01
beg_date_s=`date --utc --date="--utc $beg_date 0hour" +%s`
end_date_s=`date --utc --date="--utc $end_date 0hour" +%s`

act_date=$beg_date
act_date_s=`date --utc --date="--utc $act_date 0hour" +%s`
while [ $act_date_s -le $end_date_s ]; do

      ./WRITE_INFO_RUN.sh \
        $act_date 00 25 2 /storage/shared/data/aladin/grib /storage/shared/projekty/Life_MALOPOLSKA/aladin2camx/CAMx_inp INFO_RUN.nml.$act_date
    
      ./aladin2camx_MAIN_v5.41.exe INFO_RUN.nml.$act_date  INFO_ALADIN_GRIBS.nml  INFO_CAMx_GRID.nml  aladin2camx_control.nml

  act_date=`  date --utc --date="--utc $act_date +24hour" +%Y-%m-%d`
  act_date_s=`date --utc --date="--utc $act_date   0hour" +%s`
done
