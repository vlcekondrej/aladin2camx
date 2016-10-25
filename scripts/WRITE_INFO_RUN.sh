#!/bin/bash

if [ $# -ne 7 ] ; then 
cat << EOF

Script to write  INFO_RUN.nml


To run this script, call it from this
directory
  ./write_info_run.sh YYYY-MM-DD SH NH NG GRIBDIR CAMXDIR NML

 YYYY-MM-DD  is a string representing 
             the date

 SH          the starting hour (0 - 23 are valid),

 NH          the number of hours 
             to process (min is 1),

 NG          the number of grid domains
             that will be prepared (min is 1),

 GRIBDIR     the directory where the grib files will be,

 CAMXDIR     where the camx files will be put,

 NML         namelist file name to be written.

EOF
exit 1
fi

#------------------------------------------
# Command line inputs
SD=$1 #start date
SH=$2 # start hour
NH=$3 # number of hours
ED=`date -u -d "$SD +$[$SH +$NH -1]hours" +%Y-%m-%d`
EH=`date -u -d "$ED +$[$SH +$NH -1]hours" +%H`
NG=$4 #number of grid domains
GRIBDIR=$5
CAMXDIR=$6
RUN_NML=${7:-./INFO_RUN.nml}


#------------------------------------------
# Setup 
begD=`date -u -d "${SD}" +%Y%m%d`
begT=`printf "%02d" $SH`
endD=`date -u -d "${ED}" +%Y%m%d`
endT=`printf "%02d" $EH`

# Source extra functions
. FILE_FROM_DATE.sh

#------------------------------------------
# Write the namelist
rm -f ${RUN_NML}

# message 

echo "! This namelist was generated using:" > "${RUN_NML}"
echo "!   $0   $*" >> "${RUN_NML}"
echo "" >> "${RUN_NML}"

#--------------------------------------------------------
# part 1/3
cat >> "${RUN_NML}" <<EOF
&clock_control
begYYYYMMDD   = ${begD}
begHHMI       = ${begT}00 ! MI (minutes) hard coded in $0 
endYYYYMMDD   = ${endD}   !
endHHMI       = ${endT}00 ! MI (minutes) hard coded in $0 
met_frequency = 60 ! this is hard coded in $0
TimeZone      = 0  ! casove pasmo, ve kterem jsou uvadeny casy pocatku a konce simulace
/

EOF

#--------------------------------------------------------
# part 2/3
g=1
echo "&output_files" >> ${RUN_NML}
#beginEnd format: 'YYMMDD-HH_YYMMDD-HH'
beginEnd="${begD:2:6}-${begT}_${endD:2:6}-${endT}"
while [ $g -le $NG ] ; do
cat >> "${RUN_NML}" <<EOF
zp_file(${g})     = '${CAMXDIR}/camx.zp.d0${g}.${beginEnd}'
tp_file(${g})     = '${CAMXDIR}/camx.tp.d0${g}.${beginEnd}'
uv_file(${g})     = '${CAMXDIR}/camx.uv.d0${g}.${beginEnd}'
qa_file(${g})     = '${CAMXDIR}/camx.qa.d0${g}.${beginEnd}'
cr_file(${g})     = '${CAMXDIR}/camx.cr.d0${g}.${beginEnd}'
kv_file(${g})     = '${CAMXDIR}/camx.kv.d0${g}.${beginEnd}'
avgHGT_file(${g}) = '${CAMXDIR}/camx.avgHGT.d0${g}.${beginEnd}'
beis_file(${g})   = '${CAMXDIR}/BEISmet.d0${g}.${beginEnd}' ! used only if BEIS_flag=.TRUE.
megan_file(${g})  = '${CAMXDIR}/MEGANmet.d0${g}.${beginEnd}' ! used only if MEGAN_flag=.TRUE.

EOF
g=$[$g+1]
done
echo -e "/\n" >> ${RUN_NML}


#--------------------------------------------------------
# part 3/3
cat >> ${RUN_NML} << EOF
&input_files
! aladin_met_names(t) for t = -23,..., -1 are used only if 24-h accumulated
! fields are needed (currently just if MEGAN_flag is .TRUE.)

EOF
t=-23
DT_alad=`date --date="$SD +$[$SH - 24]hours" +%Y-%m-%d\ %H` # begin hour is 24 less to get 24-h accumulated precipitation
DT_alad_s=`date --date="$DT_alad"  +%s`
endDT_alad_s=`date --date="$SD +$[$SH + $NH - 1]hours"  +%s` 
while [ $DT_alad_s -le $endDT_alad_s ]; do
  # a2c=$(aladin2camxGribName $SD $h)
  # a2c=$(aladinGribName_00fc $SD $h)
  a2c=$(aladinGribName_cont ${DT_alad:0:10} ${DT_alad:11:12})
  echo "aladin_met_names(`printf "%03d" $t`) = '${GRIBDIR}/${a2c}'" >> ${RUN_NML}
  t=$(($t+1))
  DT_alad=`date --date="$DT_alad + 1hour"  +%Y-%m-%d\ %H`
  DT_alad_s=`date --date="$DT_alad"  +%s`
done
echo "/" >> ${RUN_NML}


exit


