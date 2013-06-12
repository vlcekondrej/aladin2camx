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
             to process (min is 1, default is 25),

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
ED=`date --utc --date "$SD +$[${SH} +${NH} -1]hours" +%Y-%m-%d`
EH=`date -u -d "$ED +$[$SH +$NH -1]hours" +%H`
EDexcl=`date --utc --date "$SD +$[${SH} +${NH} ]hours" +%Y-%m-%d`
EHexcl=`date -u -d "$ED +$[$SH +$NH ]hours" +%H`
NG=$4 #number of grid domains
GRIBDIR=$5
CAMXDIR=$6
RUN_NML=$7


#------------------------------------------
# Setup 
begDt=`date -u -d "${SD}" +%Y%m%d`
begTm=`printf "%02d" $SH`
endDt=`date -u -d "${ED}" +%Y%m%d`
endTm=`printf "%02d" $EH`
endDtExcl=`date -u -d "${EDexcl}" +%Y%m%d`
endTmExcl=$EHexcl  #`printf "%02d" $EHexcl`

#------------------------------------------
# Write the namelist
rm -f ${RUN_NML}

# message 

echo "! This namelist was generated using:" > "${RUN_NML}"
echo "!   $*" >> "${RUN_NML}"
echo "" >> "${RUN_NML}"

# part 1
cat >> "${RUN_NML}" <<EOF
&clock_control
begYYYYMMDD   = ${begDt}
begHHMI       = ${begTm}00 ! MI (minutes) hard coded in RUN.sh 
endYYYYMMDD   = ${endDtExcl} !
endHHMI       = ${endTmExcl}00 ! MI (minutes) hard coded in RUN.sh 
met_frequency = 60 ! this is hard coded in RUN.sh
TimeZone      = 0 ! casove pasmo, ve kterem jsou uvadeny casy pocatku a konce simulace
/

EOF


# part 2
g=1
echo "&output_files" >> ${RUN_NML}
#beginEnd format: 'YYMMDD-HH_YYMMDD-HH'
beginEnd="${begDt:2:6}-${begTm}_${endDtExcl:2:6}-${endTmExcl}"
while [ $g -le $NG ] ; do
cat >> "${RUN_NML}" <<EOF
zp_file(${g})     = '${CAMXDIR}/camx.zp.d0${g}.${beginEnd}'
tp_file(${g})     = '${CAMXDIR}/camx.tp.d0${g}.${beginEnd}'
uv_file(${g})     = '${CAMXDIR}/camx.uv.d0${g}.${beginEnd}'
qa_file(${g})     = '${CAMXDIR}/camx.qa.d0${g}.${beginEnd}'
cr_file(${g})     = '${CAMXDIR}/camx.cr.d0${g}.${beginEnd}'
kv_file(${g})     = '${CAMXDIR}/camx.kv.d0${g}.${beginEnd}'
avgHGT_file(${g}) = '${CAMXDIR}/camx.avgHGT.d0${g}.${beginEnd}'
beis_file(${g})   = '${CAMXDIR}/BEISmet.d0${g}.${beginEnd}'

EOF
g=$[$g+1]
done
echo "/" >> ${RUN_NML}


# part 3
echo "&input_files" >> ${RUN_NML}
t=0
while [ $t -le $NH ] ; do
dt=`date --utc --date "$SD +$[$SH + $t -1]hour" +%Y%m%d_%H`
grb="${gribdir}/ALAD4camx_${dt}.grb"
cat >> "${RUN_NML}" <<EOF
aladin_met_names($t) = '${GRIBDIR}/ALAD4camx_${dt}.grb'
EOF
t=$[$t+1]
done
echo "/" >> ${RUN_NML}


exit


