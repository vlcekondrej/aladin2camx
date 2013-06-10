SOURCEDIR=./ALADIN_GRIB
TARGETDIR=./CAMX_INP
STORAGEDIR=/media/disk/CAMx_inp/met_307x275x16/
#SCPDIR=/mnt/storage1/aq/data/input/
SCPDIR=/mnt/storage1/aq/data/input/met/
KVMETHOD=_cmaq
CODMETHOD=_wrfcamx

SIMLEN=25 # delka simulace v hodinach

for YYYY in 2008
do
  for MM in 1
  do
    MM=`printf "%02d" $MM`
    month_length=`date --date="$YYYY-$MM-01 +1month -1day" +%d`
    for D in $(seq 30 ${month_length}); do
#    for D in $(seq  1 1); do
      DD=`printf "%02d" ${D}`

      case $MM in
        01|02|03|10|11|12)
           TIME_ZONE=1;
           HH=01;;
        *) 
          TIME_ZONE=2;
          HH=02;;
      esac 
      
      YYYY2=`date --date="$YYYY-$MM-$DD + $[$HH +$SIMLEN -1]hour" +%Y`
      YY2=`date   --date="$YYYY-$MM-$DD + $[$HH +$SIMLEN -1]hour" +%y`
      MM2=`date   --date="$YYYY-$MM-$DD + $[$HH +$SIMLEN -1]hour" +%m`
      DD2=`date   --date="$YYYY-$MM-$DD + $[$HH +$SIMLEN -1]hour" +%d`
      HH2=`date   --date="$YYYY-$MM-$DD + $[$HH +$SIMLEN -1]hour" +%H`

cat >INFO_RUN.nml <<EOF
&info_run
ALAD_GRIB_DIR = '${SOURCEDIR}',
CAMX_INP_DIR  = '${TARGETDIR}',
begYYYYMMDD   = ${YYYY}${MM}${DD},
begHHMI       = ${HH}00,
endYYYYMMDD   = ${YYYY2}${MM2}${DD2},
endHHMI       = ${HH2}00,
met_frequency = 60,
TimeZone      = $TIME_ZONE
/
EOF

      echo pocitam $YYYY-$MM-$DD
      ./aladin2camx_MAIN.exe
      sleep 60
      # read preprocessor identificator number
      PID=`tail -n 1 .aladin2camx.counter`
  
      FINISHED=0
      CYCLES=1
      while [ $FINISHED -eq 0 ]; do
        sleep 60
        FINISHED=`ls aladin2camx.$PID.end | wc -l`
        CYCLES=$[$CYCLES+1]
        # pokud by pocital prilis dlouho, tak ho shod
        if [ ${CYCLES} -gt 10 ]; then exit 1; fi
      done
      rm aladin2camx.$PID.end

mv  ${TARGETDIR}/camx.zp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.zp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.zp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.zp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv  ${TARGETDIR}/camx.qa.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.qa.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.qa.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.qa.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv  ${TARGETDIR}/camx.cr.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${TARGETDIR}/camx.cr${CODMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}
mv  ${TARGETDIR}/camx.cr${CODMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.cr${CODMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.cr${CODMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.cr${CODMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv  ${TARGETDIR}/camx.tp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.tp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.tp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.tp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv  ${TARGETDIR}/camx.uv.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.uv.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.uv.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.uv.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv ${TARGETDIR}/camx.kv.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}  ${TARGETDIR}/camx.kv${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}
mv ${TARGETDIR}/camx.kv${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}  ${STORAGEDIR}/
scp ${STORAGEDIR}/camx.kv${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.kv${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.kv${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

mv ${TARGETDIR}/BEISmet.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}.nc  ${STORAGEDIR}/BEISmet.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}.nc
scp ${STORAGEDIR}/BEISmet.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}.nc met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/BEISmet.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}.nc met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/BEISmet.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}.nc &

mv ${TARGETDIR}/camx.avgHGT.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}  ${STORAGEDIR}/camx.avgHGT.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}
scp ${STORAGEDIR}/camx.avgHGT.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ &
#scp ${TARGETDIR}/camx.avgHGT.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} met@10.5.75.249:${SCPDIR}/ && \
# rm ${TARGETDIR}/camx.avgHGT.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} &

#rm ${TARGETDIR}/camx_tmp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}
#mv ${TARGETDIR}/camx_tmp.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2} ${STORAGEDIR}/camx_tmp${KVMETHOD}.d01.${YYYY}-${MM}-${DD}-${HH}_${YYYY2}-${MM2}-${DD2}-${HH2}

#rm ${TARGETDIR}/emiss*

    done
  done
done
