SUBROUTINE run_info()
 USE module_global_variables
 USE module_datetime
 USE INTER_FACES, ONLY: juldate, julday
 IMPLICIT NONE

 INTEGER :: unit_INFO_RUN, unit_INFO_GRID
 INTEGER :: g, i, istat
 TYPE(tDateTime) :: beg_dt_LT, end_dt_LT, act_dt_LT, act_dt_UT ! _LT means Local Time
 INTEGER :: da,ti, ainc
 INTEGER :: unit_counter
 CHARACTER(LEN=200), DIMENSION(0:200) :: aladin_met_names ! pomocne pole pro nasteni jmen Alad souboru z namelistu
 NAMELIST /input_file_names/ aladin_met_names
 
 CALL null_DateTime(beg_dt_LT)
 CALL null_DateTime(end_dt_LT)


 !----------------------------
 ! read in path and date/time |
 ! ---------------------------
 unit_INFO_RUN=getFreeUnitNo()
 OPEN(UNIT=unit_INFO_RUN,FILE='INFO_RUN.nml',STATUS='OLD',DELIM='APOSTROPHE')
  READ(unit_INFO_RUN,NML=info_run)!,IOSTAT=istat) 
  READ(unit_INFO_RUN,NML=info_grid)!,IOSTAT=istat) 
!   CALL TestStop(istat,'__run_info: ERROR reading info_run namelist.')
  READ(unit_INFO_RUN,NML=output_file_names) 
  READ(unit_INFO_RUN,NML=input_file_names) 
 CLOSE(unit_INFO_RUN)

 ! adds end slash, if necessary
 CALL if_not_add_char(ALAD_GRIB_DIR,'/')
 ! adds end slash, if necessary
 CALL if_not_add_char(CAMX_INP_DIR,'/')

 beg_dt_LT%y=begYYYYMMDD/10000
 beg_dt_LT%m=MOD(begYYYYMMDD,10000)/100
 beg_dt_LT%d=MOD(begYYYYMMDD,100)
 beg_dt_LT%h=begHHMI/100
 beg_dt_LT%mi=MOD(begHHMI,100)

 end_dt_LT%y=endYYYYMMDD/10000
 end_dt_LT%m=MOD(endYYYYMMDD,10000)/100
 end_dt_LT%d=MOD(endYYYYMMDD,100)
 end_dt_LT%h=endHHMI/100
 end_dt_LT%mi=MOD(endHHMI,100)

!OLD unit_INFO_RUN=getFreeUnitNo()
!OLD OPEN(UNIT=unit_INFO_RUN,FILE=INFO_RUN_FILE,STATUS='OLD',IOSTAT=istat)
!OLD   call TestStop(istat,'STOP __run_info: ERROR opening '//INFO_RUN_FILE)
!OLD
!OLD ! read directory with ALADIN files
!OLD READ(unit_INFO_RUN,'(a)',IOSTAT=istat) ALAD_GRIB_DIR
!OLD   CALL TestStop(istat,'__run_info: ERROR reading path for ALADIN gribs from '//INFO_RUN_FILE)
!OLD   ! adds end slash, if necessary
!OLD   CALL if_not_add_char(ALAD_GRIB_DIR,'/')
!OLD
!OLD ! read directory for CAMx input files
!OLD READ(unit_INFO_RUN,'(a)',IOSTAT=istat) CAMx_INP_DIR
!OLD   CALL TestStop(istat,'__run_info: ERROR reading path for CAMx input from '//INFO_RUN_FILE)
!OLD   ! adds end slash, if necessary
!OLD   CALL if_not_add_char(CAMX_INP_DIR,'/')
!OLD
!OLD ! read start date and time of simulation
!OLD READ(unit_INFO_RUN,'(I4,2I2,X,2I2,F2.0)',IOSTAT=istat) beg_dt_LT%y,beg_dt_LT%m,beg_dt_LT%d,beg_dt_LT%h,beg_dt_LT%mi,beg_dt_LT%s
!OLD   CALL TestStop(istat,'__run_info: ERROR reading beginning date and time of simulation')
!OLD
!OLD ! read end date and time of simulation
!OLD READ(unit_INFO_RUN,'(I4,2I2,X,2I2,F2.0)',IOSTAT=istat) end_dt_LT%y,end_dt_LT%m,end_dt_LT%d,end_dt_LT%h,end_dt_LT%mi,end_dt_LT%s
!OLD   CALL TestStop(istat,'__run_info: ERROR reading beginning date and time of simulation')
!OLD
!OLD ! read frequency of meteorological inputs
!OLD READ(unit_INFO_RUN,'(I8,X,I4)',IOSTAT=istat) met_frequency
!OLD
!OLD CLOSE(unit_INFO_RUN)

 
 !-----------------------------
 ! open logFile               |
 ! ----------------------------
 unit_counter=getFreeUnitNo()
 OPEN(UNIT=unit_counter,FILE='.aladin2camx.counter')
 DO
     READ(unit_counter,IOSTAT=istat,FMT='(I5.5)')PID
     IF (istat.ne.0) EXIT
 END DO
 PID=PID+1
 WRITE(unit_counter,'(I5.5)')PID
 CLOSE(unit_counter) 

 WRITE(logfile,"('aladin2camx','_',I4.4,2('.',I2.2),'_',I2.2,':',I2.2,'-',I4.4,2('.',I2.2),'_',I2.2,':',I2.2,'.log',I5.5)") &
   beg_dt_LT%y,beg_dt_LT%m,beg_dt_LT%d,beg_dt_LT%h,beg_dt_LT%mi,end_dt_LT%y,end_dt_LT%m,end_dt_LT%d,end_dt_LT%h,end_dt_LT%mi,PID
 logFileUnit = getFreeUnitNo()
 OPEN(unit=logFileUnit,file=logFile)


 !-------------------------------------------------------------------
 !  initialize aladin_met                                            |
 ! - names for ALADIN files, calc. Julian and UTC+TimeZone time, ... |
 ! ------------------------------------------------------------------

 ! get number of input grib files
 act_dt_LT = beg_dt_LT
 nAladFiles = 0
 DO WHILE (act_dt_LT .LE. end_dt_LT)
     CALL DateTime_plus_min(act_dt_LT, met_frequency)
     nAladFiles = nAladFiles+1
 END DO
 IF (nAladFiles==1) STOP'Need at least two time steps! Check start and end date/time of simulation!'

 allocate(aladin_met(0:nAladFiles))

 act_dt_LT=beg_dt_LT
 act_dt_UT=beg_dt_LT
 ! need one time step back because of subtracting of accumated fields
 CALL DateTime_plus_min(act_dt_UT, -1*met_frequency -60*TimeZone )
 CALL DateTime_plus_min(act_dt_LT, -1*met_frequency )
 DO i = 0, nAladFiles
     da = act_dt_UT%y*10000 + act_dt_UT%m*100 + act_dt_UT%d  ! da (YYYYMMDD in Universal time)
     ti = act_dt_UT%h*10000 + act_dt_UT%mi*100 + act_dt_UT%s ! ti (HHMISS in Universal time)
     aladin_met(i)%UT_YYYYMMDD = da 
     aladin_met(i)%UT_YYYYJJJ  = act_dt_UT%y*1000 + julday(da) 
     aladin_met(i)%UT_HHMISS   = ti

     da = act_dt_LT%y*10000 + act_dt_LT%m*100 + act_dt_LT%d ! da (YYYYMMDD in local time)
     aladin_met(i)%LT_YYYYMMDD = da ! date (YYYYMMDD)
     aladin_met(i)%LT_YYJJJ = MOD(act_dt_LT%y,100)*1000 + julday(da)! date (YYJJJ)
     aladin_met(i)%LT_HHMI  = act_dt_LT%h*100 + act_dt_LT%mi ! time (HHMI)

     aladin_met(i)%name_f=trim(aladin_met_names(i))

     CALL DateTime_plus_min(act_dt_UT, met_frequency)
     CALL DateTime_plus_min(act_dt_LT, met_frequency)

 END DO

 !---------------------------
 ! CAMx grid specifications  |
 ! --------------------------
 DO g=1,ngridnumber
     ! kolikrat je ALADINovsky grid hustsi, nez CAMx-ovsky; ten nejvnorenejsi CAMx-ovsky odpovida hustotou ALADIN-ovkemu 
     ainc=nest_mesh(ngridnumber)/nest_mesh(g) 

     CAMx_dx(g)=Alad_dx*ainc
     CAMx_dy(g)=Alad_dy*ainc

     CAMx_nx(g)=(nest_xend(g)-nest_xstart(g))/ainc + 1
     CAMx_ny(g)=(nest_yend(g)-nest_ystart(g))/ainc + 1

     CAMx_SWCor11_x(g)= Alad_Centr11_X + (nest_xstart(g)-1.5D0)*Alad_dx
     CAMx_SWCor11_y(g)= Alad_Centr11_Y + (nest_ystart(g)-1.5D0)*Alad_dy
 END DO

 !-----------------------------
 ! write information to logFile|
 ! ----------------------------
 WRITE(logFileUnit,"(a,I4,'-',I2.2,'-',I2.2,X,I2.2,':',I2.2,':',F2.0)") &
   ' ** Beginning of Simulation in local time: ',beg_dt_LT%y,beg_dt_LT%m,beg_dt_LT%d,beg_dt_LT%h,beg_dt_LT%mi,beg_dt_LT%s
 WRITE(logFileUnit,"(a,I4,'-',I2.2,'-',I2.2,X,I2.2,':',I2.2,':',F2.0)") &
   ' ** End of Simulation in local time     : ', end_dt_LT%y,end_dt_LT%m,end_dt_LT%d,end_dt_LT%h,end_dt_LT%mi,end_dt_LT%s
 WRITE(logFileUnit,"(a,I5)")' ** Time step in minutes   : ', met_frequency
 WRITE(logFileUnit,*)
 WRITE(logFileUnit,'(a,I1)')' Number of CAMx grids: ',ngridnumber
 DO g=1,ngridnumber
     IF (g==1) THEN
         WRITE(logFileUnit,"('   mother grid: ')")
     ELSE
         WRITE(logFileUnit,"('   grid number: ',I1)")g
     END IF
     WRITE(logFileUnit,"('      meshing factor relative to mother grid = ', I4)")nest_mesh(g)
     WRITE(logFileUnit,"('      ALADIN grid subsection:')")
     WRITE(logFileUnit,"('                        Xmin = ',I4)")nest_xstart(g)
     WRITE(logFileUnit,"('                        Xmax = ',I4)")nest_xend(g)
     WRITE(logFileUnit,"('                        Ymin = ',I4)")nest_ystart(g)
     WRITE(logFileUnit,"('                        Ymax = ',I4)")nest_yend(g)
     WRITE(logFileUnit,*)
     WRITE(logFileUnit,"('      nx = ',I3)")CAMx_nx(g)
     WRITE(logFileUnit,"('      ny = ',I3)")CAMx_ny(g)
     WRITE(logFileUnit,"('      dx = ',F8.1)")CAMx_dx(g)
     WRITE(logFileUnit,"('      dy = ',F8.1)")CAMx_dy(g)
 END DO

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"('CAMx levels definition (last level is suplemental to define ',I2,'th level upper interface)')")CAMx_nLev
 DO i = 1, CAMx_nLev+1
     WRITE(logFileUnit,"('    CAMx level(',I2,') = ',I2,' - ',I2)")i,CAMx_levDef( i,1),CAMx_levDef( i,2)
 END DO

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"('SMOOTHING  = ',L1)"    )SMOOTHER_SWITCH
 WRITE(logFileUnit,"('smoothing method = ',I1)")SMOOTHER_METHOD

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"('KV method  = ',a)"    )kv_method
 WRITE(logFileUnit,"('KV minimum = ',F10.5)")kvmin

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"('COD method = ',a)")cod_method
 IF (TRIM(cod_method).eq.'chimere') THEN
     WRITE(logFileUnit,"('  Low clouds    = ',a)")odMetL
     WRITE(logFileUnit,"('  Medium clouds = ',a)")odMetM
     WRITE(logFileUnit,"('  High clouds   = ',a)")odMetH
 ELSE
     WRITE(logFileUnit,*)
     WRITE(logFileUnit,*)
     WRITE(logFileUnit,*)
 END IF

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"('PROFILE is printed for point:')")
 WRITE(logFileUnit,"('    nx = ',I3)")vpx
 WRITE(logFileUnit,"('    ny = ',I3)")vpy

END SUBROUTINE run_info

