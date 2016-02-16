SUBROUTINE run_info()
 USE module_global_variables
 USE module_datetime
 USE INTER_FACES, ONLY: julday
 IMPLICIT NONE

 INTEGER :: iunit
 INTEGER :: g, i, istat
 TYPE(tDateTime) :: beg_dt_LT, end_dt_LT, act_dt_LT, act_dt_UT ! _LT means Local Time
 INTEGER :: da,ti, ainc
 INTEGER :: unit_counter
 
 CHARACTER(LEN=200) :: text_temp ! temporary text variable

 INTEGER            :: camxinUnit
 CHARACTER(LEN=200) :: camxinFile
 CHARACTER(LEN=7)   :: Map_Projection

 CALL null_DateTime(beg_dt_LT)
 CALL null_DateTime(end_dt_LT)


 !----------------------------
 ! read in path and date/time |
 ! ---------------------------
 iunit=getFreeUnitNo()
 OPEN(UNIT=iunit,FILE=INFO_RUN_file,STATUS='OLD',DELIM='APOSTROPHE')
  READ(iunit,NML=clock_control); REWIND(iunit)
  READ(iunit,NML=input_files)  ; REWIND(iunit)
  READ(iunit,NML=output_files) 
 CLOSE(iunit)

 OPEN(UNIT=iunit,FILE=INFO_ALADIN_GRIBS_file,STATUS='OLD',DELIM='APOSTROPHE')
  READ(iunit,NML=aladin_gribs_info) 
 CLOSE(iunit)
 Alad_nVal = Alad_nX*Alad_nY

 OPEN(UNIT=iunit,FILE=INFO_CAMx_GRID_file,STATUS='OLD',DELIM='APOSTROPHE')
  READ(iunit,NML=camx_grid_info) 
 CLOSE(iunit)

 OPEN(UNIT=iunit,FILE=aladin2camx_control_file,STATUS='OLD',DELIM='APOSTROPHE')
  READ(iunit,NML=aladin2camx_control) 
 CLOSE(iunit)



 beg_dt_LT%y  = begYYYYMMDD / 10000
 beg_dt_LT%m  = MOD(begYYYYMMDD,10000) / 100
 beg_dt_LT%d  = MOD(begYYYYMMDD,100)
 beg_dt_LT%h  = begHHMI / 100
 beg_dt_LT%mi = MOD(begHHMI,100)

 end_dt_LT%y  = endYYYYMMDD / 10000
 end_dt_LT%m  = MOD(endYYYYMMDD,10000) / 100
 end_dt_LT%d  = MOD(endYYYYMMDD,100)
 end_dt_LT%h  = endHHMI / 100
 end_dt_LT%mi = MOD(endHHMI,100)

 DO g = 1, ngridnumber
   ! elemental check (CAMx grid must interlap with ALADIN grid )
   write(text_temp,'(a,I2,a)')'Grid ',g,','

   ! Check if grid is a subset of ALADIN grid
   IF (MOD(CAMx_grid_xend(g)-CAMx_grid_xbeg(g)+1,CAMx_grid_step(g)) /= 0 ) THEN
     write(*,*)trim(text_temp),' osa X: pocet bodu neni celociselnym nasobkem kroku'
     STOP
   END IF
   IF (MOD(CAMx_grid_yend(g)-CAMx_grid_ybeg(g)+1,CAMx_grid_step(g)) /= 0 ) THEN
     write(*,*)trim(text_temp),' osa Y: pocet bodu neni celociselnym nasobkem kroku'
     STOP
   END IF

 END DO

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

 WRITE(logfile,"('aladin2camx','_',I4.4,2I2.2,'-',2I2.2,'_',I4.4,2I2.2,'-',2I2.2,'.log',I5.5)") &
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
     CAMx_dx(g)=Alad_dx*CAMx_grid_step(g)
     CAMx_dy(g)=Alad_dy*CAMx_grid_step(g)

     CAMx_nx(g)=(CAMx_grid_xend(g)-CAMx_grid_xbeg(g)+1)/CAMx_grid_step(g)
     CAMx_ny(g)=(CAMx_grid_yend(g)-CAMx_grid_ybeg(g)+1)/CAMx_grid_step(g)

     ! reflects what is in CAMx meteo files - includes buffer cells
     CAMx_SWCor11_x(g)= Alad_Centr11_X + (CAMx_grid_xbeg(g)-1.5D0)*Alad_dx
     CAMx_SWCor11_y(g)= Alad_Centr11_Y + (CAMx_grid_ybeg(g)-1.5D0)*Alad_dy
 END DO

 !---------------------------------------------------------------------
 ! write information on CAMx grids in the form needed by CAMx namelist |
 ! --------------------------------------------------------------------
 SELECT CASE (Alad_PROJ)
   CASE (1)
      Map_Projection="LATLON"
      write(*,*)" ... neni osetreno zadavani parametru pro projekci ",Map_Projection
      STOP
   CASE (2)
      Map_Projection="LAMBERT"
   CASE (5)
      Map_Projection="UTM"
      write(*,*)" ... neni osetreno zadavani parametru pro projekci ",Map_Projection
      STOP
   CASE (6)
      Map_Projection="POLAR"
      write(*,*)" ... neni osetreno zadavani parametru pro projekci ",Map_Projection
      STOP
   CASE DEFAULT
     write(*,*)"CAMx neni schopen pracovat s projekci Alad_PROJ=",Alad_PROJ
     STOP
 END SELECT
      
 WRITE(camxinFile,"('camxin','_',I4.4,2I2.2,'-',2I2.2,'_',I4.4,2I2.2,'-',2I2.2,'.log',I5.5)") &
   beg_dt_LT%y,beg_dt_LT%m,beg_dt_LT%d,beg_dt_LT%h,beg_dt_LT%mi,end_dt_LT%y,end_dt_LT%m,end_dt_LT%d,end_dt_LT%h,end_dt_LT%mi,PID
 camxinUnit = getFreeUnitNo()
 OPEN(unit=camxinUnit,file=camxinFile)
   ! --- Map projection parameters ---
   WRITE(camxinUnit,"(a          )")' !--- Map projection parameters ---'
   WRITE(camxinUnit,"(a,a    ,a  )")' Map_Projection           = "',Map_Projection,'",'
   WRITE(camxinUnit,"(a,I2   ,',')")' UTM_Zone                 = ',0
   WRITE(camxinUnit,"(a,F2.0 ,',')")' POLAR_Longitude_Pole     = ',0.
   WRITE(camxinUnit,"(a,F2.0 ,',')")' POLAR_Latitude_Pole      = ',0. ! deg (west<0,south<0)
   WRITE(camxinUnit,"(a,F12.8,',')")' LAMBERT_Central_Meridian = ',Alad_PROJ_GAMMA  ! deg (west<0,south<0)
   WRITE(camxinUnit,"(a,F12.8,',')")' LAMBERT_Center_Longitude = ',Alad_X_CENT      ! deg (west<0,south<0)
   WRITE(camxinUnit,"(a,F12.8,',')")' LAMBERT_Center_Latitude  = ',Alad_Y_CENT      ! deg (west<0,south<0)
   WRITE(camxinUnit,"(a,F12.8,',')")' LAMBERT_True_Latitude1   = ',Alad_PROJ_ALPHA  ! deg (west<0,south<0)
   WRITE(camxinUnit,"(a,F12.8,',')")' LAMBERT_True_Latitude2   = ',Alad_PROJ_BETA   ! deg (west<0,south<0), can be same as LAMBERT_True_Latitude1
  
   ! --- Grid parameters ---
   WRITE(camxinUnit,"(/,a        )")' !--- Parameters for the master (first) grid ---'
   WRITE(camxinUnit,"(a,I2,','   )")' Number_of_Grids      = ',ngridnumber
   WRITE(camxinUnit,"(a,F16.9,',')")' Master_Origin_XCoord = ',CAMx_SWCor11_x(1)/1000D0  ! km or deg, SW corner of cell(1,1)
   WRITE(camxinUnit,"(a,F16.9,',')")' Master_Origin_YCoord = ',CAMx_SWCor11_y(1)/1000D0  ! km or deg, SW corner of cell (1,1)
   WRITE(camxinUnit,"(a,F12.9,',')")' Master_Cell_XSize    = ',CAMx_dx(1)/1000D0  ! km or deg
   WRITE(camxinUnit,"(a,F12.9,',')")' Master_Cell_YSize    = ',CAMx_dy(1)/1000D0  ! km or deg
   WRITE(camxinUnit,"(a,I4   ,',')")' Master_Grid_Columns  = ',CAMx_nx(1)
   WRITE(camxinUnit,"(a,I4   ,',')")' Master_Grid_Rows     = ',CAMx_ny(1)
   WRITE(camxinUnit,"(a,I3   ,',')")' Number_of_Layers(1)  = ',CAMx_nLev

   DO g = 2, ngridnumber
     WRITE(camxinUnit,"(/,a,I1,a        )")' !--- Parameters for the grid ',ngridnumber,' ---'
     WRITE(camxinUnit,"(a,I1,a,I2,','   )")' Nest_Meshing_Factor(',g,') = ',CAMx_grid_step(1)/CAMx_grid_step(g) ! Relative to master grid
     WRITE(camxinUnit,"(a,I1,a,I3,','   )")' Nest_Beg_I_Index(',g,')    = ',(CAMx_grid_xbeg(g)+1-CAMx_grid_xbeg(1))/CAMx_grid_step(1)+1 ! Relative to master grid ! CAMx_grid_xbeg include buffers
     WRITE(camxinUnit,"(a,I1,a,I3,','   )")' Nest_End_I_Index(',g,')    = ',(CAMx_grid_xend(g)  -CAMx_grid_xbeg(1))/CAMx_grid_step(1)+1 ! Relative to master grid ! CAMx_grid_xend    -- || --
     WRITE(camxinUnit,"(a,I1,a,I3,','   )")' Nest_Beg_J_Index(',g,')    = ',(CAMx_grid_ybeg(g)+1-CAMx_grid_ybeg(1))/CAMx_grid_step(1)+1 ! Relative to master grid ! CAMx_grid_ybeg    -- || --
     WRITE(camxinUnit,"(a,I1,a,I3,','   )")' Nest_End_J_Index(',g,')    = ',(CAMx_grid_yend(g)  -CAMx_grid_ybeg(1))/CAMx_grid_step(1)+1 ! Relative to master grid ! CAMx_grid_yend    -- || --
     WRITE(camxinUnit,"(a,I1,a,I2,','   )")' Number_of_Layers(',g,')    = ',CAMx_nLev
   END DO
 CLOSE(unit=camxinUnit)
 text_temp='ln -sf '//trim(camxinFile)//' camx.in.proj.and.grid.info' 
 CALL system(text_temp)

 !-----------------------------
 ! write information to logFile|
 ! ----------------------------
 WRITE(logFileUnit,"( a,I4,'-',I2.2,'-',I2.2,X,I2.2,':',I2.2,':',F2.0)") &
   '** Beginning of Simulation in local time: ',beg_dt_LT%y,beg_dt_LT%m,beg_dt_LT%d,beg_dt_LT%h,beg_dt_LT%mi,beg_dt_LT%s
 WRITE(logFileUnit,"( a,I4,'-',I2.2,'-',I2.2,X,I2.2,':',I2.2,':',F2.0)") &
   '** End of Simulation in local time      : ', end_dt_LT%y,end_dt_LT%m,end_dt_LT%d,end_dt_LT%h,end_dt_LT%mi,end_dt_LT%s
 WRITE(logFileUnit,"( a,I2)") &
   '** Time Zone                            : ', TimeZone
 WRITE(logFileUnit,"( a,I5)") &
   '** Time step in minutes                 : ', met_frequency
 WRITE(logFileUnit,"(/a,I2)")' Number of CAMx grids: ',ngridnumber
 DO g=1,ngridnumber
     text_temp='nested'; IF (g==1) text_temp='Master'
     WRITE(logFileUnit,"(/'grid ',I2,a)")g,'  ( '//trim(text_temp)//' )'
     WRITE(logFileUnit,"( '   Resolution of CAMx grid relative to ALADIN = ', I4)")CAMx_grid_step(g)
     WRITE(logFileUnit,"(/'   CAMx grid indexes in ALADIN grid (for CAMx nested grids including buffer cells):')")
     WRITE(logFileUnit,"( '     ALAD_Xmin = ',I4)")CAMx_grid_xbeg(g)
     WRITE(logFileUnit,"( '     ALAD_Xmax = ',I4)")CAMx_grid_xend(g)
     WRITE(logFileUnit,"( '     ALAD_Ymin = ',I4)")CAMx_grid_ybeg(g)
     WRITE(logFileUnit,"( '     ALAD_Ymax = ',I4)")CAMx_grid_yend(g)
     WRITE(logFileUnit,"(/'     CAMx_nx = ',I3  )")CAMx_nx(g)
     WRITE(logFileUnit,"( '     CAMx_ny = ',I3  )")CAMx_ny(g)
     WRITE(logFileUnit,"(/'     CAMx_dx = ',F8.1)")CAMx_dx(g)
     WRITE(logFileUnit,"( '     CAMx_dy = ',F8.1)")CAMx_dy(g)
 END DO

 WRITE(logFileUnit,*)
 WRITE(logFileUnit,"(/'CAMx levels definition (last level is suplemental to define ',I2,'th level upper interface)')")CAMx_nLev
 DO i = 1, CAMx_nLev+1
     WRITE(logFileUnit,"('    CAMx level(',I2,') = ',I3,' - ',I3)")i,CAMx_levDef( i,1),CAMx_levDef( i,2)
 END DO

 WRITE(logFileUnit,"(/'SMOOTHING        = ',L1)"    )SMOOTHER_SWITCH
 WRITE(logFileUnit,"( 'smoothing method = ',I1)")SMOOTHER_METHOD

 WRITE(logFileUnit,"(/'KV method  = ',a)"    )kv_method
 WRITE(logFileUnit,"( 'KV minimum = ',F10.5)")kvmin

 WRITE(logFileUnit,"(/'COD method = ',a)")cod_method
 IF (TRIM(cod_method).eq.'chimere') THEN
     WRITE(logFileUnit,"('  Low clouds    = ',a)")odMetL
     WRITE(logFileUnit,"('  Medium clouds = ',a)")odMetM
     WRITE(logFileUnit,"('  High clouds   = ',a)")odMetH
 END IF

!!!vp WRITE(logFileUnit,"(/'PROFILE is printed for point:')")
!!!vp WRITE(logFileUnit,"( '    nx = ',I3)")vpx
!!!vp WRITE(logFileUnit,"( '    ny = ',I3)")vpy

END SUBROUTINE run_info

