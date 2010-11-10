PROGRAM aladin2camx_MAIN
 USE module_global_variables
 USE module_physical_constants
 USE module_BEISmet
 USE netcdf

 IMPLICIT NONE
 INTEGER :: g ! grid index; g=1 for driving grid, >1 for nested
 INTEGER :: aladin_unit, istat, d, unit_nml, k

 CHARACTER(LEN= 8) :: begDateStr, endDateStr
 CHARACTER(LEN= 4) :: begTimeStr, endTimeStr
 CHARACTER(LEN=27) :: datestring ! part of the name of CAMx input files - e.g. "20041204.010000_20041205.000000"
 CHARACTER(LEN= 3) :: gridNoString ! characrer for grid number

 ! read aladin2camx namelist
 unit_nml=1
 OPEN(UNIT=unit_nml,FILE='aladin2camx.nml',STATUS='OLD',DELIM='APOSTROPHE')
 READ(unit_nml,NML=aladin2camx_control)
 CLOSE(unit_nml)

 ! from INFO_RUN  gets - Aladin and CAMx directories
 !                     - start date and end date
 ! from INFO_GRID gets - total number of grids (mother & nested)
 !                     - position of grids relative to ALADIN grid                      
 CALL run_info()

 
 ! create filenames for the input parameters:
 ! camx.zp.dxx.YYYY-MM-DD-HH_YYYY-MM-DD-HH !!! Dates and hour are in local time !!! 
 ! eg.  camx.zp.dxx.2004-02-24-01_2004-02-25-00
 WRITE(begDateStr,'(I8.8)') aladin_met(1)%LT_YYYYMMDD
 WRITE(begTimeStr,'(I4.4)') INT(aladin_met(1)%LT_HHMI)
 WRITE(endDateStr,'(I8.8)') aladin_met(nAladFiles)%LT_YYYYMMDD
 WRITE(endTimeStr,'(I4.4)') INT(aladin_met(nAladFiles)%LT_HHMI)
 datestring=begDateStr(1:4)//'-'//begDateStr(5:6)//'-'//begDateStr(7:8)//'-'//begTimeStr(1:2)//'_'// &
            endDateStr(1:4)//'-'//endDateStr(5:6)//'-'//endDateStr(7:8)//'-'//endTimeStr(1:2)

 IF (BEIS_flag) CALL alloc_netCDFids(ngridnumber)

 ! open CAMx input files for every nested grid and meteorological parameter
 DO g=1,ngridnumber
     WRITE(GridNoString,'(A,I2.2)')'d', g

     h_p_unit(g)=getFreeUnitNo()
     OPEN(UNIT=h_p_unit(g),FILE=h_p_file(g),   FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx HEIGHT/PRESSURE FILE',logFileUnit)

     temp_unit(g)=getFreeUnitNo()
     OPEN(UNIT=temp_unit(g),FILE=temp_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx TEMPERATURE',logFileUnit)

     wind_unit(g)=getFreeUnitNo()
     OPEN(UNIT=wind_unit(g),FILE=wind_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WIND',logFileUnit)

     wv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=wv_unit(g), FILE=wv_file(g),    FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WATER VAPOR FILE',logFileUnit)

     cl_unit(g)=getFreeUnitNo()
     OPEN(UNIT=cl_unit(g), FILE=cl_file(g),    FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx CLOUD/RAIN FILE',logFileUnit)

     rkv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=rkv_unit(g),FILE=rkv_file(g),   FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx RKV FILE',logFileUnit)

     IF (BEIS_flag) THEN
         CALL BEISmet_createHeader(g=g,ncfname=beis_file(g))
         CALL BEISmet_putTime(netCDFid(g),TFLAG_varID(g))
     END IF

     avgHGT_unit(g)=getFreeUnitNo()
     OPEN(UNIT=avgHGT_unit(g),FILE=avgHGT_file(g))

!tmpFile tmp_unit=getFreeUnitNo()
!tmpFile OPEN(UNIT=tmp_unit,FILE=TRIM(CAMx_INP_DIR)//'camx_tmp.'//GridNoString//'.'//dateString, &
!tmpFile  & FORM='UNFORMATTED',access='direct',recl=4*Alad_nx*Alad_ny,ACTION='WRITE',STATUS='NEW',IOSTAT=istat)
!tmpFileCALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx tmp  FILE',logFileUnit)
!irec=1
 END DO

 ! allocate arrays for aladin fields
 CALL Alloc_alad()
 ! loop over all aladin files and extract/calculate/write out fields
 DO d=0,nAladFiles
     ! get ALADIN fields
     CALL get_aladin_fields(aladin_met(d)%name_f) 
     IF ( d==0 ) CYCLE ! just read accumulated fields from one time step before start
     ! generate CAMx meteo inputs
     CALL get_h_p_t_wv(d)
 END DO

 ! write average layer heights in km for computation of camx photolysis rates (tuv.in.sh)
 ! average is taken over domain and time steps
 CAMx_avgLevHgt=CAMx_avgLevHgt/nAladFiles*.001
 do g=1,ngridnumber
   write(avgHGT_unit(g),'(a)')'average_lev_heights[kmAGL]'
   write(avgHGT_unit(g),'(100F10.3)')(CAMx_avgLevHgt(k,g),k=1,CAMx_nLev)
 end do

 ! deallocate ALADIN fields
 CALL DEALLOC_ALAD()

 ! closing all binary files at the end of preprocessor
 DO g=1,ngridnumber
     CLOSE(h_p_unit(g))
     CLOSE(temp_unit(g))
     CLOSE(wind_unit(g))
     CLOSE(wv_unit(g))
     CLOSE(cl_unit(g))
     CLOSE(avgHGT_unit(g))
 END DO
!tmpFile close(tmp_unit)

 ! deallocate NetCDF IDs and close NetCDF files
 IF (BEIS_flag) THEN
     DO g=1,ngridnumber
         iret = nf90_close(netCDFid(g))
         CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
     END DO
     CALL dealloc_netCDFids()
 END IF

 ! congratulations
 WRITE(logFileUnit,*)
 WRITE(logFileUnit,*) '*******************************'
 WRITE(logFileUnit,*) '*                             *'
 WRITE(logFileUnit,*) '* !!!!! CONGRATULATIONS !!!!! *'
 WRITE(logFileUnit,*) '*                             *'
 WRITE(logFileUnit,*) '*      NO ERRORS OCCURED      *'
 WRITE(logFileUnit,*) '*                             *'
 WRITE(logFileUnit,*) '*******************************'
 WRITE(logFileUnit,*)
 WRITE(logFileUnit,*) 'ALADIN MET FIELDS ARE READY FOR CAMX RUN IN DIRECTORY: '
 WRITE(logFileUnit,*) '    ',TRIM(CAMx_INP_DIR)
 WRITE(logFileUnit,*)

 CLOSE(logFileUnit)

 ! create file which existence ificates end of run
 WRITE(logFile,"('aladin2camx.',I5.5,'.end')")PID
 OPEN(unit=logFileUnit,file=logfile)
 CLOSE(logFileUnit)
END PROGRAM aladin2camx_MAIN

