PROGRAM aladin2camx_MAIN
 USE module_global_variables
 USE module_physical_constants
 USE module_BEISmet
 USE netcdf

 IMPLICIT NONE
 INTEGER :: g ! grid index; g=1 for driving grid, >1 for nested
 INTEGER :: aladin_unit, istat, d, unit_nml, k

 ! run_info will read information on run
 CALL run_info()

 IF (BEIS_flag) CALL alloc_netCDFids(ngridnumber)

 ! open CAMx input files for every nested grid and meteorological parameter
 DO g=1,ngridnumber

     zp_unit(g)=getFreeUnitNo()
     OPEN(UNIT=zp_unit(g),FILE=zp_file(g),   FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx HEIGHT/PRESSURE FILE',logFileUnit)

     tp_unit(g)=getFreeUnitNo()
     OPEN(UNIT=tp_unit(g),FILE=tp_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx TEMPERATURE',logFileUnit)

     uv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=uv_unit(g),FILE=uv_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WIND',logFileUnit)

     qa_unit(g)=getFreeUnitNo()
     OPEN(UNIT=qa_unit(g), FILE=qa_file(g),    FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WATER VAPOR FILE',logFileUnit)

     cr_unit(g)=getFreeUnitNo()
     OPEN(UNIT=cr_unit(g), FILE=cr_file(g),    FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx CLOUD/RAIN FILE',logFileUnit)

     kv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=kv_unit(g),FILE=kv_file(g),   FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx RKV FILE',logFileUnit)

     IF (BEIS_flag) THEN
         CALL BEISmet_createHeader(g=g,ncfname=beis_file(g))
         CALL BEISmet_putTime(netCDFid(g),TFLAG_varID(g))
     END IF

     ! soubor s prumernymi vyskami hladin CAMxu
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
     CALL get_aladin_fields(aladin_met(d)%name_f, d) 
     IF ( d .gt. 0 ) THEN 
         ! generate CAMx meteo inputs
         CALL get_h_p_t_wv(d)
     ENDIF
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
     CLOSE(zp_unit(g))
     CLOSE(tp_unit(g))
     CLOSE(uv_unit(g))
     CLOSE(qa_unit(g))
     CLOSE(cr_unit(g))
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

 CLOSE(logFileUnit)

 ! create file which existence ificates end of run
 WRITE(logFile,"('aladin2camx.',I5.5,'.end')")PID
 OPEN(unit=logFileUnit,file=logfile)
 CLOSE(logFileUnit)
END PROGRAM aladin2camx_MAIN

