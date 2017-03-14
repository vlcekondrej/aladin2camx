PROGRAM aladin2camx_MAIN
 USE module_global_variables
 USE module_physical_constants
 USE module_ioapi
 USE netcdf

 IMPLICIT NONE
 INTEGER :: g ! grid index; g=1 for driving grid, >1 for nested
 INTEGER :: aladin_unit, istat, d, unit_nml, k
 INTEGER :: shift=0

 CHARACTER(len=32) :: arg
 CHARACTER(len=100) :: progName
          
 
 ! ----------------------------------------------------------------------------
 ! read command line arguments and files with information on run
 ! ----------------------------------------------------------------------------
 CALL GET_COMMAND_ARGUMENT(1, arg)
 IF ( trim(arg) == "--info" ) THEN 
     ! known option
     shift=1
 END IF

 IF (COMMAND_ARGUMENT_COUNT()-shift .LT. 4 ) THEN
     WRITE(*,*)"Error: missing input arguments. Program usage:"
     WRITE(*,*)"     ./aladin2camx_MAIN.exe [option] <INFO_RUN_file> <INFO_ALADIN_GRIBS_file> <INFO_CAMx_GRID_file> <aladin2camx_control_file>"
     WRITE(*,*)
     WRITE(*,*)"     option:"
     WRITE(*,*)"      --info: just write out input parameters and quit"
     STOP
 END IF
 CALL GET_COMMAND_ARGUMENT(0      , progName)
 CALL GET_COMMAND_ARGUMENT(1+shift, INFO_RUN_file)
 CALL GET_COMMAND_ARGUMENT(2+shift, INFO_ALADIN_GRIBS_file)
 CALL GET_COMMAND_ARGUMENT(3+shift, INFO_CAMx_GRID_file)
 CALL GET_COMMAND_ARGUMENT(4+shift, aladin2camx_control_file)
 write(*,*)"RUNNING: ",trim(progName)," ",trim(INFO_RUN_file)," ",trim(INFO_ALADIN_GRIBS_file),&
            " ",trim(INFO_CAMx_GRID_file)," ",trim(aladin2camx_control_file)

 ! run_info will read information on run
 CALL run_info()
 IF ( trim(arg) == "--info" ) GO TO 9999
 
 ! ----------------------------------------------------------------------------
 ! open CAMx, BEIS and MEGAN input files for every grid
 ! ----------------------------------------------------------------------------
 DO g=1,ngridnumber

     zp_unit(g)=getFreeUnitNo()
     OPEN(UNIT=zp_unit(g), FILE=zp_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx HEIGHT/PRESSURE FILE "'//&
                           trim(zp_file(g))//'"',logFileUnit)

     tp_unit(g)=getFreeUnitNo()
     OPEN(UNIT=tp_unit(g), FILE=tp_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx TEMPERATURE "'//&
                           trim(tp_file(g))//'"',logFileUnit)

     uv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=uv_unit(g), FILE=uv_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WIND "'//&
                           trim(uv_file(g))//'"',logFileUnit)

     qa_unit(g)=getFreeUnitNo()
     OPEN(UNIT=qa_unit(g), FILE=qa_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx WATER VAPOR FILE "'//&
                           trim(qa_file(g))//'"',logFileUnit)

     cr_unit(g)=getFreeUnitNo()
     OPEN(UNIT=cr_unit(g), FILE=cr_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx CLOUD/RAIN FILE "'//&
                           trim(cr_file(g))//'"',logFileUnit)

     kv_unit(g)=getFreeUnitNo()
     OPEN(UNIT=kv_unit(g), FILE=kv_file(g), FORM='UNFORMATTED',ACTION='WRITE',IOSTAT=istat)
       CALL TestStop(istat,'STOP __aladin2camx_MAIN: COULD NOT CREATE CAMx RKV FILE "'//&
                           trim(kv_file(g))//'"',logFileUnit)

     IF (BEIS_flag) THEN
         CALL BEISmet_createHeader(g=g,ncfname=beis_file(g))
         CALL putTime(BEIS_netCDFid(g),BEIS_TFLAG_varID(g),BEIS_nvar)
     END IF

     IF (MEGAN_flag) THEN
         CALL MEGANmet_createHeader(g=g,ncfname=megan_file(g))
         CALL putTime(MEGAN_netCDFid(g),MEGAN_TFLAG_varID(g),MEGAN_nvar)
     END IF

     ! soubor s prumernymi vyskami hladin CAMxu
     avgHGT_unit(g)=getFreeUnitNo()
     OPEN(UNIT=avgHGT_unit(g),FILE=avgHGT_file(g))

 END DO

 ! ----------------------------------------------------------------------------
 ! read and process ALADIN fields
 ! ----------------------------------------------------------------------------

 ! allocate arrays for aladin fields
 CALL Alloc_alad()

 IF (MEGAN_flag) THEN
     DO d=-23,-1
         CALL get_aladin_fields(aladin_met(d)%name_f, d) ! total rain only
     END DO
 END IF

 ! loop over all aladin files and extract/calculate/write out fields
 CALL get_aladin_fields(aladin_met(0)%name_f, d) ! for SolRad and rain fields only

 DO d=1,nAladFiles
     ! get ALADIN fields, generate CAMx meteo inputs
     CALL get_aladin_fields(aladin_met(d)%name_f, d) 
     CALL get_h_p_t_wv(d)
 END DO

 ! ----------------------------------------------------------------------------
 ! write average layer heights in km for computation of camx photolysis rates (tuv.in.sh)
 ! average is taken over domain and time steps
 ! ----------------------------------------------------------------------------
 CAMx_avgLevHgt=CAMx_avgLevHgt/nAladFiles*.001
 do g=1,ngridnumber
   write(avgHGT_unit(g),'(a)')'average_lev_heights[kmAGL]'
   write(avgHGT_unit(g),'(100F10.3)')(CAMx_avgLevHgt(k,g),k=1,CAMx_nLev)
 end do

 ! ----------------------------------------------------------------------------
 ! Deallocate fields and close files
 ! ----------------------------------------------------------------------------

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

 ! close BEIS NetCDF files
 IF (BEIS_flag) THEN
     DO g=1,ngridnumber
         istat = nf90_close(BEIS_netCDFid(g))
         CALL TestStop(istat-nf90_NoErr,trim(nf90_strerror(istat)),logFileUnit)
     END DO
 END IF

 ! close MEGAN NetCDF files
 IF (MEGAN_flag) THEN
     DO g=1,ngridnumber
         istat = nf90_close(MEGAN_netCDFid(g))
         CALL TestStop(istat-nf90_NoErr,trim(nf90_strerror(istat)),logFileUnit)
     END DO
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

 9999 CONTINUE
 CLOSE(logFileUnit)

 ! create file which existence ificates end of run
 WRITE(logFile,"('aladin2camx.',I5.5,'.end')")PID
 OPEN(unit=logFileUnit,file=logfile)
 CLOSE(logFileUnit)
END PROGRAM aladin2camx_MAIN

