module module_beismet
USE module_global_variables
USE netcdf

 ! IDs for netCDF files
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: netCDFid(:)

 ! dimension ids
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: TSTEP_dimID, DATE_MINUS_TIME_dimID
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: LAY_dimID, VAR_dimID
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: ROW_dimID, COL_dimID

 ! variable ids
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: PRSFC_varID, TEMPSFC_varID, SWRSFC_varID, TFLAG_varID

 ! error flag
 integer :: iret

CONTAINS

SUBROUTINE alloc_netCDFids(n)
 IMPLICIT NONE
 INTEGER :: n

 ALLOCATE(netCDFid(n))

 ALLOCATE(TSTEP_dimID(n))
 ALLOCATE(DATE_MINUS_TIME_dimID(n))
 ALLOCATE(LAY_dimID(n))
 ALLOCATE(VAR_dimID(n))
 ALLOCATE(ROW_dimID(n), COL_dimID(n))

 ALLOCATE(PRSFC_varID(n), TEMPSFC_varID(n), SWRSFC_varID(n), TFLAG_varID(n))

END SUBROUTINE alloc_netCDFids

! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - 

SUBROUTINE dealloc_netCDFids()
 IMPLICIT NONE

 IF (ALLOCATED(netCDFid))              DEALLOCATE(netCDFid)

 IF (ALLOCATED(TSTEP_dimID))           DEALLOCATE(TSTEP_dimID)
 IF (ALLOCATED(DATE_MINUS_TIME_dimID)) DEALLOCATE(DATE_MINUS_TIME_dimID)
 IF (ALLOCATED(LAY_dimID))             DEALLOCATE(LAY_dimID)
 IF (ALLOCATED(VAR_dimID))             DEALLOCATE(VAR_dimID)
 IF (ALLOCATED(ROW_dimID))             DEALLOCATE(ROW_dimID)
 IF (ALLOCATED(COL_dimID))             DEALLOCATE(COL_dimID)

 IF (ALLOCATED(PRSFC_varID))           DEALLOCATE(PRSFC_varID)
 IF (ALLOCATED(TEMPSFC_varID))         DEALLOCATE(TEMPSFC_varID)
 IF (ALLOCATED(SWRSFC_varID))          DEALLOCATE(SWRSFC_varID)
 IF (ALLOCATED(TFLAG_varID))           DEALLOCATE(TFLAG_varID)

END SUBROUTINE dealloc_netCDFids

! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

SUBROUTINE BEISmet_createHeader(g, ncfname)
 !
 ! creates NetCDF file for BEIS meteorology and place it in a data mode.
 !
 IMPLICIT NONE 
 INTEGER,          INTENT(IN   ) :: g ! grid number
 CHARACTER(LEN=*), INTENT(IN   ) :: ncfname

 ! error
 INTEGER(4) :: iret

 ! variable shapes
 INTEGER(4), DIMENSION(:) :: TFLAG_dims(3)   ! tSTEP,DATE,DATEMINUSTIME
 INTEGER(4), DIMENSION(:) :: PRSFC_dims(4)   ! PRSFC_rank
 INTEGER(4), DIMENSION(:) :: TEMPSFC_dims(4) ! TEMPSFC_rank
 INTEGER(4), DIMENSION(:) :: SWRSFC_dims(4)  ! SWRSFC_rank

 ! global attribs
 CHARACTER(LEN=200) :: gdnam, upnam

 INTEGER(4) :: YYYYJJJ, HHMMSS ! a date with year and julian day; time
 INTEGER(4) :: t ! index
 INTEGER(4) :: TimeStep

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !
 ! Write the header info
 !
 ! enter define mode
! print*, "Entering netCDF define mode. Lib version ",nf90_inq_libvers()
 iret = nf90_create(ncfname, NF90_CLOBBER, netCDFid(g))
 CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! define dimensions
 iret = nf90_def_dim(netCDFid(g), 'TSTEP'    , NF90_UNLIMITED , TSTEP_dimID(g)) 
 iret = nf90_def_dim(netCDFid(g), 'DATE-TIME', 2              , DATE_MINUS_TIME_dimID(g)) ! this means what??
 iret = nf90_def_dim(netCDFid(g), 'LAY'      , 1              , LAY_dimID(g)) ! n LAYers
 iret = nf90_def_dim(netCDFid(g), 'VAR'      , 3              , VAR_dimID(g)) ! temp, press, SWR + time
 iret = nf90_def_dim(netCDFid(g), 'ROW'      , CAMx_ny(g)     , ROW_dimID(g))
 iret = nf90_def_dim(netCDFid(g), 'COL'      , CAMx_nx(g)     , COL_dimID(g))
 
 ! define variables
 TFLAG_dims(3) = TSTEP_dimID(g) ! TSTEPS 
 TFLAG_dims(2) = VAR_dimID(g) ! VARS
 TFLAG_dims(1) = DATE_MINUS_TIME_dimID(g) ! dates
 iret = nf90_def_var(netCDFid(g), 'TFLAG           ', NF90_INT ,  TFLAG_dims , TFLAG_varID(g))

 PRSFC_dims(4) = TSTEP_dimID(g)
 PRSFC_dims(3) = LAY_dimID(g)
 PRSFC_dims(2) = ROW_dimID(g)
 PRSFC_dims(1) = COL_dimID(g)
 iret = nf90_def_var(netCDFid(g), 'PRSFC           ', NF90_REAL,  PRSFC_dims , PRSFC_varID(g))

 TEMPSFC_dims(4) = TSTEP_dimID(g)
 TEMPSFC_dims(3) = LAY_dimID(g)
 TEMPSFC_dims(2) = ROW_dimID(g)
 TEMPSFC_dims(1) = COL_dimID(g)
 iret = nf90_def_var(netCDFid(g), 'TEMPSFC         ', NF90_REAL, TEMPSFC_dims, TEMPSFC_varID(g))

 SWRSFC_dims(4) = TSTEP_dimID(g)
 SWRSFC_dims(3) = LAY_dimID(g)
 SWRSFC_dims(2) = ROW_dimID(g)
 SWRSFC_dims(1) = COL_dimID(g)
 iret = nf90_def_var(netCDFid(g), 'SWRSFC          ', NF90_REAL,  SWRSFC_dims, SWRSFC_varID(g))

 ! assign VARIABLE attributes
 ! TIME
 iret=nf90_put_att(netCDFid(g),TFLAG_varID(g),  'units',    '<YYYYDDD,HHMMSS>')  
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),TFLAG_varID(g),  'long_name','TFLAG           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),TFLAG_varID(g),  'var_desc', 'Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS                                ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Pressure
 iret=nf90_put_att(netCDFid(g),PRSFC_varID(g),  'long_name','PRSFC           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),PRSFC_varID(g),  'units',    'Pascal (Pa)     ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),PRSFC_varID(g),  'var_desc', 'surface pressure                                                                ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Temperature
 iret=nf90_put_att(netCDFid(g),TEMPSFC_varID(g),'long_name','TEMP surface    ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),TEMPSFC_varID(g),'units',    'Kelvin (K)      ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),TEMPSFC_varID(g),'var_desc', 'air temperature at lowest model level                                           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Solar radiation
 iret=nf90_put_att(netCDFid(g),SWRSFC_varID(g), 'long_name','SWRSFC          ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),SWRSFC_varID(g), 'units',    'WATTS/M**2      ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(netCDFid(g),SWRSFC_varID(g), 'var_desc', 'solar rad reaching sfc (short-wave)                                             ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 
 ! assign GLOBAL attributes
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'EXEC_ID', 'neco...                                                                  ...ocen')
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'FTYPE', 1) ! must be =1 for beis I think...! 1=gridded, 2=custom...?
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'CDATE', current_date() ) !? created in YYYYMMDD format
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'CTIME', current_time())  !? created in HHMMSS format.
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'WDATE', current_date())  !? written in YYYYMMDD
 iret=nf90_put_att(netCDFid(g),NF90_GLOBAL,'WTIME', current_time())  !? written in HHMMSS

 ! starting date and time
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'SDATE', aladin_met(1)%UT_YYYYJJJ) 
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'STIME', aladin_met(1)%UT_HHMISS) 
 ! step in HHMISS
 TimeStep=(met_frequency/60*100+MOD(met_frequency,60))*100
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'TSTEP', (TimeStep)) !HHMMSS 
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'NTHIK', 1) ! External boundary thickness of the grid (i.e., the number of grid cells to extend the grid beyond each boundary
                                                    ! [and around the corners] in a direction towards the exterior) 
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'NCOLS', CAMx_nx(g)) ! nx
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'NROWS', CAMx_ny(g)) ! ny
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'NLAYS', 1) ! layers
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'NVARS', 3) ! 3 variables + TFLAG not included
 ! setting and meaning of projection parameters in aladin2camx.nml
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'GDTYP', Alad_PROJ) 

 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'P_ALP', Alad_PROJ_ALPHA   )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'P_BET', Alad_PROJ_BETA    )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'P_GAM', Alad_PROJ_GAMMA   )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'XCENT', Alad_X_CENT       )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'YCENT', Alad_Y_CENT       )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'XORIG', DNINT(CAMx_SWCor11_x(g)) )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'YORIG', DNINT(CAMx_SWCor11_y(g)) )

! nastavit krok
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'XCELL', CAMx_dx(g) )
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'YCELL', CAMx_dy(g) )

 ! For 3D data: (as this is a 2D dataset, these shouldn't be needed...)
 ! http://niceguy.wustl.edu/mapserver/temp/HTML/BUFFERED.html
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'VGTYP' , 2 )     !gridtype? 2=nonhydrostatic sigma-P !?!? 
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'VGTOP' , (/ 10000D0 /) ) ! model-top, for sigma coord types; does not correspond to reality
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'VGLVLS', (/ 1., 0.98000002 /) ) !real. vertical coordinate values!
   
 gdnam = 'MET_CRO_2D' ! = Time-dependent 2-D cross point meteorology file as per http://www.cmaq-model.org/op_guidance_4.6/html/ch05s07s02.html#ch5_sect7.2.4
 upnam = 'unknownupname' ! could use prefix of gdnam = 'MET_CRO'??
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'GDNAM', trim(gdnam) ) !gridname
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'UPNAM', trim(upnam) )  !
                           !
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'VAR-LIST', "PRSFC           TEMPSFC         SWRSFC          ")

! mozna doplnit puvod dat
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'FILEDESC','data from ...')!//trim(ingribfname))
 iret = nf90_put_att(netCDFid(g), NF90_GLOBAL, 'HISTORY','History descrip.'//&
       &' This file was generated from ALADIN model data to be used in the BEIS model.'//&
       &' Additional info: ...none yet... ')
 !
 ! leave define mode
 iret = nf90_enddef(netCDFid(g))
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)


END SUBROUTINE BEISmet_createHeader

SUBROUTINE BEISmet_putTime(ncID,timeID)
 IMPLICIT NONE
 INTEGER :: ncID, timeID
 
 ! for the TFLAG variable
 INTEGER*4, DIMENSION(:,:,:), ALLOCATABLE:: dates 
 INTEGER :: t, iret

 allocate(dates(2,3,nAladFiles)) ! druhe cislo odpovida poctu promennych v netCDF-ku
 DO t=1,nAladFiles
   dates(1,:,t)=aladin_met(t)%UT_YYYYJJJ ! YYYYJJJ
   dates(2,:,t)=aladin_met(t)%UT_HHMISS  ! HHMMSS
 END DO

 iret = nf90_put_var(ncID, timeID,dates)
   CALL TestStop(iret-nf90_NoErr,'time: '//trim(nf90_strerror(iret)),logFileUnit)

 DEALLOCATE(dates)

END SUBROUTINE BEISmet_putTime


function current_date()
 implicit none
 integer*4 :: current_date
 integer*4, dimension(:), allocatable :: vals
 !return date as an integer in YYYYMMDD format 
 
 allocate(vals(8))
 call date_and_time(values=vals)
 current_date = vals(1)*10000 + vals(2)*100 +vals(3)
 ! YYYY0000 +MM00 + DD
 deallocate(vals)
end function current_date

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
function current_time()
 implicit none
 integer*4 :: current_time
 integer*4, dimension(:), allocatable :: vals
 !return time as an integer in HHMMSS format

 allocate(vals(8))
 call date_and_time(values=vals)
 current_time = vals(5)*10000 + vals(6)*100 +vals(7)
 ! YYYY0000 +MM00 + DD
 deallocate(vals)
end function current_time

end module module_beismet
