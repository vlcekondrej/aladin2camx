module module_ioapi
USE module_global_variables
USE netcdf

 ! IDs for netCDF files
 INTEGER(4), DIMENSION(ngridnumber_max) :: BEIS_netCDFid, MEGAN_netCDFid

 ! variable ids
 INTEGER(4), DIMENSION(ngridnumber_max) :: BEIS_PRSFC_varID, BEIS_TEMPSFC_varID, BEIS_SWRSFC_varID, BEIS_TFLAG_varID

 INTEGER(4), DIMENSION(ngridnumber_max) :: MEGAN_TFLAG_varID, MEGAN_TEMP2_varID, &
             MEGAN_PRES_varID, MEGAN_QV_varID, MEGAN_WINDSPD_varID, &
             MEGAN_RAIN_ACC24_varID, MEGAN_PREC_ADJ_varID, MEGAN_PAR_varID, &
             MEGAN_SOIM1_varID, MEGAN_SOIT1_varID, MEGAN_SLTYP_varID

 INTEGER(4), PARAMETER :: BEIS_nvar=3, MEGAN_nvar=10

! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

CONTAINS

SUBROUTINE BEISmet_createHeader(g, ncfname)
 !
 ! creates NetCDF file for BEIS meteorology and place it in a data mode.
 !
 IMPLICIT NONE 
 INTEGER,          INTENT(IN   ) :: g ! grid number
 CHARACTER(LEN=*), INTENT(IN   ) :: ncfname

 INTEGER, PARAMETER :: nlay=1 ! number of vertical layers

 ! error
 INTEGER(4) :: iret

 ! dimension ids
 INTEGER(4), DIMENSION(ngridnumber_max) :: TSTEP_dimID, &
             DATE_TIME_dimID, LAY_dimID, VAR_dimID, &
             ROW_dimID, COL_dimID

 ! variable shapes
 INTEGER(4), DIMENSION(3) :: TFLAG_dims   ! tSTEP,DATE,DATEMINUSTIME
 INTEGER(4), DIMENSION(4) :: VAR_dims   ! meteorological variables rank

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
 iret = nf90_create(ncfname, NF90_CLOBBER, BEIS_netCDFid(g))
 CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! define dimensions
 iret = nf90_def_dim(BEIS_netCDFid(g), 'TSTEP'    , NF90_UNLIMITED , TSTEP_dimID(g)) 
 iret = nf90_def_dim(BEIS_netCDFid(g), 'DATE-TIME', 2              , DATE_TIME_dimID(g)) ! this means what??
 iret = nf90_def_dim(BEIS_netCDFid(g), 'LAY'      , nlay           , LAY_dimID(g)) ! n LAYers
 iret = nf90_def_dim(BEIS_netCDFid(g), 'VAR'      , BEIS_nvar      , VAR_dimID(g)) ! temp, press, SWR + time
 iret = nf90_def_dim(BEIS_netCDFid(g), 'ROW'      , CAMx_ny(g)     , ROW_dimID(g))
 iret = nf90_def_dim(BEIS_netCDFid(g), 'COL'      , CAMx_nx(g)     , COL_dimID(g))
 
 ! define variables
 TFLAG_dims(3) = TSTEP_dimID(g) ! TSTEPS 
 TFLAG_dims(2) = VAR_dimID(g) ! VARS
 TFLAG_dims(1) = DATE_TIME_dimID(g) ! dates
 iret = nf90_def_var(BEIS_netCDFid(g), 'TFLAG           ', NF90_INT , TFLAG_dims , BEIS_TFLAG_varID(g))

 VAR_dims(4) = TSTEP_dimID(g)
 VAR_dims(3) = LAY_dimID(g)
 VAR_dims(2) = ROW_dimID(g)
 VAR_dims(1) = COL_dimID(g)
 iret = nf90_def_var(BEIS_netCDFid(g), 'PRSFC           ', NF90_REAL, VAR_dims, BEIS_PRSFC_varID(g))
 iret = nf90_def_var(BEIS_netCDFid(g), 'TEMPSFC         ', NF90_REAL, VAR_dims, BEIS_TEMPSFC_varID(g))
 iret = nf90_def_var(BEIS_netCDFid(g), 'SWRSFC          ', NF90_REAL, VAR_dims, BEIS_SWRSFC_varID(g))

 ! assign VARIABLE attributes
 ! TIME
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TFLAG_varID(g),  'units',    '<YYYYDDD,HHMMSS>')  
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TFLAG_varID(g),  'long_name','TFLAG           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TFLAG_varID(g),  'var_desc', 'Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS                                ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Pressure
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_PRSFC_varID(g),  'long_name','PRSFC           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_PRSFC_varID(g),  'units',    'Pascal (Pa)     ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_PRSFC_varID(g),  'var_desc', 'surface pressure                                                                ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Temperature
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TEMPSFC_varID(g),'long_name','TEMP surface    ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TEMPSFC_varID(g),'units',    'Kelvin (K)      ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_TEMPSFC_varID(g),'var_desc', 'air temperature at lowest model level                                           ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! Solar radiation
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_SWRSFC_varID(g), 'long_name','SWRSFC          ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_SWRSFC_varID(g), 'units',    'WATTS/M**2      ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 iret=nf90_put_att(BEIS_netCDFid(g),BEIS_SWRSFC_varID(g), 'var_desc', 'solar rad reaching sfc (short-wave)                                             ')
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)
 
 ! assign GLOBAL attributes
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'EXEC_ID', 'neco...                                                                  ...ocen')
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'FTYPE', 1) ! must be =1 for beis I think...! 1=gridded, 2=custom...?
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'CDATE', current_date() ) !? created in YYYYMMDD format
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'CTIME', current_time())  !? created in HHMMSS format.
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'WDATE', current_date())  !? written in YYYYMMDD
 iret=nf90_put_att(BEIS_netCDFid(g),NF90_GLOBAL,'WTIME', current_time())  !? written in HHMMSS

 ! starting date and time
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'SDATE', aladin_met(1)%UT_YYYYJJJ) 
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'STIME', aladin_met(1)%UT_HHMISS) 

 ! step in HHMISS
 TimeStep=(met_frequency/60*100+MOD(met_frequency,60))*100
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'TSTEP', (TimeStep))
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'NTHIK', 1)

 ! External boundary thickness of the grid (i.e., the number of grid cells to extend 
 ! the grid beyond each boundary [and around the corners] in a direction towards the exterior
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'NCOLS', CAMx_nx(g)) ! nx
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'NROWS', CAMx_ny(g)) ! ny
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'NLAYS', nlay) ! layers
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'NVARS', BEIS_nvar) ! 3 variables + TFLAG not included

 ! setting and meaning of projection parameters in aladin2camx.nml
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'GDTYP', Alad_PROJ) 
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'P_ALP', Alad_PROJ_ALPHA   )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'P_BET', Alad_PROJ_BETA    )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'P_GAM', Alad_PROJ_GAMMA   )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'XCENT', Alad_X_CENT       )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'YCENT', Alad_Y_CENT       )

 ! grid parameters
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'XORIG', DNINT(CAMx_SWCor11_x(g)) )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'YORIG', DNINT(CAMx_SWCor11_y(g)) )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'XCELL', CAMx_dx(g) )
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'YCELL', CAMx_dy(g) )

 ! For 3D data: (as this is a 2D dataset, these shouldn't be needed...)
 ! http://niceguy.wustl.edu/mapserver/temp/HTML/BUFFERED.html
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'VGTYP' , 2 )     !gridtype? 2=nonhydrostatic sigma-P !?!? 
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'VGTOP' , (/ 10000D0 /) ) ! model-top, for sigma coord types; does not correspond to reality
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'VGLVLS', (/ 1., 0.98000002 /) ) !real. vertical coordinate values!
   
 gdnam = 'MET_CRO_2D' ! = Time-dependent 2-D cross point meteorology file as per http://www.cmaq-model.org/op_guidance_4.6/html/ch05s07s02.html#ch5_sect7.2.4
 upnam = 'unknownupname' ! could use prefix of gdnam = 'MET_CRO'??
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'GDNAM', trim(gdnam) ) !gridname
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'UPNAM', trim(upnam) )  !
                           !
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'VAR-LIST', "PRSFC           TEMPSFC         SWRSFC          ")

! mozna doplnit puvod dat
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'FILEDESC','data from ...')!//trim(ingribfname))
 iret = nf90_put_att(BEIS_netCDFid(g), NF90_GLOBAL, 'HISTORY','History descrip.'//&
       &' This file was generated from ALADIN model data to be used in the BEIS model.'//&
       &' Additional info: ...none yet... ')
 !
 ! leave define mode
 iret = nf90_enddef(BEIS_netCDFid(g))
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)


END SUBROUTINE BEISmet_createHeader


! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

SUBROUTINE MEGANmet_createHeader(g, ncfname)
 !
 ! creates NetCDF file for MEGAN meteorology and place it in a data mode.
 !
 IMPLICIT NONE 
 INTEGER,          INTENT(IN   ) :: g ! grid number
 CHARACTER(LEN=*), INTENT(IN   ) :: ncfname

 INTEGER, PARAMETER :: nlay=1 ! number of vertical layers

 ! error
 INTEGER(4) :: iret

 ! dimension ids
 INTEGER(4), DIMENSION(ngridnumber_max) :: TSTEP_dimID, &
             DATE_TIME_dimID, LAY_dimID, VAR_dimID, &
             ROW_dimID, COL_dimID

 ! variable shapes
 INTEGER(4), DIMENSION(3) :: TFLAG_dims   ! tSTEP,DATE,DATEMINUSTIME
 INTEGER(4), DIMENSION(4) :: VAR_dims   ! meteorological variables rank
 INTEGER(4), DIMENSION(4) :: TEMPSFC_dims ! TEMPSFC_rank
 INTEGER(4), DIMENSION(4) :: SWRSFC_dims  ! SWRSFC_rank

 ! global attribs
 CHARACTER(LEN=200) :: gdnam, upnam

 INTEGER(4) :: YYYYJJJ, HHMMSS ! a date with year and julian day; time
 INTEGER(4) :: t ! index
 INTEGER(4) :: TimeStep


 IF (g>ngridnumber_max) CALL TestStop(-1,'MEGANmet_createHeader: grid number cannot be '&
        &'greater then ngridnumber_max',logFileUnit)

 ! enter define mode & write the header info
 ! -----------------------------------------

! print*, "Entering netCDF define mode. Lib version ",nf90_inq_libvers()
 iret = nf90_create(ncfname, NF90_CLOBBER, MEGAN_netCDFid(g))
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)

 ! define dimensions
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'TSTEP'    , NF90_UNLIMITED , TSTEP_dimID(g)) 
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'DATE-TIME', 2              , DATE_TIME_dimID(g)) ! 
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'LAY'      , nlay           , LAY_dimID(g)) ! n LAYers
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'VAR'      , MEGAN_nvar     , VAR_dimID(g)) ! all avriables except time
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'ROW'      , CAMx_ny(g)     , ROW_dimID(g))
 iret = nf90_def_dim(MEGAN_netCDFid(g), 'COL'      , CAMx_nx(g)     , COL_dimID(g))
 
 ! define variables
 TFLAG_dims(3) = TSTEP_dimID(g) ! TSTEPS 
 TFLAG_dims(2) = VAR_dimID(g) ! VARS
 TFLAG_dims(1) = DATE_TIME_dimID(g) ! dates
 iret = nf90_def_var(MEGAN_netCDFid(g), 'TFLAG          ', NF90_INT ,  TFLAG_dims, MEGAN_TFLAG_varID(g))

 VAR_dims(4) = TSTEP_dimID(g)
 VAR_dims(3) = LAY_dimID(g)
 VAR_dims(2) = ROW_dimID(g)
 VAR_dims(1) = COL_dimID(g)
 iret = nf90_def_var(MEGAN_netCDFid(g), 'SOIM1          ', NF90_REAL, VAR_dims, MEGAN_SOIM1_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'SOIT1          ', NF90_REAL, VAR_dims, MEGAN_SOIT1_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'SLTYP          ', NF90_REAL, VAR_dims, MEGAN_SLTYP_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'TEMP2          ', NF90_REAL, VAR_dims, MEGAN_TEMP2_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'PRES           ', NF90_REAL, VAR_dims, MEGAN_PRES_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'QV             ', NF90_REAL, VAR_dims, MEGAN_QV_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'WINDSPD        ', NF90_REAL, VAR_dims, MEGAN_WINDSPD_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'RAIN_ACC24     ', NF90_REAL, VAR_dims, MEGAN_RAIN_ACC24_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'PREC_ADJ       ', NF90_REAL, VAR_dims, MEGAN_PREC_ADJ_varID(g))
 iret = nf90_def_var(MEGAN_netCDFid(g), 'PAR            ', NF90_REAL, VAR_dims, MEGAN_PAR_varID(g))


 ! assign VARIABLE attributes
 ! --------------------------
 ! TIME
 iret=nf90_put_att(MEGAN_netCDFid(g),MEGAN_TFLAG_varID(g), 'units'     ,'<YYYYDDD,HHMMSS>')  
 iret=nf90_put_att(MEGAN_netCDFid(g),MEGAN_TFLAG_varID(g), 'long_name' ,'TFLAG           ')
 iret=nf90_put_att(MEGAN_netCDFid(g),MEGAN_TFLAG_varID(g), 'var_desc'  ,&
  '                                Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS') ! trailing blanks are removed and MEGAN as problem with var_desc string length

 ! soil moisture
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIM1_varID(g), 'long_name','SOIM1           ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIM1_varID(g), 'units'    ,'M**3/M**3')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIM1_varID(g), 'var_desc' ,&
  '                                            volumetric soil moisture in top 1 cm')

 ! soil temperature 
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIT1_varID(g), 'long_name','SOIT1           ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIT1_varID(g), 'units'    ,'K')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SOIT1_varID(g), 'var_desc' ,&
  '                                                    soil temperature in top 1 cm')

 ! soil texture
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SLTYP_varID(g), 'long_name','SLTYP           ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SLTYP_varID(g), 'units'    ,'CATEGORY')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_SLTYP_varID(g), 'var_desc' ,&
  '                                              soil texture type by USDA category')

 ! temperature at 2 m
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_TEMP2_varID(g), 'long_name','TEMP2           ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_TEMP2_varID(g), 'units'    ,'K')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_TEMP2_varID(g), 'var_desc' ,&
  '                                                              temperature at 2 m')

 ! pressure
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PRES_varID(g), 'long_name','PRES             ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PRES_varID(g), 'units'    ,'Pa')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PRES_varID(g), 'var_desc' ,&
  '                                                                surface pressure')

 ! water vapor mixing ratio
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_QV_varID(g), 'long_name'  ,'QV               ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_QV_varID(g), 'units'      ,'KG/KG')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_QV_varID(g), 'var_desc'   ,&
  '                              water vapor mixing ratio M(water vapor)/M(dry air)')

 ! Cell centered Windspeed
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_WINDSPD_varID(g), 'long_name','WINDSPD         ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_WINDSPD_varID(g), 'units'    ,'m/s')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_WINDSPD_varID(g), 'var_desc' ,&
  '                                                         Cell centered windspeed')

 ! 24-hour accumulated rain
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_RAIN_ACC24_varID(g), 'long_name','RAIN_ACC24      ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_RAIN_ACC24_varID(g), 'units'    ,'cm')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_RAIN_ACC24_varID(g), 'var_desc' ,&
  '                              total precipitation accumulated over last 24 hours')

 ! Precipitation adjustment factor
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PREC_ADJ_varID(g), 'long_name','PREC_ADJ        ')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PREC_ADJ_varID(g), 'units'    ,'No dimension')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PREC_ADJ_varID(g), 'var_desc' ,&
  '                                                 Precipitation adjustment factor')

 ! Photosynthetically Active Radiation
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PAR_varID(g), 'long_name','PAR')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PAR_varID(g), 'units'    ,'WATTS/M**2')
 iret=nf90_put_att(MEGAN_netCDFid(g), MEGAN_PAR_varID(g), 'var_desc' ,&
  '    Photosyn. Active Radiation (half of ALADIN surface net short-wave rad. flux)')

 
 ! assign GLOBAL attributes
 ! ------------------------
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'IOAPI_VERSION', 'Unknown ioapi version')
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'EXEC_ID', 'Some exec id')
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'FTYPE', 1) ! must be =1 for beis I think...! 1=gridded, 2=custom...?
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'CDATE', current_date()) !? created in YYYYMMDD format
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'CTIME', current_time()) !? created in HHMMSS format.
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'WDATE', current_date()) !? written in YYYYMMDD
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'WTIME', current_time()) !? written in HHMMSS

 ! starting date and time
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'SDATE', aladin_met(1)%UT_YYYYJJJ) 
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'STIME', aladin_met(1)%UT_HHMISS) 

 ! step in HHMISS
 TimeStep=(met_frequency/60*100+MOD(met_frequency,60))*100
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'TSTEP', (TimeStep))  

 ! External boundary thickness of the grid (i.e., the number of grid cells to extend 
 ! the grid beyond each boundary [and around the corners] in a direction towards the exterior
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'NTHIK', 1)
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'NCOLS', CAMx_nx(g)) ! nx
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'NROWS', CAMx_ny(g)) ! ny
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'NLAYS', nlay) ! layers
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'NVARS', MEGAN_nvar) ! variables (TFLAG not included)

 ! setting and meaning of projection parameters in aladin2camx.nml
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'GDTYP', Alad_PROJ) 
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'P_ALP', Alad_PROJ_ALPHA)
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'P_BET', Alad_PROJ_BETA)
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'P_GAM', Alad_PROJ_GAMMA)
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'XCENT', Alad_X_CENT)
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'YCENT', Alad_Y_CENT)

 ! grid parameters
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'XORIG', DNINT(CAMx_SWCor11_x(g)) )
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'YORIG', DNINT(CAMx_SWCor11_y(g)) )
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'XCELL', CAMx_dx(g) )
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'YCELL', CAMx_dy(g) )

 ! For 3D data: (as this is a 2D dataset, these shouldn't be needed...)
 ! http://niceguy.wustl.edu/mapserver/temp/HTML/BUFFERED.html
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'VGTYP' , 2 ) ! gridtype? 2=nonhydrostatic sigma-P !?!? 
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'VGTOP' , (/ 10000D0 /) ) ! model-top, for sigma coord types; does not correspond to reality
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'VGLVLS', (/ 1., 0.98000002 /) ) !real. vertical coordinate values!
   
 gdnam = 'METCRO_xxx' ! = Time-dependent 2-D cross point meteorology file as per http://www.cmaq-model.org/op_guidance_4.6/html/ch05s07s02.html#ch5_sect7.2.4
 upnam = 'UnknownUpName' ! could use prefix of gdnam = 'MET_CRO'??
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'GDNAM', trim(gdnam) ) !gridname
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'UPNAM', trim(upnam) )  !
                           !
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'VAR-LIST', &
   'SOIM1           SOIT1           SLTYP           TEMP2           PRES            '//&
   'QV              WINDSPD         RAIN_ACC24      PREC_ADJ        PAR             ')

 ! file description
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'FILEDESC',&
       &' This file was generated from ALADIN model data to be used in the MEGAN model.'//&
       &' Additional info: ...none yet... ')
 iret = nf90_put_att(MEGAN_netCDFid(g), NF90_GLOBAL, 'HISTORY','')
 
 ! leave define mode
 ! -----------------
 iret = nf90_enddef(MEGAN_netCDFid(g))
   CALL TestStop(iret-nf90_NoErr,trim(nf90_strerror(iret)),logFileUnit)


END SUBROUTINE MEGANmet_createHeader


! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

SUBROUTINE putTime(ncID,timeID,nvar)
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: ncID, timeID, nvar
 
 ! for the TFLAG variable
 INTEGER*4, DIMENSION(:,:,:), ALLOCATABLE:: dates 
 INTEGER :: t, iret

 allocate(dates(2,nvar,nAladFiles)) ! druhe cislo odpovida poctu promennych v netCDF-ku
 DO t=1,nAladFiles
   dates(1,:,t)=aladin_met(t)%UT_YYYYJJJ ! YYYYJJJ
   dates(2,:,t)=aladin_met(t)%UT_HHMISS  ! HHMMSS
 END DO

 iret = nf90_put_var(ncID, timeID,dates)
   CALL TestStop(iret-nf90_NoErr,'time: '//trim(nf90_strerror(iret)),logFileUnit)

 DEALLOCATE(dates)

END SUBROUTINE putTime


! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

function current_date()
 ! return date as an integer in YYYYMMDD format 
 implicit none
 integer*4 :: current_date
 integer*4, dimension(8) :: vals
 
 call date_and_time(values=vals)
 current_date = vals(1)*10000 + vals(2)*100 +vals(3) ! YYYY0000 +MM00 + DD

end function current_date
 

! * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * - * -

function current_time()
 ! return time as an integer in HHMMSS format
 implicit none
 integer*4 :: current_time
 integer*4, dimension(8) :: vals

 call date_and_time(values=vals)
 current_time = vals(5)*10000 + vals(6)*100 +vals(7) ! YYYY0000 +MM00 + DD

end function current_time

end module module_ioapi
