MODULE module_global_variables
 ! globalni promenne. Jejich inicializace v module_RUN_INFO.f90
 USE module_standard_types

 IMPLICIT NONE

 integer:: tmp_unit,irec

 TYPE tAladFieldID
     INTEGER :: Param, LevType, Lev, minLev, maxLev
     LOGICAL :: read(100)=.FALSE. ! volim cislo s rezervou presahujici pocet hladin v ALADINu 
 END TYPE tAladFieldID
 !operator pro funkci zjistujici, jestli aktualni nactene pole odpovida pozadovanemu
 INTERFACE OPERATOR (==)
     MODULE PROCEDURE AF_eqs_AFdef
 END INTERFACE OPERATOR (==)
 PRIVATE AF_eqs_AFDef

 TYPE tAVAILABLE_FILES
     ! to be written in CAMx met files; LT_YYYYMMDD needed for log file:
     INTEGER           :: LT_YYJJJ, LT_YYYYMMDD   ! date UTC+timeZone (YYJJJ - year and day of the year)
     REAL(KIND=sp)     :: LT_HHMI    ! time UTC+timeZone (HHMI)

     ! to be written in BEIS met NetCDF file (YYYYJJJ, HHMISS) and in the name of METEO GRIBS (YYYYMMDD, HHMISS):
     INTEGER           :: UT_YYYYMMDD, UT_YYYYJJJ, UT_HHMISS ! UTC dates (YYYYMMDD/YYYYJJJ) and time (HHMISS) correspoding to meteo grib file
     CHARACTER(LEN=200) :: name_f       ! name of the file
 END TYPE tAVAILABLE_FILES

 ! maximum number of grids
 INTEGER, PARAMETER :: ngridnumber_max=3

 CHARACTER(LEN=200) :: logfile
 INTEGER :: logFileUnit, PID=0

 !---------------------------------------------------!
 !                                                   !
 ! RUN OPTIONS CONTROL                               ! 
 !                                                   !
 !  read by info_run() from aladin2camx_control_file !
 !---------------------------------------------------!
   ! file with the aladin2camx_control namelist
   CHARACTER(200) :: aladin2camx_control_file


   ! == biogenic emission related flag ==
   LOGICAL :: BEIS_flag, MEGAN_flag ! if .TRUE. NetDCF files for BEIS and/or MEGAN will be created

   ! nearest gridpoint to the reference point for each grid (relative position in each grid)
   INTEGER, DIMENSION(ngridnumber_max) :: vp_x, vp_y
   !INTEGER :: vp_lon, vp_lat ! grid for which print vertical profile - jeste nutno doprogramovat

   ! === SMOOTHER SWITCH ==
   LOGICAL :: SMOOTHER_SWITCH
   INTEGER :: SMOOTHER_METHOD    ! method of smoothing to be used if smoother_switch=.true. 

   ! === vertical difussivity method ===
   CHARACTER(LEN=4) :: kv_method
   ! === minimum vertical diffusivity ===
   REAL :: kvmin

   ! == optical depth method == 
   CHARACTER(LEN=10) :: cod_method
   CHARACTER(LEN=2)  :: odMetL, odMetM, odMetH

   ! == flag for checking Aladin input data
   LOGICAL :: checkFlag
   ! == critical values of Aladin data
   REAL :: Tsfc_cmin, Q2m_cmin, Psfc_cmin, GEOsfc_cmin, PBL_cmin, sfcROUGH_cmin, &
           T_cmin, geo_cmin, p_cmin, wind_cmax, TKE_cmin, Q_cmin, Rh_cmin, Qs_cmin, Ql_cmin, Qr_cmin, Qi_cmin

   ! missing value code in output fields
   REAL :: missingVal
  
   NAMELIST /aladin2camx_control/  &
       missingVal, &
       BEIS_flag, MEGAN_flag, &
       vp_x, vp_y, &
       SMOOTHER_SWITCH, SMOOTHER_METHOD, &
       kv_method, kvmin, &
       cod_method, odMetL, odMetM, odMetH, &
       checkFlag, Tsfc_cmin, Q2m_cmin, Psfc_cmin, GEOsfc_cmin, PBL_cmin, sfcROUGH_cmin, &
       T_cmin, geo_cmin, p_cmin, wind_cmax, TKE_cmin, Q_cmin, Rh_cmin, Qs_cmin, Ql_cmin, Qr_cmin, Qi_cmin


 !-------------------------------------------------!
 !                                                 !
 ! PARAMETERS OF ALADIN GRIBS                      !
 !                                                 !
 !  read by info_run() from INFO_ALADIN_GRIBS_file !
 !-------------------------------------------------!
   ! file with the aladin_gribs_info namelist
   CHARACTER(200) :: INFO_ALADIN_GRIBS_file

   ! == ALADIN PROJECTION ==
   INTEGER       :: Alad_PROJ        ! MODELS3 grid definitions
   REAL          :: Alad_EarthRadius ! Earth radius
   REAL(KIND=dp) :: Alad_PROJ_ALPHA, Alad_PROJ_BETA, Alad_PROJ_GAMMA, Alad_X_CENT, Alad_Y_CENT

   ! == ALADIN grid ==
   INTEGER       :: Alad_nX, Alad_nY, Alad_nLev 
   REAL(KIND=dp) :: Alad_dx, Alad_dy, Alad_Centr11_X, Alad_Centr11_Y

   ! == ALADIN GRIBs ==
   INTEGER :: Alad_iScanNeg, Alad_jScanPos, Alad_jConsec, Alad_aRowScan
   LOGICAL :: AladLevNumberedFromBottom
   REAL    :: Alad_missingVal
   INTEGER :: Alad_mlLevID, Alad_sfcLevID 

   ! ==  ALADIN FIELDS to be read (their specification in ALADIN GRIB files) ==
   TYPE(tAladFieldID) :: AladField_T2m, AladField_T, &
                         AladField_GEOsfc, AladField_GEO, &
                         AladField_Psfc, AladField_P, &
                         AladField_uWind, AladField_vWind, &
                         AladField_uWind10m, AladField_vWind10m, &
                         AladField_TKE, AladField_Rh, &
                         AladField_Q, AladField_Q2m, &
                         AladField_Ql, AladField_Qi, AladField_Qr, AladField_Qs, &
                         AladField_AccTotPrecip, &
                         AladField_PBL, AladField_sfcROUGH, AladField_AccSolRad, &
                         AladField_sfcSoilMoist, AladField_sfcSoilT

   NAMELIST /aladin_gribs_info/  &
       Alad_PROJ, Alad_EarthRadius, Alad_PROJ_ALPHA, Alad_PROJ_BETA, Alad_PROJ_GAMMA, Alad_X_CENT, Alad_Y_CENT, &
       Alad_nX, Alad_nY, Alad_nLev, Alad_dx, Alad_dy, Alad_Centr11_X, Alad_Centr11_Y, &
       Alad_iScanNeg, Alad_jScanPos, Alad_jConsec, Alad_aRowScan, &
       AladLevNumberedFromBottom, Alad_missingVal, Alad_mlLevID, Alad_sfcLevID, &
       AladField_T, AladField_T2m, AladField_GEOsfc, AladField_GEO, AladField_Psfc, &
       AladField_P, AladField_uWind, AladField_vWind, AladField_TKE, AladField_Rh, AladField_Q, AladField_Q2m, &
       AladField_Ql, AladField_Qi, AladField_Qr, AladField_Qs, AladField_PBL, AladField_sfcROUGH, AladField_AccSolRad, &
       AladField_uWind10m, AladField_vWind10m,  AladField_AccTotPrecip, &
       AladField_sfcSoilMoist, AladField_sfcSoilT 

 INTEGER :: Alad_nVal ! dopocte se jako Alad_nX*Alad_nY 


 !-----------------------------------------!
 !                                         !
 ! CLOCK CONTROL AND I/O FILE NAMES        !
 !                                         !
 !   read by info_run() from INFO_RUN_file !
 !-----------------------------------------!
   ! file with the clock_control, output_files, and input_files namelists
   CHARACTER(200) :: INFO_RUN_file
   ! first and last time layer in CAMx input files
   INTEGER :: begYYYYMMDD, begHHMI, endYYYYMMDD, endHHMI
   ! input frequency of meteorological files in minutes
   INTEGER :: met_frequency
   ! time zone used for CAMx simulation. CET: TimeZone=1
   INTEGER :: TimeZone
   NAMELIST /clock_control/ begYYYYMMDD, begHHMI, endYYYYMMDD, endHHMI, met_frequency, TimeZone

   CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: zp_file, tp_file, uv_file, qa_file
   CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: cr_file,  kv_file
   CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: avgHGT_file, beis_file, megan_file
   NAMELIST /output_files/ zp_file, tp_file, uv_file, qa_file, cr_file, kv_file, avgHGT_file, &
                           beis_file, megan_file

   INTEGER, PARAMETER :: NPrecedAlaMetFiles = 23
   CHARACTER(LEN=200), DIMENSION(-NPrecedAlaMetFiles:200) :: aladin_met_names ! pomocne pole pro nasteni jmen Alad souboru z namelistu
   ! aladin met files:
   ! 1 corresponds to starting hour
   ! 0 is needed to calculate solar radiation from aladin accumulated sol. rad.
   ! -23, ..., -1 are needed to calculate 1-h precipitation from aladin
   ! accumulated tot. precip. field (then used for calculation of the 24-h acc. precip)
  NAMELIST /input_files/ aladin_met_names

 ! number of ALADIN grib files - derived from beginning/end simulation time and met_frequency
 ! if required nAladFiles is greater than dimension of aladin_met, it is necessary to increase the dimension and rebuild the source
 INTEGER :: nAladFiles
 ! information on ALADIN grib files that should be available for the run 
 ! (accord. to begining/end date/time and input frequency given in RUN_INFO_FILE)
 TYPE(tAVAILABLE_FILES), ALLOCATABLE, DIMENSION(:) :: aladin_met

 ! unit numbers and file names for CAMx input files and master resp. nested grids; alloccated and set in aladin2camx_MAIN
 INTEGER,            DIMENSION(ngridnumber_max) :: zp_unit, tp_unit, uv_unit, qa_unit
 INTEGER,            DIMENSION(ngridnumber_max) :: cr_unit,  kv_unit
 INTEGER,            DIMENSION(ngridnumber_max) :: avgHGT_unit


 !----------------------------------------------!
 !                                              !
 ! CAMx GRID INFO                               !
 !                                              !
 !----------------------------------------------!
 INTEGER,       DIMENSION(ngridnumber_max) :: CAMx_nx, CAMx_ny
 REAL(KIND=dp), DIMENSION(ngridnumber_max) :: CAMx_dx, CAMx_dy
 REAL(KIND=dp), DIMENSION(ngridnumber_max) :: CAMx_SWCor11_x, CAMx_SWCor11_y

 !----------------------------------------------!
 !  read by info_run() from INFO_CAMx_GRID_file !
 !----------------------------------------------!
   ! file with the camx_grid_info namelist
   CHARACTER(200) :: INFO_CAMx_GRID_file
   ! number of grids - master plus all nested grids
   INTEGER :: ngridnumber  
   ! CAMx grid parameters relative to (NWP) ALADIN grid.
   ! For nested grids must include buffer cells !!! 
   ! CAMx_grid_step ... sets the resolution or cell size of the CAMx grid relative to NWP (ALADIN) grid.
   INTEGER, DIMENSION(ngridnumber_max) :: CAMx_grid_xbeg, CAMx_grid_xend, &
                                          CAMx_grid_ybeg, CAMx_grid_yend, &
                                          CAMx_grid_step

   ! == CAMx vertical grid structure ==
   INTEGER :: Alad_maxLev ! number of ALADIN levels (counted from bottom) to be used for calculating CAMx levels
   INTEGER :: CAMx_nLev   ! number of CAMx levels to which ALAD_maxLev are reduced
   INTEGER :: CAMx_LevDef(70,2)=-99 ! i-th CAMx level includes CAMx_levDef(i,1), ..., CAMx_levDef(i,2) ALADIN levels

   NAMELIST /camx_grid_info/  &
     ngridnumber, &
     CAMx_grid_xbeg, CAMx_grid_ybeg, CAMx_grid_xend, CAMx_grid_yend, CAMx_grid_step, &
     Alad_maxLev, CAMx_nLev, CAMx_LevDef





 ! emiheader.F90
 CHARACTER(LEN=4) :: mspec(10,4)
 INTEGER, SAVE    :: nspec=4

 data mspec(:,1) /'T','_','l','e','v','e','l','1','',''/
 data mspec(:,2) /'T','_','g','r','o','u','n','d','',''/
 data mspec(:,3) /'S','W','D','O','W','N','','','',''/
 data mspec(:,4) /'S','N','O','W','C','V','R','','',''/


 ! promenne, do kterych budou nactena pole z ALADINovskych GRIBu
 !  - jednotlive dimenze: (i,j,k) (x,y,z) == (zem. delka, zem. sirka, vyska)
 !  - i roste na vychod; j roste na sever; k roste s vyskou
 REAL(KIND=sp), ALLOCATABLE, DIMENSION(:,:,:), TARGET :: Alad_Tsfc, Alad_T, Alad_GEOsfc, Alad_GEO, &
              & Alad_Psfc, Alad_P, Alad_uWind, Alad_vWind, Alad_TKE, &
              & Alad_Rh, Alad_Q2m, Alad_Q, Alad_Ql, Alad_Qi, Alad_Qr, Alad_Qs, &
              & Alad_PBL, Alad_sfcROUGH, Alad_AccSolRad, Alad_SolRad, &
              & Alad_T2m, Alad_totPrecip_1h, Alad_AccTotPrecip, Alad_totPrecip_acc24, &
              & Alad_uWind10m, Alad_vWind10m, Alad_sfcSoilMoist, Alad_sfcSoilT

 ! CAMx_avgLevHgt(lev,grid) ... average height [mAGL] of CAMx level for each grid
 REAL(KIND=sp), ALLOCATABLE, DIMENSION(:,:) :: CAMx_avgLevHgt

 CHARACTER(LEN=200) :: errmsg ! character for error messages


 CONTAINS
 ! = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

 SUBROUTINE init_AladField()
   IMPLICIT NONE

   AladField_T%read=.FALSE.
   AladField_T2m%read=.FALSE.
   AladField_GEOsfc%read=.FALSE.
   AladField_GEO%read=.FALSE.
   AladField_Psfc%read=.FALSE.
   AladField_P%read=.FALSE.
   AladField_uWind%read=.FALSE.
   AladField_vWind%read=.FALSE.
   AladField_TKE%read=.FALSE.
   AladField_Rh%read=.FALSE.
   AladField_Q%read=.FALSE.
   AladField_Q2m%read=.FALSE.
   AladField_Ql%read=.FALSE.
   AladField_Qi%read=.FALSE.
   AladField_Qr%read=.FALSE.
   AladField_Qs%read=.FALSE.
   AladField_PBL%read=.FALSE.
   AladField_sfcROUGH%read=.FALSE.
   AladField_AccSolRad%read=.FALSE.
   AladField_uWind10m%read=.FALSE.
   AladField_vWind10m%read=.FALSE.
   AladField_AccTotPrecip%read=.FALSE.
   AladField_sfcSoilMoist%read=.FALSE.
   AladField_sfcSoilT%read=.FALSE.
 END SUBROUTINE init_AladField


 SUBROUTINE alloc_Alad()
   IMPLICIT NONE
   INTEGER :: istat

   ! Allocating Aladin Field for [Alad_nx, Alad_ny, Alad_maxLev]

   ! == 2D ===
   ALLOCATE(Alad_Tsfc(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Tsfc')
   ALLOCATE(Alad_T2m(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_T2m')
   ALLOCATE(Alad_Q2m(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Q2m')
   ALLOCATE(Alad_Psfc(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Psfc')
   ALLOCATE(Alad_GEOsfc(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_GEOsfc')
   ALLOCATE(Alad_PBL(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_PBL')
   ALLOCATE(Alad_sfcROUGH(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_sfcROUGH')
   ALLOCATE(Alad_AccSolRad(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_AccSolRad')
     Alad_AccSolRad=0.
   ALLOCATE(Alad_SolRad(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_SolRad')
     Alad_SolRad=0.
   ALLOCATE(Alad_uWind10m(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_uWind10m')
   ALLOCATE(Alad_vWind10m(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_vWind10m')
   ! in case of Alad_totPrecip_1h 3rd dimension represents time variable:
   ! Alad_totPrecip_1h(:,:,1)   holds 1-h precipitation for the most recent hour
   ! Alad_totPrecip_1h(:,:,-22) holds 1-h precipitation before 23 hours
   ALLOCATE(Alad_totPrecip_1h(Alad_nx,Alad_ny,-22:1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_totPrecip_1h')
   ALLOCATE(Alad_AccTotPrecip(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_AccTotPrecip')
   ALLOCATE(Alad_totPrecip_acc24(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_totPrecip_acc24')
   ALLOCATE(Alad_sfcSoilMoist(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_scfSoilMoist')
   ALLOCATE(Alad_sfcSoilT(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_sfcSoilT')

   ! == 3D ===
   ALLOCATE(Alad_T(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_T')
   ALLOCATE(Alad_geo(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_geo')
   ALLOCATE(Alad_p(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_p')
   ALLOCATE(Alad_uWind(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_uWind')
   ALLOCATE(Alad_vWind(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_vWind')
   ALLOCATE(Alad_TKE(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_TKE')
   ALLOCATE(Alad_Rh(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Rh')
   ALLOCATE(Alad_Q(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Q')
   ALLOCATE(Alad_Ql(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Ql')
   ALLOCATE(Alad_Qi(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Qi')
   ALLOCATE(Alad_Qr(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Qr')
   ALLOCATE(Alad_Qs(Alad_nX,Alad_nY,Alad_maxLev), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Qs')

   ALLOCATE(CAMx_avgLevHgt(Alad_maxLev,ngridnumber), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for CAMx_avgLevHgt')
     CAMx_avgLevHgt=0.
 END SUBROUTINE alloc_Alad
 
 
 SUBROUTINE dealloc_Alad()
   IMPLICIT NONE
   INTEGER :: istat

   ! == 2D ===
   IF (ALLOCATED(Alad_Q2m)) DEALLOCATE(Alad_Q2m,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Q2m')
   IF (ALLOCATED(Alad_Tsfc))  DEALLOCATE(Alad_Tsfc,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Tsfc')
   IF (ALLOCATED(Alad_T2m))  DEALLOCATE(Alad_T2m,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_T2m')
   IF (ALLOCATED(Alad_Psfc)) DEALLOCATE(Alad_Psfc,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Psfc')
   IF (ALLOCATED(Alad_GEOsfc)) DEALLOCATE(Alad_GEOsfc,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_GEOsfc')
   IF (ALLOCATED(Alad_PBL)) DEALLOCATE(Alad_PBL,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_PBL')
   IF (ALLOCATED(Alad_sfcROUGH)) DEALLOCATE(Alad_sfcROUGH,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_sfcROUGH')
   IF (ALLOCATED(Alad_AccSolRad)) DEALLOCATE(Alad_AccSolRad,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_AccSolRad')
   IF (ALLOCATED(Alad_SolRad)) DEALLOCATE(Alad_SolRad,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_SolRad')
   IF (ALLOCATED(Alad_uWind10m))  DEALLOCATE(Alad_uWind10m,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_uWind10m')
   IF (ALLOCATED(Alad_vWind10m))  DEALLOCATE(Alad_vWind10m,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_vWind10m')
   IF (ALLOCATED(Alad_totPrecip_1h))  DEALLOCATE(Alad_totPrecip_1h,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_totPrecip_1h')
   IF (ALLOCATED(Alad_AccTotPrecip))  DEALLOCATE(Alad_AccTotPrecip,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_AccTotPrecip')
   IF (ALLOCATED(Alad_totPrecip_acc24))  DEALLOCATE(Alad_totPrecip_acc24,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_totPrecip_acc24')
   IF (ALLOCATED(Alad_sfcSoilMoist))  DEALLOCATE(Alad_sfcSoilMoist,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_sfcSoilMoist')
   IF (ALLOCATED(Alad_sfcSoilT))  DEALLOCATE(Alad_sfcSoilT,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_sfcSoilT')

   ! == 3D ===
   IF (ALLOCATED(Alad_T)) DEALLOCATE(Alad_T,       STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_T')
   IF (ALLOCATED(Alad_geo)) DEALLOCATE (Alad_geo,   STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_geo')
   IF (ALLOCATED(Alad_p)) DEALLOCATE(Alad_p,     STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_p')
   IF (ALLOCATED(Alad_uWind)) DEALLOCATE(Alad_uWind, STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_uWind')
   IF (ALLOCATED(Alad_vWind)) DEALLOCATE(Alad_vWind, STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_vWind')
   IF (ALLOCATED(Alad_TKE)) DEALLOCATE(Alad_TKE, STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_TKE')
   IF (ALLOCATED(Alad_Rh)) DEALLOCATE(Alad_Rh,       STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Rh')
   IF (ALLOCATED(Alad_Q)) DEALLOCATE(Alad_Q,       STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Q')
   IF (ALLOCATED(Alad_Ql)) DEALLOCATE(Alad_Ql,      STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Ql')
   IF (ALLOCATED(Alad_Qi)) DEALLOCATE(Alad_Qi,      STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Qi')
   IF (ALLOCATED(Alad_Qr)) DEALLOCATE(Alad_Qr,      STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Qr')
   IF (ALLOCATED(Alad_Qs)) DEALLOCATE(Alad_Qs,      STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Qs')

   IF (ALLOCATED(CAMx_avgLevHgt)) DEALLOCATE(CAMx_avgLevHgt, STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for CAMx_avgLevHgt')

 END SUBROUTINE dealloc_Alad

 LOGICAL  FUNCTION AF_eqs_AFdef(f,fDef)
 ! funkce zjistujici, jestli aktualni nactene pole odpovida pozadovanemu
     IMPLICIT NONE
     TYPE(tAladFieldID), INTENT(IN) :: f,fDef
     AF_eqs_AFdef = .TRUE.
     IF (f%Param   /= fDef%Param  ) THEN; AF_eqs_AFdef = .FALSE.; RETURN; END IF
     IF (f%LevType /= fDef%LevType) THEN; AF_eqs_AFdef = .FALSE.; RETURN; END IF
     IF (f%levType.EQ.Alad_sfcLevID ) THEN 
         IF( f%lev/=fDef%minLev) AF_eqs_AFdef = .FALSE.
     ELSEIF (f%levType.EQ.Alad_mlLevID) THEN 
         IF ( f%lev<1 .OR. f%Lev>Alad_maxLev) AF_eqs_AFdef = .FALSE.
     END IF
 END FUNCTION AF_eqs_AFdef

  INTEGER FUNCTION getFreeUnitNo()
  ! returns smallest number that is not associated with an opened file
    IMPLICIT NONE
    INTEGER, PARAMETER :: maxUnitNo=100
    INTEGER :: i
    LOGICAL :: LAnswer
    DO i=1, maxUnitNo
        INQUIRE(UNIT=i, OPENED=LAnswer)
        IF (.NOT.LAnswer) THEN
            getFreeUnitNo=i
            RETURN
        END IF
    END DO
  END FUNCTION getFreeUnitNo

  SUBROUTINE TestStop(istat,message,ounit)
  ! if istat/=0, writes message to standard output and stops program
    IMPLICIT NONE
    INTEGER,          INTENT(IN   )           :: istat
    CHARACTER(LEN=*), INTENT(IN   )           :: message
    INTEGER,          INTENT(IN   ), OPTIONAL :: ounit

    IF (istat /= 0) THEN
        IF (PRESENT(ounit)) THEN
            WRITE(ounit, *) message
        ELSE
            WRITE(*, *) message
        END IF
        CLOSE(logFileUnit)
        STOP
    END IF
  END SUBROUTINE TestStop

  SUBROUTINE if_not_add_char(text,znak)
  ! if "znak" is not on the last position of string "text", subroutine "if_not_ad_char" adds it.
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: text
    CHARACTER(LEN=1), INTENT(IN   ) :: znak
    INTEGER :: pozice, delka

    delka  = LEN_TRIM(text)
    pozice = SCAN(text,znak,BACK=.TRUE.)
    IF (delka /= pozice) text=TRIM(text)//znak
  END SUBROUTINE if_not_add_char


 FUNCTION juldate(YYYYMMDD,HHMISS)
 ! USE module_standard_types
 
  IMPLICIT NONE
  REAL(KIND=dp)       :: JULDATE
  INTEGER, INTENT(IN) :: YYYYMMDD,HHMISS
 
  ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
  !                                                                             *
  !     Calculates the Julian date                                              *
  !       Julian date (JD) is the interval of time in days and                  *
  !       fractions of a day, since January 1, 4713 BC Greenwich noon           *
  !                                                                             *
  !     AUTHOR: Andreas Stohl (15 October 1993)                                 *
  !                                                                             *
  !     Variables:                                                              *
  !     DD             Day                                                      *
  !     HH             Hour                                                     *
  !     HHMISS         Hour, minute + second                                    *
  !     JA,JM,JY       help variables                                           *
  !     JULDATE        Julian Date                                              *
  !     JULDAY         help variable                                            *
  !     MI             Minute                                                   *
  !     MM             Month                                                    *
  !     SS             Second                                                   *
  !     YYYY           Year                                                     *
  !     YYYYMMDDHH     Date and Time                                            *
  !                                                                             *
  !     Constants:                                                              *
  !     IGREG          help constant                                            *
  !                                                                             *
  ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
  
  INTEGER :: YYYY,MM,DD,HH,MI,SS
  INTEGER :: JULDAY,JY,JM,JA,IGREG
  PARAMETER (IGREG=15+31*(10+12*1582))
  
  YYYY=YYYYMMDD/10000
  MM=(YYYYMMDD-10000*YYYY)/100
  DD=YYYYMMDD-10000*YYYY-100*MM
  HH=HHMISS/10000
  MI=(HHMISS-10000*HH)/100
  SS=HHMISS-10000*HH-100*MI
  
  IF (YYYY.EQ.0) STOP 'There is no Year Zero.'
  IF (YYYY.LT.0) YYYY=YYYY+1
  IF (MM.GT.2) THEN
    JY=YYYY
    JM=MM+1
  ELSE
    JY=YYYY-1
    JM=MM+13
  ENDIF
  JULDAY=INT(365.25*JY)+INT(30.6001*JM)+DD+1720995
  IF (DD+31*(MM+12*YYYY).GE.IGREG) THEN
    JA=INT(0.01*JY)
    JULDAY=JULDAY+2-JA+INT(0.25*JA)
  ENDIF
  
  JULDATE=DBLE(REAL(JULDAY)) + DBLE(REAL(HH)/24.) + DBLE(REAL(MI)/1440.) + DBLE(REAL(SS)/86400.)
  
 END FUNCTION juldate
 
 
 
 
 
 
 FUNCTION julday(dummydate)
 ! USE module_standard_types
 
  IMPLICIT NONE
  INTEGER            :: julday
  INTEGER,INTENT(IN) :: dummydate ! YYYYMMDD
 
  !-----------------------------------------------------------
  ! EN: calculates order of day from the begining of the year |
  ! CZ: spocte poradi dne v roce                              |
  !-----------------------------------------------------------
 
  INTEGER :: ref_year 
  REAL(KIND=dp) :: date_act,date_ref
  !REAL(KIND=dp) :: juldate
  CHARACTER*8 :: chdate
 
 
  write(chdate,'(I8)') dummydate 
  chdate(1:8)=chdate(1:4)//'0101'
 
  read(chdate(1:8),'(I8)') ref_year 
 
  date_ref=juldate(ref_year,000000)
  date_act=juldate(dummydate,000000)
 
  julday=date_act-date_ref+1
 
  RETURN
 END FUNCTION julday

END MODULE module_global_variables
