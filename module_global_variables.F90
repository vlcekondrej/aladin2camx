MODULE module_global_variables
 ! globalni promenne. Jejich inicializace v module_RUN_INFO.f90
 USE module_standard_types
! USE module_utils

 IMPLICIT NONE
integer:: tmp_unit,irec
 TYPE tAladFieldID
     INTEGER :: Param, LevType, Lev, minLev, maxLev
     INTEGER :: read(100) ! volim cislo s erzervou presahujici pocet hladin v ALADINu 
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


 ! == control file ==
 CHARACTER(LEN=200) :: logfile
 INTEGER :: logFileUnit, PID

 !--------------------------------
 ! initialized in aladin2camx.nml |
 !--------------------------------
   ! == biogenic emission related flag ==
   LOGICAL :: BEIS_flag ! if .TRUE. NetDCF files for BEIS will be created

   ! == CAMx vertical grid structure ==
   INTEGER :: Alad_maxLev ! number of ALADIN levels to be read
   INTEGER :: CAMx_nLev   ! number of CAMx levels to which ALAD_maxLev are reduced
   INTEGER :: CAMx_LevDef(60,2) ! i-th CAMx level includes CAMx_levDef(i,1), ..., CAMx_levDef(i,2) ALADIN levels

   INTEGER :: vpx, vpy ! grid for which print vertical profile

   ! == ALADIN grid ==
   INTEGER       :: Alad_nVal, Alad_nX, Alad_nY, Alad_nLev 
   REAL(KIND=dp) :: Alad_dx, Alad_dy, Alad_Centr11_X, Alad_Centr11_Y

   ! == ALADIN GRIBs ==
   INTEGER :: Alad_iScanNeg, Alad_jScanPos, Alad_jConsec, Alad_aRowScan
   LOGICAL :: ConfAladCAMxLevIndexing
   REAL    :: Alad_missingVal
   INTEGER :: Alad_mlLevID, Alad_sfcLevID 

   ! == ALADIN PROJECTION ==
   INTEGER       :: Alad_PROJ        ! MODELS3 grid definitions
   REAL          :: Alad_EarthRadius    ! Earth radius
   REAL(KIND=dp) :: Alad_PROJ_ALPHA, Alad_PROJ_BETA, Alad_PROJ_GAMMA, Alad_X_CENT, Alad_Y_CENT

   ! ==  ALADIN FIELDS to be read ==
   ! their specification in ALADIN GRIB files
   TYPE(tAladFieldID) :: AladField_Tsfc, AladField_T, &
                         AladField_GEOsfc, AladField_GEO, &
                         AladField_Psfc, AladField_P, &
                         AladField_uWind, AladField_vWind, &
                         AladField_TKE, AladField_Rh, &
                         AladField_Q, AladField_Qsfc, &
                         AladField_Ql, AladField_Qi, AladField_Qr, AladField_Qs, &
                         AladField_PBL, AladField_sfcROUGH, AladField_AccSolRad


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
   REAL :: Tsfc_cmin, Qsfc_cmin, Psfc_cmin, GEOsfc_cmin, PBL_cmin, sfcROUGH_cmin, &
           T_cmin, geo_cmin, p_cmin, wind_cmax, TKE_cmin, Q_cmin, Rh_cmin, Qs_cmin, Ql_cmin, Qr_cmin, Qi_cmin

   NAMELIST /aladin2camx_control/  &
       BEIS_flag, &
       Alad_maxLev, CAMx_nLev, CAMx_LevDef, &
       vpx, vpy, &
       Alad_nVal, Alad_nX, Alad_nY, Alad_nLev, Alad_dx, Alad_dy, Alad_Centr11_X, Alad_Centr11_Y, &
       Alad_PROJ, Alad_EarthRadius, Alad_PROJ_ALPHA, Alad_PROJ_BETA, Alad_PROJ_GAMMA, Alad_X_CENT, Alad_Y_CENT, &
       Alad_iScanNeg, Alad_jScanPos, Alad_jConsec, Alad_aRowScan, &
       ConfAladCAMxLevIndexing, Alad_missingVal, Alad_mlLevID, Alad_sfcLevID, &
       AladField_Tsfc, AladField_T, AladField_GEOsfc, AladField_GEO, AladField_Psfc, &
       AladField_P, AladField_uWind, AladField_vWind, AladField_TKE, AladField_Rh, AladField_Q, AladField_Qsfc, &
       AladField_Ql, AladField_Qi, AladField_Qr, AladField_Qs, AladField_PBL, AladField_sfcROUGH, AladField_AccSolRad, &
       SMOOTHER_SWITCH, SMOOTHER_METHOD, & 
       kv_method, kvmin, &
       cod_method, odMetL, odMetM, odMetH, &
       checkFlag, Tsfc_cmin, Qsfc_cmin, Psfc_cmin, GEOsfc_cmin, PBL_cmin, sfcROUGH_cmin, &
       T_cmin, geo_cmin, p_cmin, wind_cmax, TKE_cmin, Q_cmin, Rh_cmin, Qs_cmin, Ql_cmin, Qr_cmin, Qi_cmin, &
       TimeZone


 CHARACTER(LEN=200) :: INFO_GRID_FILE='INFO_GRID' ! ,INFO_RUN_FILE='INFO_RUN'
 !---------------------------------------
 ! read by info_run() from INFO_RUN.nml  |
 !---------------------------------------
   CHARACTER(LEN=200) :: ALAD_GRIB_DIR, CAMX_INP_DIR
   ! first and last time layer in CAMx input files
   INTEGER :: begYYYYMMDD, begHHMI, endYYYYMMDD, endHHMI
   ! input frequency of meteorological files in minutes
   INTEGER :: met_frequency
   ! time zune used for CAMx simulation. CET: TimeZone=1
   INTEGER :: TimeZone
   NAMELIST /info_run/ ALAD_GRIB_DIR, CAMX_INP_DIR, begYYYYMMDD, begHHMI, endYYYYMMDD, endHHMI, met_frequency, TimeZone

   ! number of ALADIN grib files - derived from beginning/end simulation time and met_frequency
   ! if required nAladFiles is greater than dimension of aladin_met, it is necessary to increase the dimension and rebuild the source
   INTEGER :: nAladFiles
   ! information on ALADIN grib files that should be available for the run 
   ! (accord. to begining/end date/time and input frequency given in RUN_INFO_FILE)
   TYPE(tAVAILABLE_FILES), ALLOCATABLE, DIMENSION(:) :: aladin_met

 !---------------------------------------
 ! read by info_run() from INFO_GRID_FILE |
 !---------------------------------------
   ! maximum number of grids
   INTEGER, PARAMETER :: ngridnumber_max=3
   ! number of grids - master plus all nested grids
   INTEGER            :: ngridnumber  
   ! CAMx MASTER grid parameters relative to (NWP) ALADIN grid. 
   ! * CAMx_master2alad expresses the ratio of the CAMx master grid horiz.res. relative to NWP(ALADIN) horiz.res.
   INTEGER            :: CAMx_master2alad, CAMx_master_xbeg, CAMx_master_xend, CAMx_master_ybeg, CAMx_master_yend
   ! Nested grids parameters in accordance with CAMx namelist: 
   ! * indexes and meshing factors are taken relative to CAMx MASTER grid; 
   ! * buffer cells are not included. They will be added authomatically.
   INTEGER, DIMENSION(ngridnumber_max) :: CAMx_nest_xbeg, CAMx_nest_ybeg, CAMx_nest_xend, CAMx_nest_yend 
   INTEGER, DIMENSION(ngridnumber_max) :: CAMx_nest_mesh 
   NAMELIST /info_grid/ ngridnumber, &
     CAMx_master2alad, CAMx_master_xbeg, CAMx_master_xend, CAMx_master_ybeg, CAMx_master_yend, &
     CAMx_nest_mesh,   CAMx_nest_xbeg,   CAMx_nest_ybeg,   CAMx_nest_xend,   CAMx_nest_yend

 ! grid parameters relative to NWP(ALADIN) grid (index 1 corresponds to CAMx master):
 INTEGER,       DIMENSION(ngridnumber_max) :: grid_xbeg, grid_xend, grid_ybeg, grid_yend, grid2alad

 INTEGER,       DIMENSION(ngridnumber_max) :: CAMx_nx, CAMx_ny
 REAL(KIND=dp), DIMENSION(ngridnumber_max) :: CAMx_dx, CAMx_dy
 REAL(KIND=dp), DIMENSION(ngridnumber_max) :: CAMx_SWCor11_x, CAMx_SWCor11_y


 ! unit numbers and file names for CAMx input files and master resp. nested grids; alloccated and set in aladin2camx_MAIN
 INTEGER,            DIMENSION(ngridnumber_max) :: zp_unit, tp_unit, uv_unit, qa_unit
 INTEGER,            DIMENSION(ngridnumber_max) :: cr_unit,  kv_unit
 INTEGER,            DIMENSION(ngridnumber_max) :: avgHGT_unit
 CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: zp_file, tp_file, uv_file, qa_file
 CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: cr_file,  kv_file
 CHARACTER(LEN=200), DIMENSION(ngridnumber_max) :: avgHGT_file, beis_file
 NAMELIST /info_output_files/ zp_file, tp_file, uv_file, qa_file, cr_file, kv_file, avgHGT_file, beis_file

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
              & Alad_Rh, Alad_Qsfc, Alad_Q, Alad_Ql, Alad_Qi, Alad_Qr, Alad_Qs, &
              & Alad_PBL, Alad_sfcROUGH, Alad_AccSolRad, Alad_SolRad

 ! CAMx_avgLevHgt(lev,grid) ... average height [mAGL] of CAMx level for each grid
 REAL(KIND=sp), ALLOCATABLE, DIMENSION(:,:) :: CAMx_avgLevHgt

 CHARACTER(LEN=200) :: errmsg ! character for error messages


 CONTAINS
 ! = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

 SUBROUTINE alloc_Alad()
   IMPLICIT NONE
   INTEGER :: istat

   ! Allocating Aladin Field for [Alad_nx, Alad_ny, Alad_maxLev]

   ! == 2D ===
   ALLOCATE(Alad_Tsfc(Alad_nx,Alad_ny,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Tsfc')
   ALLOCATE(Alad_Qsfc(Alad_nX,Alad_nY,1), STAT=istat)
     CALL TestStop(istat,'__alloc_Alad: allocation ERROR for Alad_Qsfc')
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
   IF (ALLOCATED(Alad_Qsfc)) DEALLOCATE(Alad_Qsfc,  STAT=istat)
       CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Qsfc')
   IF (ALLOCATED(Alad_Tsfc))  DEALLOCATE(Alad_Tsfc,  STAT=istat)
     CALL TestStop(istat,'__dealloc_Alad: deallocation ERROR for Alad_Tsfc')
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
  ! subroutine if "znak" is not on the last position of string "text", subroutine "if_not_ad_char" adds it.
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: text
    CHARACTER(LEN=1), INTENT(IN   ) :: znak
    INTEGER :: pozice, delka

    delka  = LEN_TRIM(text)
    pozice = SCAN(text,znak,BACK=.TRUE.)
    IF (delka /= pozice) text=TRIM(text)//znak
  END SUBROUTINE if_not_add_char

END MODULE module_global_variables


