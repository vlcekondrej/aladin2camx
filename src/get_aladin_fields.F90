SUBROUTINE get_aladin_fields(gribfile, fnumber)

 USE module_global_variables
 USE module_standard_types
 USE grib_api
 
 IMPLICIT NONE

 ! name of the aladin grib file
 CHARACTER(LEN=*), INTENT(IN) :: gribfile
 INTEGER                      :: fnumber

 INTEGER            :: i, startStep ! index; integration step of the NWP model. When startStep=0, then accumulated fields are zero.
 INTEGER            :: istat, ifile, igrib, MesgNo ! error status; grib ID; grib message number 
 TYPE(tAladFieldID) :: AladField
 REAL               :: sigNeg


 ! open ALADIN grib file
 WRITE(logFileUnit,*) ' ** OPENNING ALADIN GRIB FILE: "'//TRIM(gribfile)//'" ....'
 CALL grib_open_file(ifile,gribfile,'r',istat)
   CALL TestStop(istat-GRIB_SUCCESS,'STOP __get_aladin_fields: could not open ALADIN grib file.',logFileUnit)


 ! read first message from grib file
 MesgNo = 1
 CALL grib_new_from_file(ifile,igrib,istat)

 DO WHILE (istat /= GRIB_END_OF_FILE) 
     CALL grib_get(igrib,'indicatorOfParameter',AladField%Param)
     CALL grib_get(igrib,'indicatorOfTypeOfLevel',AladField%LevType)
     CALL grib_get(igrib,'level',AladField%Lev)
     CALL grib_get(igrib,'startStep',startStep)

     ! pokud * se nejedna o prizemni pole a
     !       * cislovani hladin ALADIN-u je opacne nez CAMx-u,
     ! pak je treba obratit poradi hladin.
     IF (AladField%levType==Alad_mlLevID .AND. .NOT.AladLevNumberedFromBottom)&
       AladField%Lev = Alad_nLev - AladField%Lev + 1

     ! == 2D ==
     IF (AladField == AladField_Tsfc) THEN
         CALL read_aladin_data(igrib,Alad_Tsfc,1,             'surface temperature',MesgNo)
         AladField_Tsfc%read(1)=1
     ELSE IF (AladField == AladField_Qsfc) THEN
         CALL read_aladin_data(igrib,Alad_Qsfc,1,             'surface specific humidity',MesgNo)
         AladField_Qsfc%read(1)=1
     ELSE IF (AladField == AladField_GEOsfc) THEN
         CALL read_aladin_data(igrib,Alad_GEOsfc,1,           'surface geopotential',MesgNo)
         AladField_GEOsfc%read(1)=1
     ELSE IF (AladField == AladField_Psfc) THEN
         CALL read_aladin_data(igrib,Alad_Psfc,1,             'surface pressure',MesgNo)
         AladField_Psfc%read(1)=1
     ELSE IF (AladField == AladField_PBL) THEN
         CALL read_aladin_data(igrib,Alad_PBL,1,              'planetary boundary layer height',MesgNo)
         AladField_PBL%read(1)=1
     ELSE IF (AladField == AladField_sfcROUGH) THEN
         CALL read_aladin_data(igrib,Alad_sfcROUGH,1,         'surface roughness',MesgNo)
         AladField_sfcROUGH%read(1)=1
     ELSE IF (AladField == AladField_AccSolRad) THEN
         IF ( fnumber == 0 ) THEN
            ! just want the AccSolRad
            CALL read_aladin_data(igrib,Alad_AccSolRad,1,        'accumulated solar radiation',MesgNo)
         ELSE
           !ensure startStep is not 0
           IF (startStep == 0) CALL TestStop(1,"STOP - cannot get SolRad: startStep is 0.",logFileUnit)
           Alad_SolRad = Alad_AccSolRad ! Alad_SolRad now contains accumulated radiation from the previous step
           CALL read_aladin_data(igrib,Alad_AccSolRad,1,        'accumulated solar radiation',MesgNo)
           Alad_SolRad = (Alad_AccSolRad - Alad_SolRad)/(met_frequency*60.) ! division by length of time interval in seconds - conversion of J/m2 to W/m2
           AladField_AccSolRad%read(1)=1
         END IF 

     ! == 3D ==
     ELSE IF (AladField == AladField_T) THEN
         CALL read_aladin_data(igrib,Alad_T,AladField%Lev,    'tepmerature',MesgNo)
         AladField_T%read(AladField%lev)=1
     ELSE IF (AladField == AladField_geo) THEN
         CALL read_aladin_data(igrib,Alad_geo,AladField%Lev,  'geopotential',MesgNo)
         AladField_GEO%read(AladField%lev)=1
     ELSE IF (AladField == AladField_p) THEN
         CALL read_aladin_data(igrib,Alad_p,AladField%Lev,    'pressure',MesgNo)
         AladField_p%read(AladField%lev)=1
     ELSE IF (AladField == AladField_uWind) THEN
         CALL read_aladin_data(igrib,Alad_uWind,AladField%Lev,'uWind',MesgNo)
         AladField_uWind%read(AladField%lev)=1
     ELSE IF (AladField == AladField_vWind) THEN
         CALL read_aladin_data(igrib,Alad_vWind,AladField%Lev,'vWind',MesgNo)
         AladField_vWind%read(AladField%lev)=1
     ELSE IF (AladField == AladField_TKE) THEN
         CALL read_aladin_data(igrib,Alad_TKE,AladField%Lev,  'TKE',MesgNo)
         AladField_TKE%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Rh) THEN
         CALL read_aladin_data(igrib,Alad_Rh,AladField%Lev,   'relative humidity',MesgNo)
         AladField_Rh%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Q) THEN
         CALL read_aladin_data(igrib,Alad_Q,AladField%Lev,    'specific humidity',MesgNo)
         AladField_Q%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Ql) THEN
         CALL read_aladin_data(igrib,Alad_Ql,AladField%Lev,   'atmospheric liquid water',MesgNo)
         AladField_Ql%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Qi) THEN
         CALL read_aladin_data(igrib,Alad_Qi,AladField%Lev,   'atmospheric solid water',MesgNo)
         AladField_Qi%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Qr) THEN
         CALL read_aladin_data(igrib,Alad_Qr,AladField%Lev,   'atmospheric rain',MesgNo)
         AladField_Qr%read(AladField%lev)=1
     ELSE IF (AladField == AladField_Qs) THEN
         CALL read_aladin_data(igrib,Alad_Qs,AladField%Lev,   'atmospheric snow',MesgNo)
         AladField_Qs%read(AladField%lev)=1
     END IF

     ! read new message
     CALL grib_release(igrib)
     MesgNo = MesgNo + 1
     CALL grib_new_from_file(ifile,igrib,istat)
 END DO


 ! Check, if all 2D fields have been read.
 IF (AladField_Tsfc%read(1)      .EQ. 0) CALL TestStop(1,'STOP - surface temperature was not read',logFileUnit)
 IF (AladField_Qsfc%read(1)      .EQ. 0) CALL TestStop(1,'STOP - surface specific humidity was not read',logFileUnit)
 IF (AladField_GEOsfc%read(1)    .EQ. 0) CALL TestStop(1,'STOP - surface geopotential was not read',logFileUnit)
 IF (AladField_Psfc%read(1)      .EQ. 0) CALL TestStop(1,'STOP - surface pressure was not read',logFileUnit)
 IF (AladField_PBL%read(1)       .EQ. 0) CALL TestStop(1,'STOP - planetary boundary layer height was not read',logFileUnit)
 IF (AladField_sfcROUGH%read(1)  .EQ. 0) CALL TestStop(1,'STOP - surface roughness was not read',logFileUnit)
 IF (AladField_AccSolRad%read(1) .EQ. 0) CALL TestStop(1,'STOP - Accumulated solar radiation was not read',logFileUnit)

 ! Check, if all 3D fields have been read.
 IF (ANY(AladField_T%read(1:Alad_maxLev)     .EQ. 0)) CALL TestStop(1,'STOP - tepmerature was not read',logFileUnit)
 IF (ANY(AladField_geo%read(1:Alad_maxLev)   .EQ. 0)) CALL TestStop(1,'STOP - geopotential was not read',logFileUnit)
 IF (ANY(AladField_p%read(1:Alad_maxLev)     .EQ. 0)) CALL TestStop(1,'STOP - pressure was not read',logFileUnit)
 IF (ANY(AladField_uWind%read(1:Alad_maxLev) .EQ. 0)) CALL TestStop(1,'STOP - uWind was not read',logFileUnit)
 IF (ANY(AladField_vWind%read(1:Alad_maxLev) .EQ. 0)) CALL TestStop(1,'STOP - vWind was not read',logFileUnit)
 IF (ANY(AladField_TKE%read(1:Alad_maxLev)   .EQ. 0)) CALL TestStop(1,'STOP - TKE was not read',logFileUnit)
 IF (ANY(AladField_Rh%read(1:Alad_maxLev)    .EQ. 0)) CALL TestStop(1,'STOP - relative humidity was not read',logFileUnit)
 IF (ANY(AladField_Q%read(1:Alad_maxLev)     .EQ. 0)) CALL TestStop(1,'STOP - specific humidity was not read',logFileUnit)
 IF (ANY(AladField_Ql%read(1:Alad_maxLev)    .EQ. 0)) CALL TestStop(1,'STOP - atmospheric liquid water was not read',logFileUnit)
 IF (ANY(AladField_Qi%read(1:Alad_maxLev)    .EQ. 0)) CALL TestStop(1,'STOP - atmospheric solid water was not read',logFileUnit)
 IF (ANY(AladField_Qr%read(1:Alad_maxLev)    .EQ. 0)) CALL TestStop(1,'STOP - atmospheric rain was not read',logFileUnit)
 IF (ANY(AladField_Qs%read(1:Alad_maxLev)    .EQ. 0)) CALL TestStop(1,'STOP - atmospheric snow was not read',logFileUnit)

 ! Basic checking of data correctness
 IF (checkFlag) THEN
     IF (ANY(Alad_Tsfc    < Tsfc_cmin))     CALL TestStop(1,'STOP - surface temperature < Tsfc_cmin',logFileUnit)
     IF (ANY(Alad_Qsfc    < Qsfc_cmin))     CALL TestStop(1,'STOP - surface specific humidity < Qsfc_cmin',logFileUnit)
     IF (ANY(Alad_GEOsfc  < GEOsfc_cmin))   CALL TestStop(1,'STOP - surface geopotential < GEOsfc_cmin',logFileUnit)
     IF (ANY(Alad_PBL     < PBL_cmin))      CALL TestStop(1,'STOP - planetary boundary layer height < PBL_cmin',logFileUnit)
     IF (ANY(Alad_sfcROUGH< sfcROUGH_cmin)) CALL TestStop(1,'STOP - surface roughness < sfcROUGH_cmin',logFileUnit)
    
     IF (ANY(Alad_T < T_cmin))             CALL TestStop(1,'STOP - tepmerature < T_cmin',logFileUnit)
     IF (ANY(Alad_geo(:,:,1)<GEOsfc_cmin)) CALL TestStop(1,'STOP - geopotential on 1st lev < GEOsfc_cmin',logFileUnit)
     IF (ANY(Alad_geo(:,:,2:)<GEO_cmin))   CALL TestStop(1,'STOP - geopotential in higher levels < GEO_cmin',logFileUnit)
     IF (ANY(Alad_p<p_cmin))               CALL TestStop(1,'STOP - pressure < p_cmin',logFileUnit)
     IF (ANY(ABS(Alad_uWind)>wind_cmax))   CALL TestStop(1,'STOP - abs(uWind) > wind_cmax',logFileUnit)
     IF (ANY(ABS(Alad_vWind)>wind_cmax))   CALL TestStop(1,'STOP - abs(vWind) > wind_cmax',logFileUnit)
!     IF (ANY(Alad_TKE<TKE_cmin))           CALL TestStop(1,'STOP - TKE < TKE_cmin',logFileUnit)
     IF (ANY(Alad_Rh<Rh_cmin))             CALL TestStop(1,'STOP - relative humidity < Rh_cmin',logFileUnit)
     IF (ANY(Alad_Q<Q_cmin))               CALL TestStop(1,'STOP - specific humidity < Q_cmin',logFileUnit)
     DO i=1, Alad_maxLev 
         WRITE(logFileUnit,*)'level ',i
         WRITE(logFileUnit,*)' TKE negative values: ',REAL(COUNT(Alad_TKE(:,:,i)<0.))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Ql negative values: ',REAL(COUNT(Alad_Ql(:,:,i)<0.))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qi negative values: ',REAL(COUNT(Alad_Qi(:,:,i)<0.))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qs negative values: ',REAL(COUNT(Alad_Qs(:,:,i)<0.))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qr negative values: ',REAL(COUNT(Alad_Qr(:,:,i)<0.))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)
         TKE_cmin=-ABS(MAXVAL(Alad_TKE(:,:,i)))*.01
         sigNeg=REAL(COUNT(Alad_TKE(:,:,i)<TKE_cmin))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)' TKE sig.neg. values & field max: ',sigNeg, -TKE_cmin*100.
!         IF (sigNeg>0.01) CALL testStop(1,'STOP - too many sig.neg. values in TKE',logFileUnit)
         Ql_cmin=-ABS(MAXVAL(Alad_Ql(:,:,i)))*.01
         sigNeg=REAL(COUNT(Alad_Ql(:,:,i)<Ql_cmin))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Ql sig.neg. values & field max:',sigNeg, -Ql_cmin*100.
!         IF (sigNeg>0.01) CALL testStop(1,'STOP - too many sig.neg. values in Ql',logFileUnit)
         Qi_cmin=-ABS(MAXVAL(Alad_Qi(:,:,i)))*.01
         sigNeg=REAL(COUNT(Alad_Qi(:,:,i)<Qi_cmin))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qi sig.neg. values & field max: ',sigNeg, -Qi_cmin*100.
!         IF (sigNeg>0.01) CALL testStop(1,'STOP - too many sig.neg. values in Qi',logFileUnit)
         Qs_cmin=-ABS(MAXVAL(Alad_Qs(:,:,i)))*.01
         sigNeg=REAL(COUNT(Alad_Qs(:,:,i)<Qs_cmin))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qs sig.neg. values & field max: ',sigNeg, -Qs_cmin*100.
!         IF (sigNeg>0.01) CALL testStop(1,'STOP - too many sig.neg. values in Qs',logFileUnit)
         Qr_cmin=-ABS(MAXVAL(Alad_Qr(:,:,i)))*.01
         sigNeg=REAL(COUNT(Alad_Qr(:,:,i)<Qr_cmin))/REAL(Alad_nVal)
         WRITE(logFileUnit,*)'  Qr sig.neg. values & field max: ',sigNeg, -Qr_cmin*100.
!         IF (sigNeg>0.01) CALL testStop(1,'STOP - too many sig.neg. values in Qr',logFileUnit)
     END DO
!     IF (ANY(Alad_Ql<Ql_cmin))             CALL TestStop(1,'STOP - atmospheric liquid water < Ql_cmin',logFileUnit)
!     IF (ANY(Alad_Qi<Qi_cmin))             CALL TestStop(1,'STOP - atmospheric solid water < Qi_cmin',logFileUnit)
!     IF (ANY(Alad_Qr<Qr_cmin))             CALL TestStop(1,'STOP - atmospheric rain < Qr_cmin',logFileUnit)
!     IF (ANY(Alad_Qs<Qs_cmin))             CALL TestStop(1,'STOP - atmospheric snow < Qs_cmin',logFileUnit)
 END IF ! end of checking of values

 CALL grib_close_file(ifile) 

 CONTAINS

  SUBROUTINE read_aladin_data(igrib,AladData,level,what,MesgNo)
    IMPLICIT NONE
    INTEGER,                INTENT(IN   ) :: igrib, level
    REAL, DIMENSION(:,:,:), INTENT(  OUT) :: AladData
    CHARACTER(LEN=*),       INTENT(IN   ) :: what ! name of field being read
    INTEGER,                INTENT(IN   ) :: MesgNo ! number of grib message

    REAL, DIMENSION(Alad_nVal) :: values
    INTEGER                    :: j, istat, numberOfMissing
    REAL                       :: average, missingValue
    CHARACTER(LEN=4)           :: MesgStr

    WRITE(MesgStr,'(I4)')MesgNo
    CALL grib_get(igrib,'values',values,istat)
      CALL TestStop(istat,'__get_aladin_fields:__read_aladin_data: ERROR when reading '//what//' from message '//MesgStr)

    ! Simple replacing of (most likely actualy most likely not missing :( ) missing values. 
    CALL grib_get (igrib, 'numberOfMissing' , numberOfMissing)
    CALL grib_get (igrib, 'average'         , average)
    CALL grib_get (igrib, 'missingValue'    , missingValue)
!    IF (numberOfMissing>0) THEN
!      WRITE(logFileUnit,*)' !!! In '//what//' field are ',numberOfMissing,' missing values. Message number ',MesgNo,'.'
!      WRITE(logFileUnit,*)'     Replacing values equal to missing code (',missingValue,') by field average (',average,')'
!      WHERE(values==missingValue) values=average
!    END IF

    ! v zavislosti na zpusobu razeni dat v gribu je priradi do AladData(i,j,k), tak aby index
    !    i vzrustal smerem na vychod,
    !    j smerem na sever a
    !    k smerem k vyssim hladinam
    IF (Alad_iScanNeg==0 .AND. Alad_jScanPos==1 .AND. Alad_jConsec==0 .AND. Alad_aRowScan==0) THEN
        DO j=1, Alad_nY
            AladData(1:Alad_nX,j,level) = values( (j-1)*Alad_nX+1 : j*Alad_nX )
        END DO
    ELSE
        WRITE(*,*)'__get_aladin_fields:__read_aladin_data: STOP Prevod pole podle takovehoto scanningu jeste nebyl napsan'
    END IF
  END SUBROUTINE read_aladin_data

END SUBROUTINE get_aladin_fields

