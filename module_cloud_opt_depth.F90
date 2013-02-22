MODULE module_cloud_opt_depth

 !--------------------------------------------------------------------------------
 !
 ! functions for calculation cloud optical depth
 ! ---------------------------------------------
 !
 !
 !
 !
 !--------------------------------------------------------------------------------


 IMPLICIT NONE

 CONTAINS



 SUBROUTINE COD_chimere(nk,  hiCol, cwtrCol, rwtrCol, swtrCol, RhoCol, rhCol, MethLow, MethMed, MethHigh, codCol)
 !-----------------------------------------------------------------------------------------
 !
 ! Calculates Cloud Optical Depth using Chimere algorithm -
 ! - see module diagmet_science in Chimere 2008c code
 !
 !-----------------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER,                INTENT(IN   ) :: nk ! number of levels
   REAL   , DIMENSION(nk), INTENT(IN   ) :: hiCol, & ! layer upper interface height in      [m AGL]
                                         &  cwtrCol, & ! local profile of cloud water       [kg/kg] 
                                         &  rwtrCol, & !     -- || --     rain water        [kg/kg]
                                         &  swtrCol, & !     -- || --     snow water        [kg/kg]
                                         &  RhoCol, &  !     -- || --     air density       [kg/m3]
                                         &  rhCol      !     -- || --     relative humidity [Pa/Pa]
   CHARACTER(LEN=2)      , INTENT(IN   ) :: MethLow, MethMed, MethHigh ! method to be used for low, medium, and high clouds. Can be 'rh' or 'li'
   REAL   , DIMENSION(nk), INTENT(  OUT) :: codCol ! Cloud optical depth (dimensionless) integrated from top to bottom

   REAL, PARAMETER :: RHcritL = 0.85 ! Critical relative humidity level for formation of low    clouds 
   REAL, PARAMETER :: RHcritM = 0.95 !                     -- || --                      medium clouds
   REAL, PARAMETER :: RHcritH = 0.95 !                     -- || --                      high   clouds

   REAL, PARAMETER :: clol = 0.025  ! Low    cloud optical depth /m for RH=1
   REAL, PARAMETER :: clom = 0.010  ! Medium cloud         -- || --
   REAL, PARAMETER :: cloh = 0.005  ! High   cloud         -- || --

   REAL, PARAMETER :: odcic = 0.06E3/0.9 ! optical depth rate for ice
   REAL, PARAMETER :: odclw = 0.18E3     !      -- || --          liquid water

   REAL, PARAMETER :: topl =  2500. ! Low    cloud top altidue AGL
   REAL, PARAMETER :: topm =  6000. ! Medium cloud    -- || --
   REAL, PARAMETER :: toph = 20000. ! High   cloud    -- || --


   INTEGER :: k
   REAL    :: thickness, cwtr,rwtr,swtr,rho, rh, odsum
   REAL    :: alt(0:nk)

   !-----------------------------------------------------------------------------------------
   odsum = 0.
   alt = 0.
   alt(1:nk) = hiCol(1:nk)

   DO k = nk, 1, -1
       ! to make the code readable:
       thickness = alt(k)
       IF (k>1) thickness = thickness - alt(k-1)
       cwtr = cwtrCol(k)
       rwtr = rwtrCol(k)
       swtr = swtrCol(k)
       rho  = rhoCol(k)
       rh   = rhCol(k)
   
       IF (alt(k)<=topL) THEN
           ! ==  in LOW cloud layer
           IF (MethLow=='rh') THEN
               odsum = odsum + cloL * MAX(0.,RH-RHcritL)/(1.-RHcritL) * thickness        
           ELSE IF (MethLow=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * thickness
           ELSE 
               STOP 'Unknown optical depth option'
           END IF

       ELSE IF (alt(k)>topL .AND. alt(k-1)<topL) THEN 
           ! == devided between LOW and MEDIUM cloud layer
           IF (MethLow=='rh') THEN
               odsum = odsum + cloL * MAX(0.,RH-RHcritL)/(1.-RHcritL) * (topL-alt(k-1))        
           ELSE IF (MethLow=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * (topL-alt(k-1))
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

           IF (MethMed=='rh') THEN
               odsum = odsum + cloM * MAX(0.,RH-RHcritM)/(1.-RHcritM) * (alt(k)-topL)        
           ELSE IF (MethMed=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * (alt(k)-topL)
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

       ELSE IF (alt(k)<=topM) THEN 
           ! == in MEDIUN cloud layer
           IF (MethMed=='rh') THEN
               odsum = odsum + cloM * MAX(0.,RH-RHcritM)/(1.-RHcritM) * thickness        
           ELSE IF (MethMed=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * thickness
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

       ELSE IF (alt(k)>topM .AND. alt(k-1)<topM) THEN 
           ! == devided between MEDIUM and HIGH cloud layer
           IF (MethMed=='rh') THEN
               odsum = odsum + cloM * MAX(0.,RH-RHcritM)/(1.-RHcritM) * (topM-alt(k-1))
           ELSE IF (MethMed=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * (topM-alt(k-1))
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

           IF (MethHigh=='rh') THEN
               odsum = odsum + cloH * MAX(0.,RH-RHcritH)/(1.-RHcritH) * (alt(k)-topM)
           ELSE IF (MethHigh=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * (alt(k)-topM)
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

       ELSE
           ! == in HIGH cloud layer
           IF (MethHigh=='rh') THEN
               odsum = odsum + cloH * MAX(0.,RH-RHcritH)/(1.-RHcritH) * thickness        
           ELSE IF (MethHigh=='li') THEN
               odsum = odsum + ((cwtr+rwtr)*odclw + swtr*odcic) * rho * thickness
           ELSE 
               STOP 'Unknown optical depth option!'
           END IF

       END IF

       codCol(k) = odsum
   END DO


 END SUBROUTINE COD_chimere








 SUBROUTINE COD_wrfcamx(nk,  hiCol, rhoCol, qlCol, qiCol, qrCol, qsCol, qgCol, codCol)
 !-----------------------------------------------------------------------------------------
 !
 ! Calculates Cloud Optical Depth using algorithm from WRF-CAMx preprocessor v2.1
 !
 !-----------------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER,                INTENT(IN   ) :: nk ! number of levels
   REAL   , DIMENSION(nk), INTENT(IN   ) :: hiCol, &  ! layer upper interface height in            [m AGL]
                                         &  RhoCol, & ! local profile of air density               [kg/m3]
                                         &  qlCol, &  !      -- || --    atmospheric liquid water  [kg/kg]
                                         &  qiCol, &  !             -- || --         solid water   [kg/kg]
                                         &  qrCol, &  !             -- || --         rain          [kg/kg]
                                         &  qsCol, &  !             -- || --         snow          [kg/kg]
                                         &  qgCol     !             -- || --         graupel       [kg/kg]
   REAL   , DIMENSION(nk), INTENT(  OUT) :: codCol ! Cloud optical depth integrated from top to bottom

   REAL :: codSum, dz
   REAL, DIMENSION(nk) :: cc,pp
   INTEGER :: k

   ! convert to g/m3
   cc = (qlCol + qiCol)*rhoCol*1000
   pp = (qrCol + qsCol + qgCol)*rhoCol*1000

   codSum = 0.
   do k = nk,1,-1
     dz = hiCol(k)
     if (k > 1) dz = dz - hiCol(k-1)
     if (cc(k) > 1E-5) codSum = codSum + 3.*cc(k)*dz*.05
     if (pp(k) > 1E-5) codSum = codSum + 3.*pp(k)*dz*.001
     codCol(k) = codSum
   enddo

 END SUBROUTINE COD_wrfcamx



END MODULE module_cloud_opt_depth
