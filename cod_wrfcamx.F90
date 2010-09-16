 SUBROUTINE COD_wrfcamx(nk,  hiCol, rhoCol, qlCol, qiCol, qrCol, qsCol, qgCol, codCol)
 !-----------------------------------------------------------------------------------------
 !
 ! Calculates Cloud Optical Depth using algorithm from WRF-CAMx preprocessor v2.1
 !
 !-----------------------------------------------------------------------------------------
   USE module_global_variables
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
