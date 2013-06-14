MODULE module_verthor
IMPLICIT NONE

CONTAINS

  SUBROUTINE verthor2d(FIELD,XS,XE,YS,YE,STEP,INT_FIELD) 
   ! originating from verthor.F90 contained in ZAMG preprocesor
 
   ! Description:
   !   Performs horizontal interpolation 
   !   
   ! Method:
   !
   ! Current code maintainer: Ondrej Vlcek (CHMI)
   !
   ! History:
   ! Version   Date Y-M-D   Comment
   ! -------   ----------   -------
   ! 0         2010-11-24   ... Ondrej Vlcek
   ! 1         2013-02-15   

   USE module_standard_types
   USE module_global_variables
   IMPLICIT NONE
  
   INTEGER,      INTENT(IN   )           :: XS,XE,YS,YE,STEP ! first and last indexes of CAMx grid relatice to ALADIN and its step
   REAL(KIND=sp),INTENT(IN   )           :: FIELD(Alad_nx,Alad_ny,1)
   REAL(KIND=sp),INTENT(  OUT)           :: INT_FIELD((xe-xs+1)/step, (ye-ys+1)/step, 1)
  
   INTEGER       :: x,y
   REAL(KIND=sp) :: suma
  
   IF ( step == 1 ) THEN
     ! SIMPLY WINDOW DATA
     INT_FIELD( :, :, 1 ) = FIELD(xs:xe, ys:ye, 1 )
     RETURN
   END IF

   ! now horizontal fields are averaged
   DO y = ys, ye, step
     DO x = xs, xe, step

       suma = sum(FIELD(x:x+step-1,y:y+step-1,1))
       INT_FIELD( (x-xs)/step+1, (y-ys)/step+1, 1 ) = suma / step**2

     END DO 
   END DO
 
   RETURN
  END SUBROUTINE verthor2d
  
  
  SUBROUTINE verthor3d(FIELD,XS,XE,YS,YE,STEP,INT_FIELD)
   USE module_standard_types
   USE module_global_variables
   IMPLICIT NONE
  
   INTEGER,      INTENT(IN   ) :: XS,XE,YS,YE,STEP ! first and last indexes of CAMx grid relatice to ALADIN and its step
   REAL(KIND=sp),INTENT(IN   ) :: FIELD(Alad_nX,Alad_nY,Alad_maxLev)
   REAL(KIND=sp),INTENT(  OUT) :: INT_FIELD((xe-xs+1)/step, (ye-ys+1)/step,CAMx_nLev+1)
  
   REAL(KIND=sp) :: INT_FIELD_HELP(Alad_nX,Alad_nY,CAMx_nLev+1)
   INTEGER       :: k,n
  
  
   ! first agregate ALAD_maxLev ALADIN vertical levels into CAMx_nLev CAMx vertical levels
   ! CAMx_nLev+1 ... pomocna vrstva umoznujici definovat horni rozhrani vrstvy CAMx_nLev
   DO k=1,CAMx_nLev+1
       N = CAMx_levDef(k,2)-CAMx_levDef(k,1)+1 ! number of ALADIN levels included in k-th CAMx level
       IF(N.lt.1) THEN
           WRITE(logFileUnit,'(A,I3,A,I3)') 'Number of ALADIN levels included in CAMx level ',k,' is: ', N 
           WRITE(logFileUnit,'(A,I4,A,I4)') 'Aladin levels were ', CAMx_levDef(k,1) , ' to ', CAMx_levDef(k,2)
           CALL TestStop(1,'Error (verthor3d): number of ALADIN levels included in k-th CAMx level < 1.', logFileUnit)
       END IF
       INT_FIELD_HELP(:,:,k) = SUM(FIELD(:,:,CAMx_levDef(k,1):CAMx_levDef(k,2)),3)/N
   END DO
   
   DO k=1, CAMx_nLev+1
       CALL verthor2d(INT_FIELD_HELP(:,:,k),XS,XE,YS,YE,STEP,INT_FIELD(:,:,k))
   END DO
  
   RETURN
  END SUBROUTINE verthor3d

END MODULE module_verthor
