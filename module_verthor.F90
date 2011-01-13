MODULE module_verthor
IMPLICIT NONE

CONTAINS

  SUBROUTINE verthor2d(FIELD,NX_INT_FIELD,NY_INT_FIELD,G,INT_FIELD,NRCELL,METHOD)
   ! originating from verthor.F90 contained in ZAMG preprocesor
 
   ! Description:
   !   Performs horizontal interpolation 
   !   
   !
   ! Method:
   !   
   !
   ! Current code maintainer: Ondrej Vlcek (CHMI)
   !
   ! History:
   ! Version   Date Y-M-D   Comment
   ! -------   ----------   -------
   ! 0         2010-11-24   ... Ondrej Vlcek

   USE module_standard_types
   USE module_global_variables
   IMPLICIT NONE
  
   INTEGER,      INTENT(IN   )           :: NX_INT_FIELD, NY_INT_FIELD, G ! dimensions of output field; grid number
   REAL(KIND=sp),INTENT(IN   )           :: FIELD(Alad_nx,Alad_ny,1)
   REAL(KIND=sp),INTENT(  OUT)           :: INT_FIELD(NX_INT_FIELD,NY_INT_FIELD,1)
   INTEGER,      INTENT(IN   ), OPTIONAL :: NRCELL
   INTEGER,      INTENT(IN   ), OPTIONAL :: METHOD
  
   INTEGER       :: x,y,xi,yi,xs,xe,ys,ye,ainc,ni,ncell,meth
   REAL(KIND=sp) :: suma
  
   IF (PRESENT(NRCELL)) THEN
       ncell = NRCELL
   ELSE
       ncell = 0 
   END IF

   IF (PRESENT(METHOD)) THEN
       meth = METHOD
   ELSE
       meth = 0
   END IF

   ys   = grid_ybeg(g)
   ye   = grid_yend(g)
   xs   = grid_xbeg(g)
   xe   = grid_xend(g)
   ainc = grid2alad(g) ! number of ALADIN cells included in one CAMx cell

   ! now horizontal fields are interpolated
   DO y = ys, ye, ainc
     DO x = xs, xe, ainc

       suma = 0.
       DO xi = x-ncell, x+ncell
         DO yi = y-ncell, y+ncell

           SELECT CASE (meth)
             CASE(0)
               suma = suma + FIELD(xi,yi,1)
             CASE DEFAULT
               WRITE(logFileUnit,*) '__verthor2d: Unknown interpolation method'
               WRITE(logFileUnit,*) '           : Dummy variable METHOD = ',METHOD
               STOP
           END SELECT

         END DO
       END DO

       SELECT CASE (meth)
         CASE(0)
           INT_FIELD( (x-xs)/ainc+1, (y-ys)/ainc+1, 1 ) = suma / (2*ncell+1)**2
       END SELECT

       END DO 
   END DO
 
   RETURN
  END SUBROUTINE verthor2d
  
  
  SUBROUTINE verthor3d(FIELD,NX_INT_FIELD,NY_INT_FIELD,G,INT_FIELD)
   USE module_standard_types
   USE module_global_variables
   IMPLICIT NONE
  
   INTEGER,      INTENT(IN   ) :: NX_INT_FIELD,NY_INT_FIELD,G ! dimensions of output field; grid number
   REAL(KIND=sp),INTENT(IN   ) :: FIELD(Alad_nX,Alad_nY,Alad_maxLev)
   REAL(KIND=sp),INTENT(  OUT) :: INT_FIELD(NX_INT_FIELD,NY_INT_FIELD,CAMx_nLev+1)
  
   REAL(KIND=sp) :: INT_FIELD_HELP(Alad_nX,Alad_nY,CAMx_nLev+1)
   INTEGER       :: k,n
  
  
   ! first agregate ALAD_maxLev ALADIN vertical levels into CAMx_nLev CAMx vertical levels
   ! CAMx_nLev+1 ... pomocna vrstva umoznujici definovat horni rozhrani vrstvy CAMx_nLev
   DO k=1,CAMx_nLev+1
       N = CAMx_levDef(k,2)-CAMx_levDef(k,1)+1 ! number of ALADIN levels included in k-th CAMx level
       INT_FIELD_HELP(:,:,k) = SUM(FIELD(:,:,CAMx_levDef(k,1):CAMx_levDef(k,2)),3)/N
   END DO
   
   DO k=1, CAMx_nLev+1
       CALL verthor2d(INT_FIELD_HELP(:,:,k),NX_INT_FIELD,NY_INT_FIELD,G,INT_FIELD(:,:,k))
   END DO
  
   RETURN
  END SUBROUTINE verthor3d

END MODULE module_verthor
