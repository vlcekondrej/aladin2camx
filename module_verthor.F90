MODULE module_verthor
IMPLICIT NONE

CONTAINS

  SUBROUTINE verthor2d(FIELD,NX_INT_FIELD,NY_INT_FIELD,G,INT_FIELD)
   USE module_standard_types
   USE module_global_variables
   IMPLICIT NONE
  
   INTEGER,      INTENT(IN   ) :: NX_INT_FIELD, NY_INT_FIELD, G ! dimensions of output field; grid number
   REAL(KIND=sp),INTENT(IN   ) :: FIELD(Alad_nx,Alad_ny,1)
   REAL(KIND=sp),INTENT(  OUT) :: INT_FIELD(NX_INT_FIELD,NY_INT_FIELD,1)
  
   INTEGER :: x,y,xi,yi,xs,xe,ys,ye,ainc,ni
   REAL(KIND=sp) :: suma
  
  
   ys=nest_ystart(g)
   ye=nest_yend(g)
   xs=nest_xstart(g)
   xe=nest_xend(g)
   ainc=nest_mesh(ngridnumber)/nest_mesh(g) ! kolik ALADIN-ovskych bunek obsahuje CAMx-ovska
  
   ! now horizontal fields are interpolated
   IF(g==ngridnumber) THEN
       ! Grid s nejjemnejsim rozlisenim, odpovidajicim rozliseni nodelu ALADIN. 
       ! Provede se jednoduche precislovani.
       INT_FIELD(1:xe-xs+1, 1:ye-ys+1, 1) = FIELD(xs:xe, ys:ye, 1)
  
   ELSE
       DO y=ys,ye,ainc
           DO x=xs,xe,ainc
               ni=int(ainc-.1)/2 
               ! pro ainc=2    ... jen bod ve stredu
               !          3, 4 ... prumeruji nejblizsich 9 bodu
               !          5, 6 ... prumeruji nejblizsich 25 bodu atd.
               suma=0.
               DO xi=x-ni, x+ni
                   DO yi=y-ni, y+ni
                       suma=suma+FIELD(xi,yi,1)
                   END DO
               END DO
               INT_FIELD((x-xs)/ainc+1,(y-ys)/ainc+1,1) = suma / ainc**2 
           END DO 
       END DO
  
   END IF
  
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
