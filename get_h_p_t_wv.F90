SUBROUTINE get_h_p_t_wv(d)
!------------------------------------------------------
! All computation in SI and derived units.             |
! Final conversion to CAMx units just before write out |
!------------------------------------------------------
 USE module_smoothing_handler
 USE module_standard_types
 USE module_global_variables
 USE module_physical_constants
 USE module_meteo_functions
 USE module_verthor
 USE module_BEISmet
! USE Inter_Faces,only: kv_l79
 IMPLICIT NONE

 !  == INPUT parameters ==
 INTEGER, INTENT(IN   ) :: d ! d ... poradi alad GRIBu v poli aladin_met


 INTEGER :: i,j,k,g,istat, ii

 REAL(KIND=sp) :: Alad_hgt  (Alad_nx,Alad_ny,Alad_maxLev)   ! AGL height in grid centres
 ! nebo namisto definovani Alad_UArakC prepsat pole Alad_uWind ??????????????
 REAL(KIND=sp) :: Alad_UArakC  (Alad_nx,Alad_ny,Alad_maxLev)   ! Aladin U wind transformed to Arakawa C grid 
 REAL(KIND=sp) :: Alad_VArakC  (Alad_nx,Alad_ny,Alad_maxLev)   ! Aladin V wind transformed to Arakawa C grid 


 REAL(KIND=sp), POINTER :: pto3D(:,:,:)=>NULL(), pto2D(:,:)=>NULL()


 REAL(KIND=sp), ALLOCATABLE :: HGT(:,:,:), &      ! layer-average height        [m AGL]
                           &   Rh(:,:,:), &       ! Relative humidity           [Pa/Pa]
                           &   Q(:,:,:), &        ! specifc humidity            [kg/kg]
                           &   rho(:,:,:), &      ! air density                 [kg/m**3]
                           &   Psfc(:,:,:), &     ! surface pressure            [Pa]
                           &   PBL(:,:,:), &      ! planet. bound. layer height [m]
                           &   sfcROUGH(:,:,:), & ! surf. roughness length      [m]
                           &   TKE(:,:,:), &      ! turbulent kinetic energy    [?]
                           &   SolRad(:,:,:)      ! average solar radiation during LAST NWP model integration STEP  [W/m2]
 
 ! == output fields for CAMx ==                                                                     SI units   CAMx units (if different)
! REAL(KIND=sp), ALLOCATABLE :: HGT_I(:,:,:), &          ! layer upper interface height              [m AGL]    
 REAL, ALLOCATABLE :: HGT_I(:,:,:), &          ! layer upper interface height              [m AGL]    
                  &   P(:,:,:), &              ! layer-average pressure                    [Pa]       [mb] = [hPa]
                  &   U(:,:,:), V(:,:,:), &    ! layer-average x and y wind speed          [m/s]      
                  &   T(:,:,:), Tsfc(:,:,:), & ! layer-average and surface temperature     [K]
                  &   WV(:,:,:), &             ! layer-average water vapor concentration   [kg/kg]    [ppm]
                  &   rkv(:,:,:), &            ! layer upper interface ver. diff.          [m2/s]
                  &   cldwtr(:,:,:), &         ! atmos. cloud water                        [kg/m3]    [g/m3]
                  &   icewtr(:,:,:), &         !        solid water                        [kg/m3]    [g/m3]
                  &   ranwtr(:,:,:), &         !        rain water                         [kg/m3]    [g/m3]
                  &   snowtr(:,:,:), &         !        snow water                         [kg/m3]    [g/m3]  
                  &   COD(:,:,:)               ! from top integrated cloud optical depth   [?]

 ! header for cloud rain file
 CHARACTER(LEN=20) :: cldhdr='CAMX_V4.3 CLOUD_RAIN'


 INTEGER :: XEND,YEND ! pointer to grid dimensions

 ! == used for computation of vertical diffusivity ==
 REAL(KIND=sp) :: z1,z2, thetav2, u2, v2
 REAL(KIND=sp) :: ustar, wstar, MOLenI, thetav(CAMx_nLev+1), thetavProf(CAMx_nLev+1), uwind(CAMx_nLev+1), vwind(CAMx_nLev+1)
 REAL(KIND=sp) :: sumtk, sumtkz, xinf, eee, dz


!tmpFile real(4), dimension(Alad_nx, ALAD_ny) :: ustar_tmp, wstar_tmp, MOLenI_tmp, wind_tmp, pbl_tmp
!tmpFile real(4), dimension(Alad_nx, ALAD_ny, CAMx_nLev) :: thetav_tmp, hgt_tmp,hgt_i_tmp, uwind_tmp, vwind_tmp
!tmpFile 
!tmpFile ustar_tmp=-999999.
!tmpFile wstar_tmp=-999999.
!tmpFile MOLenI_tmp=-999999.
!tmpFile wind_tmp=-999999.
!tmpFile thetav_tmp=-999999.
!tmpFile pbl_tmp=-999999.
!tmpFile hgt_tmp=-999999.
!tmpFile hgt_i_tmp=-999999.
!tmpFile uwind_tmp=-999999.
!tmpFile vwind_tmp=-999999.

!REAL::Alad_sigma(Alad_nx,Alad_ny,0:Alad_maxLev)
!REAL, PARAMETER :: Alad_Ptop=500.


! ! define sigma levels according to *ALADIN* levels
!  Alad_sigma(:,:,0)=1.
!  do i=1,Alad_nx
!      do j=1,Alad_ny
!          do k=1,Alad_maxLev 
!              Alad_sigma(i,j,k)=(Alad_p(i,j,k)-Alad_Ptop)/(Alad_psfc(i,j,1)-Alad_Ptop)
!          end do
!      end do
!  end do
!
!
!  WRITE(logFileUnit,*)'surf press'
!  WRITE(logFileUnit,*)Alad_psfc(1,1,1),ALAD_psfc(65,150,1)
!  WRITE(logFileUnit,*)'profile'
!do k=1,Alad_maxLev
!  WRITE(logFileUnit,*)Alad_p(1,1,k),Alad_sigma(1,1,k),ALAD_sigma(1,1,k)-Alad_sigma(65,150,k)
!end do
!stop
!  WRITE(logFileUnit,"('date=',I8.8,'  time='I6.6)")aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ
!  WRITE(logFileUnit,*)'geo_sfc ',0,' ',Alad_geosfc(226,162,1)
!  do k=1,Alad_MaxLev
!    WRITE(logFileUnit,*)'geo     ',k,' ',Alad_geo(226,162,k)
!  end do
!  WRITE(logFileUnit,*)

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 !                                                                     *
 ! REMOVE missing values                                               *
 ! SMOOTH (if SMOOTHER_SWITCH=.TRUE.)                                  *
 !                                                                     *
 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 ! * 3D fields

 ! == P ==
   pto3D=>Alad_P
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
 ! == GEO ==
   pto3D=>Alad_geo
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
 ! == uWind ==
   pto3D=>Alad_uWind
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
 ! == vWind ==
   pto3D=>Alad_vWind
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
 ! == TKE ==
   pto3D=>Alad_TKE
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_TKE<1E-8) Alad_TKE=1E-8 ! according to CHMI NWP department
 ! == T ==
   pto3D=>Alad_t
   IF(SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
 ! == Rh ==
   pto3D=>Alad_Rh
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_Rh<0.) Alad_Rh=0.
   WHERE(Alad_Rh>1.) Alad_Rh=1.
 ! == Q ==
   pto3D=>Alad_q
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_q<0.) Alad_q=0.
 ! == Ql atmos. liquid water ==
   pto3D=>Alad_Ql
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_Ql<0.) Alad_Ql=0.
 ! == Qi atmos. solid water ==
   pto3D=>Alad_Qi
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_Qi<0.) Alad_Qi=0.
 ! == Qr atmos. rain ==
   pto3D=>Alad_Qr
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_Qr<0.) Alad_Qr=0.
 ! == Qs atmos. snow ==
   pto3D=>Alad_Qs
   IF (SMOOTHER_SWITCH) CALL smoother(pto3D,smoother_method)
   WHERE(Alad_Qs<0.) Alad_Qs=0.

 ! * 2D fields

 ! == GEOsfc ==
   pto2D=>Alad_GEOsfc(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
 ! == Tsfc ==
   pto2D=>Alad_Tsfc(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
 ! == Psfc ==
   pto2D=>Alad_Psfc(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
 ! == PBL ==
   pto2D=>Alad_PBL(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
   WHERE(Alad_PBL<0.) Alad_PBL=0.
 ! == sfcROUGH ==
   pto2D=>Alad_sfcROUGH(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
   WHERE(Alad_sfcROUGH<0.) Alad_sfcROUGH=0.
 ! == SolRad ==
   pto2D=>Alad_SolRad(:,:,1)
   IF(SMOOTHER_SWITCH) CALL smoother(pto2D,smoother_method)
   WHERE(Alad_SolRad<0.) Alad_SolRad=0.



 ! convert surface roughness to [m]
   Alad_sfcROUGH = Alad_sfcROUGH / grav

 ! layer-average ** HEIGHT ** in m AGL
 ! predpoklad, ze Alad_geo odpovida vertikalnimu prumeru geopotencialu
   DO k=1,Alad_maxLev
       Alad_hgt(:,:,k) = (Alad_GEO(:,:,k)-Alad_GEOsfc(:,:,1)) / grav
   END DO

 ! Interpolate wind components from cell center points to ARAKAWA C grid.
   DO i=1,Alad_nX-1
       Alad_UArakC(i,:,:) = (Alad_uWind(i,:,:)+Alad_uWind(i+1,:,:)) * 0.5
   END DO
  
   DO j=1,Alad_nY-1
       Alad_VArakC(:,j,:) = (Alad_vWind(:,j,:)+Alad_vWind(:,j+1,:)) * 0.5
   END DO
   Alad_VArakC(:,Alad_ny,:)=0.0 ! CAMx tyto hodnoty nepouzije, ale, aby tam nebyly hausnumera,...
   Alad_UArakC(Alad_nx,:,:)=0.0 !                            -- || --

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 !                                                                                   *
 ! loop over the grids (mother and nested)                                           *
 ! all needed fields are allocated for the specific grid dimensions (XEND,YEND,...)  *
 !                                                                                   *
 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 grid: DO g=1,ngridnumber
     WRITE(LogFileUnit,'(a,I2)')'   ... Processing grid No ',g

     XEND=CAMx_nx(g)
     YEND=CAMx_ny(g)
     
     ALLOCATE(P        (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array P allocation error') 
     ALLOCATE(Psfc     (XEND,YEND,1          ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array Psfc allocation error') 
     ALLOCATE(HGT_I    (XEND,YEND,CAMx_nLev  ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array HGT_I allocation error') 
     ALLOCATE(HGT      (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array HGT allocation error') 
     ALLOCATE(U        (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array U allocation error') 
     ALLOCATE(V        (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array V allocation error') 
     ALLOCATE(TKE      (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array TKE allocation error') 
     ALLOCATE(T        (XEND,YEND,CAMx_nlev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array T allocation error')  
     ALLOCATE(Tsfc     (XEND,YEND,1          ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array Tsfc allocation error')  
     ALLOCATE(Q        (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array Q allocation error')
     ALLOCATE(Rh       (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array Rh allocation error')
     ALLOCATE(WV       (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array WV allocation error')
     ALLOCATE(rkv      (XEND,YEND,CAMx_nLev  ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array RKV allocation error') 
     ALLOCATE(ranwtr   (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array RANWTR allocation error') 
     ALLOCATE(snowtr   (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array SNOWTR allocation error') 
     ALLOCATE(cldwtr   (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array CLDWTR allocation error') 
     ALLOCATE(icewtr   (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array ICEWTR allocation error') 
     ALLOCATE(COD      (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array COD allocation error') 
     ALLOCATE(rho      (XEND,YEND,CAMx_nLev+1), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array RHO allocation error') 
     ALLOCATE(PBL      (XEND,YEND,1          ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array PBL allocation error')  
     ALLOCATE(sfcROUGH (XEND,YEND,1          ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array sfcROUGH allocation error')
     ALLOCATE(SolRad   (XEND,YEND,1          ), STAT=istat); CALL TestStop(istat,'get_h_p_t_wv: array SolRad allocation error')

     ! interpolate 2D fields (horizontaly)  
     CALL verthor2d(Alad_Tsfc    ,XEND,YEND,g,Tsfc)
     CALL verthor2d(Alad_Psfc    ,XEND,YEND,g,Psfc)
     CALL verthor2d(Alad_PBL     ,XEND,YEND,g,PBL)
     CALL verthor2d(Alad_sfcROUGH,XEND,YEND,g,sfcROUGH)
     CALL verthor2d(Alad_SolRad,  XEND,YEND,g,SolRad)

     ! write BEIS meteo fields
     IF ( BEIS_flag ) THEN
         iret = nf90_put_var(netCDFid(g), PRSFC_varID(g) , &
           Psfc(1:CAMx_nx(g),1:CAMx_ny(g),1), start=(/1,1,1,d/), count=(/CAMx_nx(g),CAMx_ny(g),1,1/))
           CALL TestStop(iret-nf90_NoErr,' pressure: '//trim(nf90_strerror(iret)),logFileUnit)
!write(*,*)' Psfc  : min[',minloc(Psfc),']=',minval(Psfc), '   max[',maxloc(Psfc),']=',maxval(Psfc)         

         iret = nf90_put_var(netCDFid(g), TEMPSFC_varID(g), &
           Tsfc(1:CAMx_nx(g),1:CAMx_ny(g),1), start=(/1,1,1,d/), count=(/CAMx_nx(g),CAMx_ny(g),1,1/))
           CALL TestStop(iret-nf90_NoErr,' Sfc.Temp: '//trim(nf90_strerror(iret)),logFileUnit)
!write(*,*)' Tsfc  : min[',minloc(Tsfc),']=',minval(Tsfc), '   max[',maxloc(Tsfc),']=',maxval(Tsfc)         

         iret = nf90_put_var(netCDFid(g), SWRSFC_varID(g) , &
           SolRad(1:CAMx_nx(g),1:CAMx_ny(g),1), start=(/1,1,1,d/), count=(/CAMx_nx(g),CAMx_ny(g),1,1/))
           CALL TestStop(iret-nf90_NoErr,' Sol.rad.: '//trim(nf90_strerror(iret)),logFileUnit)
!write(*,*)' SolRad: min[',minloc(SolRad),']=',minval(SolRad), '   max[',maxloc(SolRad),']=',maxval(SolRad)         
     END IF

     ! interpolate 3Dd fields (horizontally + vertically)
     CALL verthor3d(Alad_P     ,XEND,YEND,g, P)
     CALL verthor3d(Alad_hgt   ,XEND,YEND,g, HGT)
     CALL verthor3d(Alad_UArakC,XEND,YEND,g, U)
     CALL verthor3d(Alad_VArakC,XEND,YEND,g, V)
     CALL verthor3d(Alad_TKE   ,XEND,YEND,g, TKE)
     CALL verthor3d(Alad_T     ,XEND,YEND,g, T)
     CALL verthor3d(Alad_Rh    ,XEND,YEND,g, Rh)
     CALL verthor3d(Alad_Q     ,XEND,YEND,g, Q)
     CALL verthor3d(Alad_Ql    ,XEND,YEND,g, cldwtr)
     CALL verthor3d(Alad_Qi    ,XEND,YEND,g, icewtr)
     CALL verthor3d(Alad_Qr    ,XEND,YEND,g, ranwtr)
     CALL verthor3d(Alad_Qs    ,XEND,YEND,g, snowtr)

     ! ** AVERAGE layer height
     DO k=1,CAMx_nLev
       CAMx_avgLevHgt(k,g)=CAMx_avgLevHgt(k,g) + SUM(HGT(:,:,k))/(XEND*YEND)
     END DO

     ! ** AIR DENSITY **
     Rho = AirDens( press=P, temp=Tvirt_tq(temp=T,SpHum=Q) )
 
     ! ** LAYER UPPER INTERFACE HEIGHT **
      DO k=1,CAMx_nLev
         HGT_I(:,:,k) = (HGT(:,:,k) + HGT(:,:,k+1)) * 0.5
      END DO

     ! **WATER WAPOR** = Mv/Md in [kg/kg]; Q=Mv/(Mv+Md) is specific humidity  
     WV = Q/(1-Q)

     ! ** VERTICAL DIFFUSIVITY ** 
     ! rkv is calculated on the upper layer interface; top rkv is 0
     ! wind is interpolated to cell centers
     lon: DO j=1,YEND
       lat: DO i=1,XEND

         uWind = U(i,j,:)
         IF (i>1) uWind = (U(i,j,:) + U(i-1,j,:)) * 0.5
         vWind = V(i,j,:)
         IF (j>1) vWind = (V(i,j,:) + V(i,j-1,:)) * 0.5
!tmpFile uwind_tmp(i,j,:)=uwind(1:CAMx_nLev)
!tmpFile vwind_tmp(i,j,:)=vwind(1:CAMx_nLev)

         thetav = Tpot(temp=Tvirt_tq(temp=T(i,j,:),SpHum=Q(i,j,:)), press=P(i,j,:)) ! virtual potential temperature 
         if (i==vpx .AND. j==vpy) thetavProf=thetav ! because of control output
!tmpFile thetav_tmp(i,j,:)=thetav(1:CAMx_nLev)
!tmpFile pbl_tmp(i,j)=PBL(i,j,1)
!tmpFile hgt_tmp(i,j,:)=HGT(i,j,1:CAMx_nLev)
!tmpFile hgt_i_tmp(i,j,:)=HGT_I(i,j,1:CAMx_nLev)

         SELECT CASE (trim(kv_method))
         CASE ('l79')
           call kv_l79( CAMx_nLev, thetav(1:CAMx_nLev+1), uWind(1:CAMx_nLev+1), vWind(1:CAMx_nLev+1), &
                      & HGT(i,j,1:CAMx_nLev+1), HGT_I(i,j,1:CAMx_nLev), 100., kvmin, rkv(i,j,1:CAMx_nLev))
           rkv(i,j,CAMx_nLev) = 0.
 
         CASE ('ob70')
           call kv_ob70( nz=CAMx_nLev, kvmin=kvmin, pbl=PBL(i,j,1), zz=HGT_I(i,j,1:CAMx_nLev), tv=thetav(1:CAMx_nLev), &
                       & uu=uWind(1:CAMx_nLev), vv=vWind(1:CAMx_nLev), rkv=rkv(i,j,1:CAMx_nLev))

         CASE ('cmaq')
           call micromet( temp=t(i,j,1), temp0=Tsfc(i,j,1), press=P(i,j,1), press0=Psfc(i,j,1), &
                        & deltaz=HGT(i,j,1), wind=sqrt(U(i,j,1)**2+V(i,j,1)**2), &
                        & z0=sfcROUGH(i,j,1), pbl=PBL(i,j,1), ustar=ustar, eli=MOLenI, wstar=wstar)
!tmpFile wind_tmp(i,j)=sqrt(U(i,j,1)**2+V(i,j,1)**2)
!tmpFile ustar_tmp(i,j)=ustar
!tmpFile MOLenI_tmp(i,j)=MOLenI
!tmpFile wstar_tmp(i,j)=wstar
           call kv_cmaq( CAMx_nLev, PBL(i,j,1), ustar, MOLenI, wstar, &
                       & HGT(i,j,1:CAMx_nLev), HGT_I(i,j,1:CAMx_nLev), thetav(1:CAMx_nLev), &
                       & uwind(1:CAMx_nLev), vwind(1:CAMx_nLev), kvmin, rkv(i,j,1:CAMx_nLev))

         CASE ('tke')
           sumtkz = 0.
           sumtk = 0.
           xinf = 0.
           do k = 1, CAMx_nLev
               if (TKE(i,j,k) > 1.e-6) then
                   dz = HGT_I(i,j,k)
                   if (k > 1) dz = HGT_I(i,j,k) - HGT_I(i,j,k-1)
                   eee = sqrt(2.*tke(i,j,k)) * dz
                   sumtk = sumtk + eee
                   sumtkz = sumtkz + eee*HGT(i,j,k)
               end if
           end do

           if (sumtk.gt.0.) xinf = sumtkz/sumtk
           xinf = 0.1*xinf
           if (thetav(2).lt.thetav(1)) xinf = 0.075*xinf
           xinf = min(xinf,2000.)
           xinf = max(xinf,5.)
           call kv_tke(nz=CAMx_nLev, tv=thetav(1:CAMx_nLev), uu=uWind(1:CAMx_nLev), vv=vWind(1:CAMx_nLev), &
                       zh=HGT(i,j,1:CAMx_nLev), zf=HGT_I(i,j,1:CAMx_nLev), xinf=xinf, &
                       tke=TKE(i,j,1:CAMx_nLev), kvmin=kvmin, kv=rkv(i,j,1:CAMx_nLev))

         CASE ('acm2')
           call kv_acm2(nz=CAMx_nLev, zz=HGT_I(i,j,1:CAMx_nLev), uwind=uWind(1:CAMx_nLev), vwind=vWind(1:CAMx_nLev), &
                        temp=T(i,j,1:CAMx_nLev), qvap=WV(i,j,1:CAMx_nLev), &
                        qc=cldwtr(i,j,1:CAMx_nLev)*1000.*rho(i,j,1:CAMx_nLev), &
                        press=P(i,j,1:CAMx_nLev), temps=Tsfc(i,j,1), z0=sfcROUGH(i,j,1), &
                        pbl=PBL(i,j,1), kvmin=kvmin, rkv=rkv(i,j,1:CAMx_nLev))

         CASE DEFAULT
           STOP'Unknown vertical diffusivity method - see kv_method in "aladin2camx.inc" file'
         END SELECT

       END DO lat
     END DO lon

     ! ** CLOUD OPTICAL DEPTH **
     DO i=1,XEND
         DO j=1,YEND

             SELECT CASE (trim(cod_method))
             CASE ('chimere')
                 call COD_chimere(nk=CAMx_nLev, hiCol  = HGT_I(i,j,1:CAMx_nLev), cwtrCol=cldwtr(i,j,1:CAMx_nLev), &
                                                rwtrCol=ranwtr(i,j,1:CAMx_nLev), swtrCol=snowtr(i,j,1:CAMx_nLev), &
                                                RhoCol =   rho(i,j,1:CAMx_nLev), rhCol  =    rh(i,j,1:CAMx_nLev), &
                                                codCol =   cod(i,j,1:CAMx_nLev))

             CASE('wrfcamx')
                 call COD_wrfcamx(nk=CAMx_nLev, hiCol = HGT_I (i,j,1:CAMx_nLev), rhoCol =    rho(i,j,1:CAMx_nLev), &
                                                qlCol = cldwtr(i,j,1:CAMx_nLev), qiCol  = icewtr(i,j,1:CAMx_nLev), &
                                                qrCol = ranwtr(i,j,1:CAMx_nLev), qsCol  = snowtr(i,j,1:CAMx_nLev), &
                                                qgCol = (/(0.,ii=1,CAMx_nLev)/), codCol =    cod(i,j,1:CAMx_nLev))
             CASE DEFAULT
                 STOP'Unknown cloud optical depth method - see cod_method in "aladin2camx.inc" file'
             END SELECT
                 
         END DO
     END DO


     ! * * * * * * * * * * * * * * * * *
     !                                 *
     ! final conversion to CAMx units  *
     !                                 *
     ! * * * * * * * * * * * * * * * * *

     ! convert pressure from Pa to mb
     P = P * .01

     ! convert ranwtr, cldwtr, and snowtr from kg/kg to g/m3 (multiply by 1000 and air density)
     ranwtr = ranwtr * 1000. * rho
     cldwtr = cldwtr * 1000. * rho
     snowtr = snowtr * 1000. * rho

     ! **WATER WAPOR** in ppm
     WV = WV * 1.e6 * MR_DryAir/MR_H2O 


     ! * * * * * * * * * * * * * * * * *
     ! write to log file               *
     ! * * * * * * * * * * * * * * * * *
     WRITE(logFileUnit,*)
     WRITE(logFileUnit,"('LOCAL date YYYYMMDD= ',I8.8,'  time HHMI= ',F5.0)")aladin_met(d)%LT_YYYYMMDD,  aladin_met(d)%LT_HHMI 
     WRITE(logFileUnit,"('LOCAL date YYJJJ   = ',I5.5)")aladin_met(d)%LT_YYJJJ 
     WRITE(logFileUnit,"(29X,' lev       min      grid              max      grid  profile ',I3,';',I3)") vpx,vpy
     WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'Solar radiation          [W/m2 ] ', &
       0,minval(SolRad(:,:,1)),minloc(SolRad(:,:,1)),maxval(SolRad(:,:,1)),maxloc(SolRad(:,:,1)),SolRad(vpx,vpy,1)
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'height               [m AGL] ', &
              k,minval(HGT_I(:,:,k)),minloc(HGT_I(:,:,k)),maxval(HGT_I(:,:,k)),maxloc(HGT_I(:,:,k)),HGT_I(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'pressure             [mb]    ', &
              k,minval(P(:,:,k)),minloc(P(:,:,k)),maxval(P(:,:,k)),maxloc(P(:,:,k)),P(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'u Wind               [m/s]   ', &
              k,minval(U(:,:,k)),minloc(U(:,:,k)),maxval(U(:,:,k)),maxloc(U(:,:,k)),U(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'v Wind               [m/s]   ', &
              k,minval(V(:,:,k)),minloc(V(:,:,k)),maxval(V(:,:,k)),maxloc(V(:,:,k)),V(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'sfc temperature  [K]     ', &
              0,minval(Tsfc(:,:,1)),minloc(Tsfc(:,:,1)),maxval(Tsfc(:,:,1)),maxloc(Tsfc(:,:,1)),tsfc(vpx,vpy,1)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",2F17.2)')'temp [K] + PotVirtTemp profile ', &
              k,minval(T(:,:,k)),minloc(T(:,:,k)),maxval(T(:,:,k)),maxloc(T(:,:,k)),T(vpx,vpy,k),thetavprof(k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'water vapor          [ppm]   ', &
              k,minval(WV(:,:,k)),minloc(WV(:,:,k)),maxval(WV(:,:,k)),maxloc(WV(:,:,k)),WV(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'cloud water          [g/m3]  ', &
              k,minval(CLDWTR(:,:,k)),minloc(CLDWTR(:,:,k)),maxval(CLDWTR(:,:,k)),maxloc(CLDWTR(:,:,k)),CLDWTR(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'rain water           [g/m3]  ', &
              k,minval(RANWTR(:,:,k)),minloc(RANWTR(:,:,k)),maxval(RANWTR(:,:,k)),maxloc(RANWTR(:,:,k)),RANWTR(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'snow water           [g/m3]  ', &
              k,minval(SNOWTR(:,:,k)),minloc(SNOWTR(:,:,k)),maxval(SNOWTR(:,:,k)),maxloc(SNOWTR(:,:,k)),SNOWTR(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'cloud optical depth  [-]     ', &
              k,minval(COD(:,:,k)),minloc(COD(:,:,k)),maxval(COD(:,:,k)),maxloc(COD(:,:,k)),COD(vpx,vpy,k)
     END DO
     WRITE(logFileUnit,*)
     DO k=1, CAMx_nLev
         WRITE(logFileUnit,'(A,I4,F10.2," [",I3,";"I3,"]",F17.2," [",I3,";"I3,"]",F17.2)') 'vertical siffusivity [m2/s]  ', &
              k,minval(RKV(:,:,k)),minloc(RKV(:,:,k)),maxval(RKV(:,:,k)),maxloc(RKV(:,:,k)),RKV(vpx,vpy,k)
     END DO

!tmpFile write(logfileUnit,*)'ustar',minval(ustar_tmp(:,:)),minloc(ustar_tmp(:,:)),maxval(ustar_tmp(:,:)),maxloc(ustar_tmp(:,:))
!tmpFile write(logfileUnit,*)'wstar',minval(wstar_tmp(:,:)),minloc(wstar_tmp(:,:)),maxval(wstar_tmp(:,:)),maxloc(wstar_tmp(:,:))
!tmpFile write(logfileUnit,*)'moLenI',minval(MOLenI_tmp(:,:)),minloc(MOLenI_tmp(:,:)),maxval(MOLenI_tmp(:,:)),maxloc(MOLenI_tmp(:,:))
!tmpFile do k= 1, CAMx_nLev
!tmpFile   write(logfileUnit,*)'thetav',k,minval(thetav_tmp(:,:,k)),minloc(thetav_tmp(:,:,k)),maxval(thetav_tmp(:,:,k)),maxloc(thetav_tmp(:,:,k))
!tmpFile end do
!tmpFile write(logfileUnit,*)'wind',minval(wind_tmp(:,:)),minloc(wind_tmp(:,:)),maxval(wind_tmp(:,:)),maxloc(wind_tmp(:,:))
!tmpFile write(logfileUnit,*)'pbl',minval(pbl_tmp(:,:)),minloc(pbl_tmp(:,:)),maxval(pbl_tmp(:,:)),maxloc(pbl_tmp(:,:))
!tmpFile 
!tmpFile do k=1,CAMx_nLev
!tmpFile   write(logfileUnit,*)'hgt',minval(hgt_tmp(:,:,k)),minloc(hgt_tmp(:,:,k)),maxval(hgt_tmp(:,:,k)),maxloc(hgt_tmp(:,:,k))
!tmpFile end do
!tmpFile 
!tmpFile do k=1,CAMx_nLev
!tmpFile   write(logfileUnit,*)'hgt_i',minval(hgt_i_tmp(:,:,k)),minloc(hgt_i_tmp(:,:,k)),maxval(hgt_i_tmp(:,:,k)),maxloc(hgt_i_tmp(:,:,k))
!tmpFile end do
!tmpFile 
!tmpFile do k=1,CAMx_nLev
!tmpFile   write(logfileUnit,*)'uwind',minval(uwind_tmp(:,:,k)),minloc(uwind_tmp(:,:,k)),maxval(uwind_tmp(:,:,k)),maxloc(uwind_tmp(:,:,k))
!tmpFile end do
!tmpFile do k=1,CAMx_nLev
!tmpFile   write(logfileUnit,*)'vwind',minval(vwind_tmp(:,:,k)),minloc(vwind_tmp(:,:,k)),maxval(vwind_tmp(:,:,k)),maxloc(vwind_tmp(:,:,k))
!tmpFile end do
     WRITE(logFileUnit,*)
     WRITE(logFileUnit,*)


     ! * * * * * * * * * * * * * * * * *
     ! write to CAMx files             *
     ! * * * * * * * * * * * * * * * * *

     WRITE(tp_unit(g)) aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, & 
       &((Tsfc(i,j,1),i=1,XEND),j=1,YEND)

     WRITE(uv_unit(g)) aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ,.TRUE.

     ! if first write out, then writeout header information and cloud rain data
     IF(d==1) WRITE(cr_unit(g)) cldhdr,XEND,YEND,CAMx_nLev
     WRITE(cr_unit(g)) aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ

     DO k=1,CAMx_nLev
     
         WRITE(zp_unit(g))  aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, ((HGT_I(i,j,k),i=1,XEND),j=1,YEND)
         WRITE(zp_unit(g))  aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, ((P(i,j,k)    ,i=1,XEND),j=1,YEND)
               
         WRITE(tp_unit(g)) aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, ((T(i,j,k)    ,i=1,XEND),j=1,YEND)

         WRITE(qa_unit(g))   aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, ((WV(i,j,k)   ,i=1,XEND),j=1,YEND)

         WRITE(kv_unit(g))  aladin_met(d)%LT_HHMI,aladin_met(d)%LT_YYJJJ, ((rkv(i,j,k)  ,i=1,XEND),j=1,YEND)

         WRITE(uv_unit(g)) ((U(i,j,k),i=1,XEND),j=1,YEND)
         WRITE(uv_unit(g)) ((V(i,j,k),i=1,XEND),j=1,YEND)

         WRITE(cr_unit(g))   ((CLDWTR(i,j,k),i=1,XEND),j=1,YEND)
         WRITE(cr_unit(g))   ((RANWTR(i,j,k),i=1,XEND),j=1,YEND)
         WRITE(cr_unit(g))   ((SNOWTR(i,j,k),i=1,XEND),j=1,YEND)
         WRITE(cr_unit(g))   ((0            ,i=1,XEND),j=1,YEND) ! graupel are set to zero
         WRITE(cr_unit(g))   ((COD(i,j,k)   ,i=1,XEND),j=1,YEND)


     END DO   

     WRITE(uv_unit(g)) ((0.,i=1,XEND),j=1,YEND) ! dummy array for wind file

!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((rho(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((uwind_tmp(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((vwind_tmp(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((thetav_tmp(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile write(tmp_unit,rec=irec)((ustar_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((wstar_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((MOLenI_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((1./MOLenI_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((wind_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((pbl_tmp(i,j),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((hgt_tmp(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((hgt_i_tmp(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do
!tmpFile write(tmp_unit,rec=irec)((psfc(i,j,1),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile write(tmp_unit,rec=irec)((sfcrough(i,j,1),i=1,XEND),j=1,YEND)
!tmpFile irec=irec+1
!tmpFile DO k=1,CAMx_nLev
!tmpFile   write(tmp_unit,rec=irec)((tke(i,j,k),i=1,XEND),j=1,YEND)
!tmpFile   irec=irec+1
!tmpFile end do

     IF (ALLOCATED(P        )) DEALLOCATE(P        )
     IF (ALLOCATED(Psfc     )) DEALLOCATE(Psfc     )
     IF (ALLOCATED(HGT_I    )) DEALLOCATE(HGT_I    )
     IF (ALLOCATED(HGT      )) DEALLOCATE(HGT      )
     IF (ALLOCATED(U        )) DEALLOCATE(U        )
     IF (ALLOCATED(V        )) DEALLOCATE(V        )
     IF (ALLOCATED(TKE      )) DEALLOCATE(TKE      )
     IF (ALLOCATED(T        )) DEALLOCATE(T        )
     IF (ALLOCATED(Tsfc     )) DEALLOCATE(Tsfc     )
     IF (ALLOCATED(Q        )) DEALLOCATE(Q        )
     IF (ALLOCATED(Rh       )) DEALLOCATE(Rh       )
     IF (ALLOCATED(WV       )) DEALLOCATE(WV       )
     IF (ALLOCATED(rkv      )) DEALLOCATE(rkv      )
     IF (ALLOCATED(ranwtr   )) DEALLOCATE(ranwtr   )
     IF (ALLOCATED(snowtr   )) DEALLOCATE(snowtr   )
     IF (ALLOCATED(cldwtr   )) DEALLOCATE(cldwtr   )
     IF (ALLOCATED(icewtr   )) DEALLOCATE(icewtr   )
     IF (ALLOCATED(COD      )) DEALLOCATE(COD      )
     IF (ALLOCATED(rho      )) DEALLOCATE(rho      )
     IF (ALLOCATED(PBL      )) DEALLOCATE(PBL      )
     IF (ALLOCATED(sfcROUGH )) DEALLOCATE(sfcROUGH )
     IF (ALLOCATED(SolRad   )) DEALLOCATE(SolRad   )

 END DO grid
     
 RETURN
     
END SUBROUTINE get_h_p_t_wv
