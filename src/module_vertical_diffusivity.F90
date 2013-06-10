MODULE module_vertical_diffusivity 

IMPLICIT NONE

CONTAINS


 subroutine kv_acm2(nz,zz,uwind,vwind,temp,qv,qc,press,temps,z0, &
                    pbl,kvmin,rkv)
  !-------------------------------------------------------------------------------
  !     USES micromet
  !
  !     Determines Kv using the CMAQ ACM2 methodology. This is a hybrid
  !     of the Holtslag and Boville (1993) and Liu and Carroll (1996)
  !     approaches.
  !
  !     Based on Models-3/CMAQ eddyx.F,v 1.5 2006/09/22 18:16:37
  !     John Pleim, NOAA/ARL:
  !     Journal of Applied Meteorogy and Climate, vol 46, 2007.
  !
  !     Arguments:
  !       nz     number of layers
  !       zz     layer interface height (m)
  !       uwind  U-wind component (m/s)
  !       vwind  V-wind component (m/s)
  !       temp   temperature (K)
  !       qv     water vapor (kg/kg) (puvodne bylo na vstupu qvap v ppm a to se prevadelo do qv v kg/kg; prevod jsem vykomentoval )
  !       qc     cloud water (g/m3)
  !       press  pressure (Pa) (puvodne byl na vstupu tlak v mb - upravil jsem prepocet pro thetav)
  !       temps  surface temperature (K)
  !       z0     surface roughness (m)
  !       pbl    PBL height (m)
  !       kvmin  Minimum Kv value (m2/s)
  !       rkv    diffusivity (m2/s)
  !
  !-------------------------------------------------------------------------------
  implicit none

  integer,             intent(in   ) :: nz
  real,                intent(in   ) :: pbl,z0,temps,kvmin
  real, dimension(nz), intent(in   ) :: zz,uwind,vwind,temp,qv,qc,press
  real, dimension(nz), intent(  out) :: rkv

  logical :: lstable
  integer :: k,kpbl
  real :: tv,dpdz,press0,ustar,el,psih,wstar,hoverl,rkhb, &
          zoverl,phih,dwdz,dwdz2,dtvdz,tvavg,rib,qavg,tavg,xlv,alph, &
          cpair,chi,zk,sql,rklc
  real :: wind(100),thetav(100),zm(100),dzm(100)
 
  real, parameter :: gamma=0.286, vk=0.4, grav=9.8, rdry=287., &
                     rlam=80., ric=0.25, rvap=461.

  !
  !-----Get the PBL parameters
  ! 
      do k = 1,nz
         wind(k) = sqrt(uwind(k)**2 + vwind(k)**2)
         !qv(k) = (qvap(k)/1.e6)*18./28.8
         tv = temp(k)*(1. + 0.608*qv(k))
         thetav(k) = tv*(100000./press(k))**gamma
      enddo
      zm(1) = zz(1)/2.
      do k = 2,nz
         zm(k)  = (zz(k) + zz(k-1))/2.
         dzm(k-1) = zm(k) - zm(k-1)
      enddo
      dzm(nz) = 0.
 
      do k = 2,nz
         if (zz(k).gt.pbl) then
           kpbl = k-1
           goto 10
         endif
      enddo
      kpbl = nz - 1
  10  continue

      dpdz  = (press(2) - press(1))/dzm(1)
      press0 = press(1) - dpdz*zm(1)
      call micromet(temp(1),temps,press(1),press0,zm(1), &
                    wind(1),z0,pbl,ustar,el,wstar)
      hoverl = pbl/el

  ! 
  !-----Kv profile: Holtslag and Boville (1993)
  ! 
      do k = 1,nz-1
         rkhb = kvmin
         if (k .lt. kpbl) then
            zoverl = zz(k)/el
            if (zoverl .lt. 0.) then
               if (zz(k) .lt. 0.1*pbl) then
                  phih = 1./sqrt(1. - 15.*zoverl)
               else
                  phih = 1./sqrt(1. - 1.5*hoverl)
               endif
            elseif (zoverl .lt. 1.) then
               phih = 1. + 5.*zoverl
            else
               phih = 5. + zoverl
            endif
            rkhb = vk*(ustar/phih)*zz(k)*(1. - zz(k)/pbl)**2
            rkhb = max(rkhb,kvmin)
         endif

         !
         !-----Liu and Carroll (1996)
         !
         dwdz = (wind(k+1) - wind(k))/dzm(k) + 1.e-10
         dwdz2 = dwdz*dwdz
         dtvdz = (thetav(k+1) - thetav(k))/dzm(k)
         tvavg = (thetav(k+1) + thetav(k))/2.
         rib = grav*dtvdz/(dwdz2*tvavg)
      
         if ((qc(k) .gt. 1.e-5) .and. (qc(k+1) .gt. 1.e-5)) then
            qavg = (qv(k+1) + qv(k))/2.
            tavg = (temp(k+1) + temp(k))/2.
            xlv = (2.501 - 0.00237*(tavg - 273.15))*1.0e6
            alph = xlv*qavg/rdry/tavg
            cpair = 1004.67*(1. + 0.84*qv(k))
            chi = xlv*xlv*qavg/(cpair*rvap*tavg*tavg)
            rib = (1. + alph)*(rib - grav*grav/(dwdz2*tavg*cpair)* &
                  ((chi - alph)/(1. + chi)))
         endif

         zk = vk*zz(k)
         sql = (zk*rlam/(rlam + zk))**2
         if (rib .ge. ric) then
            rklc = kvmin
         elseif (rib .ge. 0.) then
            rklc = kvmin + sql*sqrt(dwdz2)*(1. - rib/ric)**2
         else
            rklc = kvmin + sql*sqrt(dwdz2*(1. - 25.*rib))
         endif

         ! 
         !-----Choose a Kv value
         !
         rkv(k) = rklc
         if (k .lt. kpbl .and. (rkhb .gt. rklc .or. zoverl .gt. 0.)) &
           rkv(k) = rkhb
         rkv(k) = min(1000.,vk*zz(k),rkv(k))
      enddo
      rkv(nz) = 0.

      return
 end subroutine kv_acm2



 subroutine kv_cmaq(nz,pbl,ustar,moli,wstar,zh,zf,thetav, uwind,vwind,kz0ut,eddyv)
  !-------------------------------------------------------------------------------
  !
  !     prevzato z preprocesoru WRF-CAMx v2.1
  !
  !-----Determines Kv profile using the integration methodology employed
  !     in CMAQ (EPA, 1999; Byun et al., 1999).  
  !
  !     Summary of Cases:
  !     Case 1 : 0 <= ZL < ZU  < ZSL < PBL (A,B,C)
  !     Case 2 : 0 <= ZL < ZSL < ZU  < PBL (A,B,C) + (D,E)
  !     Case 3 : 0 <= ZL < ZSL < PBL < ZU  (A,B,C) + (D,E) + (F)
  !     Case 4 : 0 < ZSL < ZL  < ZU  < PBL (D,E)
  !     Case 5 : 0 < ZSL < ZL  < PBL < ZU  (D,E)   + (F)
  !     Case 6 : 0 < ZSL < PBL < ZL  < ZU  (F)
  !     Where (A): stable surface layer formula
  !           (B): neutral surface layer formula
  !           (C): unstable surface layer formula
  !           (D): stable pbl formula
  !           (E): unstable mixed layer formula
  !           (F): formula for free atmosphere
  !
  !-------------------------------------------------------------------------------
  implicit none

  integer, intent(in   ) :: nz
  !     input parameters
  real, intent(in   ) :: pbl           ! pbl height [m]
  real, intent(in   ) :: ustar         ! friction velocity [m/s]
  real, intent(in   ) :: moli          ! inverse Monin-Obukhov Length [1/m]
  real, intent(in   ) :: wstar         ! convective velocity scale [m/s]
  real, intent(in   ) :: zh(nz)        ! mid-layer elevation [m]
  real, intent(in   ) :: zf(nz)        ! full layer elevation [m]
  real, intent(in   ) :: thetav(nz)    ! virtual potential temp [K]
  real, intent(in   ) :: uwind(nz)     ! x-direction winds [m/s]
  real, intent(in   ) :: vwind(nz)     ! y-direction winds [m/s]
  real, intent(in   ) :: kz0ut         ! minimum diffusivity [m**2/s]
  !     output parameters
  real, intent(  out) :: eddyv(nz)     ! eddy diffusivity [m**2/s]

  integer :: k,kftb,icase(nz)
  real :: dzl,ww2,ws2,rib,ric,rl,ru,zl,zu,head,arg1,beta,dzint
  real :: zsl,beta1,as,as2,as3,alpha
  real :: zint(nz)
  real, parameter :: karm=0.4, karmc=0.54, xlamb=1600., grav=9.8

  zsl = amin1(50.,0.1*pbl)
  arg1 = karmc*ustar                  !0.4*ustar/0.74
  kftb = nz - 1
  icase(nz) = 6

  dzint = 0.5*zf(1)
  zint(1) = dzint
  do k = 2,nz
    zint(k) = zf(k-1) + dzint
    dzint = zf(k) - zint(k)
  enddo

  !
  !-----Determine layer stability cases and bottom of free troposphere
  !
  do k = 1,nz-1

    if (zint(k).lt.zsl) then
      if (zint(k+1).lt.zsl) then
        icase(k) = 1
      elseif (zint(k+1).lt.pbl) then
        icase(k) = 2
      else
        icase(k) = 3
      endif
    elseif (zint(k).lt.pbl) then
      if (zint(k+1).lt.pbl) then
        icase(k) = 4
      else
        icase(k) = 5
      endif
    else
      icase(k) = 6
      kftb = min0(kftb,k)
    endif
  enddo

  !
  !-----Depending on the case, call appropriate routines
  !
  do k = 1,nz-1

    if (k.lt.kftb) then                    !within PBL

      if (moli.gt.0.) then                 !stable case

        beta1 = 6.345*moli
        beta  = pbl*beta1                  !4.7*pbl/(0.74*l)
        head  = 2.*arg1*pbl*pbl/beta
        as2   = (1. + beta)/beta
        as    = sqrt(as2)
        as3   = as*as2
        zl = zint(k)
        zu = zint(k+1)

        if (icase(k).eq.1) then
          eddyv(k) = arg1*fka(zl,zu,beta1)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        elseif (icase(k).eq.2) then
          rl = sqrt(amax1(1.e-5,1. - zsl/pbl))/as
          ru = sqrt(1. - zu/pbl)/as
          eddyv(k) = arg1*fka(zl,zsl,beta1) + &
                     head*fkd(rl,ru,as2,as3)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k))

        elseif (icase(k).eq.3) then
          rl = sqrt(amax1(1.e-5,1. - zsl/pbl))/as
          ru = 0.
          eddyv(k) = arg1*fka(zl,zsl,beta1) +  &
                     head*fkd(rl,ru,as2,as3) + &
                     free(pbl,zu,kz0ut)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k))

        elseif (icase(k).eq.4) then
          rl = sqrt(1. - zl/pbl)/as
          ru = sqrt(1. - zu/pbl)/as
          eddyv(k) = head*fkd(rl,ru,as2,as3)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k))

        elseif (icase(k).eq.5) then
          rl = sqrt(1. - zl/pbl)/as
          ru = 0.
          eddyv(k) = head*fkd(rl,ru,as2,as3) + &
                     free(pbl,zu,kz0ut)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k))

        else                               !  case 6
          eddyv(k) = kz0ut
        endif

      else                                 ! unstable case

        alpha = -9.*moli
        zl = zint(k)
        zu = zint(k+1)

        if (icase(k).eq.1) then
          eddyv(k) = arg1*fkc(zl,zu,alpha)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        elseif (icase(k).eq.2) then
          eddyv(k) = arg1*fkc(zl,zsl,alpha) +   &
                     fke(zsl,zu,karm,wstar,pbl)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        elseif (icase(k).eq.3) then
          eddyv(k) = arg1*fkc(zl,zsl,alpha) +      &
                     fke(zsl,pbl,karm,wstar,pbl) + &
                     free(pbl,zu,kz0ut)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        elseif (icase(k).eq.4) then
          eddyv(k) = fke(zl,zu,karm,wstar,pbl) 
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        elseif (icase(k).eq.5) then
          eddyv(k) = fke(zl,pbl,karm,wstar,pbl) + &
                     free(pbl,zu,kz0ut)
          eddyv(k) = eddyv(k)/(zu - zl)
          eddyv(k) = max(kz0ut,eddyv(k)) 

        else                               !  case 6
          eddyv(k) = kz0ut
        endif
      endif

    else                                   !free toposphere
      dzl = zh(k+1) - zh(k)
      ric = 0.257*dzl**0.175
      ww2 = (uwind(k+1) - uwind(k))**2 + (vwind(k+1) - vwind(k))**2
      ws2 = ww2/(dzl*dzl) + 1.e-9
 
      rib = 2.*grav*(thetav(k+1) - thetav(k))/ &
            (dzl*ws2*(thetav(k+1) + thetav(k)))
      rib = max(rib,0.)
 
      if ((rib-ric).ge.0.) then
        eddyv(k) = kz0ut
      else
        eddyv(k) = kz0ut + xlamb*sqrt(ws2)*(1. - rib/ric)
      endif
    endif

  enddo

  eddyv(nz) = 0.

  return

  contains

    real function fka(z1,z2,beta1)
     implicit none
     real, intent(in) ::  beta1,z1,z2
     fka = (z2 - z1 - log((beta1*z2 + 1.)/(beta1*z1 + 1.))/beta1)/beta1
     return
    end function fka
   
    real function fkc(z1,z2,alpha)
     implicit none
     real, intent(in) :: alpha,z1,z2
     fkc = ((3.*alpha*z2 - 2.)*sqrt(1. + alpha*z2)**3 -  &
            (3.*alpha*z1 - 2.)*sqrt(1. + alpha*z1)**3)/  &
           (7.5*alpha*alpha)
     return
    end function fkc
   
    real function fkd(x1,x2,as2,as3)
     implicit none
     real, intent(in) :: as2,as3,x1,x2
     real, parameter :: third=1./3., fifth=1./5.
     fkd = as3*(as2*fifth*(x1**5 - x2**5) + &
               (as2 - 1.)*(third*(x1**3 - x2**3) + x1 - x2 - &
           0.5*(log((1. + x1)/(1. - x1)) - log((1. + x2)/(1. - x2)))))
     return
    end function fkd
   
    real function fke(z1,z2,zk,zwst,zpbl)
     implicit none
     real, intent(in) :: z1,z2,zk,zpbl,zwst
     real, parameter :: third=1./3.
     fke = zk*zwst*(z2*z2*(0.5 - third*z2/zpbl) - &
                    z1*z1*(0.5 - third*z1/zpbl))
     return
    end function fke
   
    real function free(z1,z2,zk)
     implicit none
     real, intent(in) :: z1,z2,zk
     free = zk*(z2 - z1)
     return
    end function free
   
 end subroutine kv_cmaq



 subroutine micromet(temp,temp0,press,press0,deltaz,wind,z0,pbl, ustar,eli,wstar)
  !---------------------------------------------------------------------------------------
  !   USED by kv_acm2
  ! 
  !    MICROMET calculates *surface* layer micro-meteorological flux-gradient;
  !    Relationships and variables based on Louis (1979):
  !      Louis J-F, 1979. A parametric model of vertical eddy fluxes in the atmosphere. 
  !      Boundary-Layer Meteorology 17, 187–202
  !
  !    Numbers in brackets refer to Louis 1979.
  !
  !  
  !    Input arguments:  
  !       temp                Layer 1 temperature      [K] 
  !       temp0               Surface temperature      [K]
  !       press               Layer 1 pressure         [Pa]
  !       press0              Surface pressure         [Pa]
  !       deltaz              Layer 1 midpoint height  [m]
  !       wind                Layer 1 total wind speed [m/s]
  !       z0                  Surface roughness length [m]
  !       pbl                 PBL depth                [m]
  !             
  !    Output arguments:  
  !       ustar               friction velocity            [m/s]
  !       eli                 inverse Monin-Obuchov length [1/m]
  !       wstar               convective velocity scale    [m/s]
  !             
  !---------------------------------------------------------------------------------------
  use module_standard_types ! remove _sp and (kind=sp) if module not used
  ! use module_physical_constants, only: g=>grav, gamma=>R_DryAir/cp_DryAir
  use module_physical_constants, only: g=>grav, R_DryAir, cp_DryAir
  implicit none
  !
  real(kind=sp), intent(in   ) :: temp,temp0,press,press0,deltaz,wind,z0,pbl
  real(kind=sp), intent(  out) :: ustar,eli,wstar

  real(kind=sp) :: theta,theta0,dtheta,thetabar,ri,zscale,cm,ch,fm,fh, &
                &  ustar2,thstar,el,elabs
  ! real(kind=sp), parameter :: g=9.8_sp, gamma=0.286_sp
  real(kind=sp), parameter :: vk=0.4_sp, xinf=100._sp, elimin=.0001_sp, gamma=R_DryAir/cp_DryAir

  !
  !-----Entry point
  !
  !-----Calculate potential temperature and richardson number
  !
      theta = temp*(100000._sp/press)**gamma
      theta0 = temp0*(100000._sp/press0)**gamma
      dtheta = theta - theta0
      thetabar = (theta + theta0)/2.
      ri = (g/thetabar)*deltaz*dtheta/(wind**2 + 1.e-20) ! Louis 1979 (11);  average Ri for the first layer

  !
  !-----Determine stability functions
  !
      zscale = vk/alog(deltaz/z0)
      if (ri.le.0.) then
        ! unstable conditions
        cm = 69.56_sp*sqrt(deltaz/z0)*zscale**2
        ch = 49.82_sp*sqrt(deltaz/z0)*zscale**2
        fm = 1._sp - 9.4_sp*ri/(1._sp + cm*sqrt(abs(ri)))
        fh = 1._sp - 9.4_sp*ri/(1._sp + ch*sqrt(abs(ri)))
      else
        fm = 1._sp/((1._sp + 4.7_sp*ri)**2)
        fh = fm
      endif

  !
  !-----Calculate micromet variables
  !
      ustar2 = fm*(wind*zscale)**2
      ustar2 = amax1(1.e-20,ustar2)
      ustar = sqrt(ustar2)
      thstar = 1.35_sp*zscale**2*wind*dtheta*fh/ustar
      el = ustar2*temp/(vk*g*thstar + 1.e-20)
      eli = 1._sp/el
      eli = sign(max(abs(eli),elimin),eli)
      el = 1._sp/eli
      elabs = abs(el) 

      wstar = 0._sp
      if (el.lt.0.) wstar = (pbl*ustar**3._sp/(vk*elabs))**(1._sp/3._sp)

      return
 end subroutine micromet
 

 subroutine kv_l79(nz,t,u,v,h,h_i,xinf,kvmin,kv)!,lprnt)
  ! ----------------------------------------------------------------------------------------------------------
  !
  ! USES DIAGTUR2
  !
  ! Purpose:
  !   Determines vertical diffusivity coefficient Kv at a given layer interface using the methodology of 
  !   Louis J-F, 1979. A parametric model of vertical eddy fluxes in the atmosphere. Boundary-Layer Meteorology 17, 187–202
  !   Assumes that Kv follows diffusivity for heat (Kh)
  !
  !   Numbers in brackets refer to Louis 1979.
  !
  ! ----------------------------------------------------------------------------------------------------------
  USE module_physical_constants, ONLY: g=>grav
 
  implicit none
  integer              , intent(in   ) :: nz
  real, dimension(nz+1), intent(in   ) :: t,u,v,h ! mid-layer virtual potential temperature, x and y winds, and height
  real, dimension(nz)  , intent(in   ) :: h_i     ! upper layer interface height
  real,                  intent(in   ) :: xinf, kvmin  ! assymptotic mixing length, minimum vertical diffusivity
  real, dimension(nz)  , intent(  out) :: kv    ! vertical diffusivity
 
  real :: zz        ! height of the interface between layers i and i+1
  real :: dz        ! vertical distance between layers i and i+1
  real :: zlz       ! gravity over average temperature
  real :: dtemp
  real :: du,dv,duv ! u and v wind shear; total squared wind shear
  real :: xl        ! mixing length
  real :: sh
  integer :: k 
  
  
  ! real,save :: g=9.81
  real, parameter :: vonk=0.35 ! Von Karman constant
  
  
  do k=1,nz
      zz = h_i(k)
      dz = (h(k+1) - h(k)) ! vzdalenost stredu vrstev
      zlz = 2.*g/(t(k+1) + t(k))
      dtemp = -zlz*(t(k+1) - t(k))/dz
      du = (u(k+1) - u(k))/dz ! u wind shear
      dv = (v(k+1) - v(k))/dz ! v wind shear
      duv = du*du + dv*dv ! total squared wind shear 
      
      xl = vonk*zz/(1. + vonk*zz/xinf) ! mixing length according to (22)
      
      call diagtur2(xl,zz,dz,duv,dtemp,sh)
 
      kv(k) = sh * xl*xl * sqrt(duv) ! vertical diffusion coefficient according (21), where sh==F(Ri)
  
      kv(k) = max(kv(k),kvmin)
  end do      
  return
 end subroutine kv_l79


 subroutine DIAGTUR2(xl,zz,dz,duv,dtemp,sh)
  !-----------------------------------------------------------------------
  ! USED by kv_l79
  !
  ! Purpose:
  !   Nondimensional eddy diffusivities (sm,sh) from Louis (1979)

  ! Revisions: 
  !   Date        Programmer      Change
  !   ====        ==========      ======
  !   ??          ??              original code
  !
  !   2009-08-19  Ondrej Vlcek    cc1 = (zz+dz/zz)**0.333 
  !                               ( according to Louis 1979; original code: ((zz+dz)/zz)**0.333)
  !
  !-----------------------------------------------------------------------

  implicit none
 
  real,intent(in   ) :: xl,zz,dz,duv,dtemp
  real,intent(  out) :: sh
  
  real :: rich,cc1,cc2,ccc,she
  
        
  !-----Level 2
        
  rich = -dtemp/max(duv,1.e-11) ! Richardson number according to (23)
  if (rich.ge.0.) then ! stable conditions
    she = 1./(1.+4.7*rich)**2. ! following Louis (15) with b'=b/2=4.7
  else !unstable conditions
    ! cc1 = ((zz+dz)/zz)**0.333 ! original code from ZAMG
    cc1 = (zz+dz/zz)**0.333 ! now according to Louis; original code: ((zz+dz)/zz)**0.333
    cc1 = (cc1-1.)**1.5
    cc2 = 1./sqrt(zz*dz**3.)
    ccc = 49.8 * xl*xl * cc1 * cc2 ! the same as c in Louis (24); hence b=9.4, C* must be 5.3
    she = 1.-9.4*rich/(1.+ccc*sqrt(abs(rich))) ! Louis (14) with b=9.4
  endif
  sh = she
        
  return
 end subroutine DIAGTUR2


 subroutine kv_ob70(nz,kvmin,pbl,zz,tv,uu,vv,rkv)
  !-----------------------------------------------------------------------
  !
  ! Purpose:
  !   Determines Kv profile using the profile methodology of O'Brien (1970):
  !       O’Brien, J. J., 1970: A note on the vertical structure of the eddy exchange coefficient in the planetary boundary layer. J. Atmos. Sci., 27, 1213–1215 
  !   Kv at top of surface layer based on Louis (1979) relationships:
  !	 Louis J-F, 1979. A parametric model of vertical eddy fluxes in the atmosphere. Boundary-Layer Meteorology 17, 187–202
  !
  ! Revisions:
  !   Date        Programmer      Change
  !   ====        ==========      ======
  !   ??          ??              original code
  !
  !   2009-12-17  Ondrej Vlcek    dz = zz(2)*0.5 instead of (zz(2)-zz(1))/2 for
  !                               dz should represent distance between the mid-layers 
  !                               dz = zz(1)+(zz(2)-zz(1))/2 - zz(1)/2 = zz(2)/2
  !
  !-----------------------------------------------------------------------
  implicit none
  !
  integer            , intent(in   ) :: nz
  real               , intent(in   ) :: kvmin ! minimum vertical diffusivity [m2/s]
  real               , intent(in   ) :: pbl ! planetary boundary layer height [m]
  real, dimension(nz), intent(in   ) :: zz ! layer upper-interface height [m]
  real, dimension(nz), intent(in   ) :: tv ! temperature [K]
  real, dimension(nz), intent(in   ) :: uu,vv ! x and y winds [m/s]
  real, dimension(nz), intent(  out) :: rkv ! vertical diffusivity [m2/s]
 
  integer :: k
  real :: dkdzs,obk
  real :: dz,dtdz,tbar,dudz,dvdz,dwdz,ri,xl,sh,cc1,cc2,ccc
  real, parameter :: vonk=0.4, xinf=100., g=9.8, rktop=1.0
  !
  !-----Determine Kv at top of surface layer based on Louis (1979) relationships
  !
  ! dz   = (zz(2) - zz(1))
  dz   = zz(2)*0.5 ! dz ma predstavovat vzdalenost stredu 1. (prizemni) a 2. vrstvy
  dtdz = (tv(2) - tv(1))/dz
  tbar = (tv(2) + tv(1))/2.
  dudz = (uu(2) - uu(1))/dz
  dvdz = (vv(2) - vv(1))/dz
  dwdz = dudz*dudz + dvdz*dvdz
  ri = (g/tbar)*dtdz/(dwdz + 1.e-20)
  
  xl = vonk*zz(1)/(1. + vonk*zz(1)/xinf)
  if (ri.ge.0.) then
    sh = 1./(1. + 4.7*ri)**2
  else
    cc1 = (zz(1) + dz/zz(1))**0.333
    cc1 = (cc1 - 1.)**1.5
    cc2 = 1./sqrt(zz(1)*dz**3)
    ccc = 49.8*xl*xl*cc1*cc2
    sh = 1. - 9.4*ri/(1. + ccc*sqrt(abs(ri)))
  endif
  
  rkv(1) = sh*xl*xl*sqrt(dwdz)
  rkv(1) = amin1(rkv(1),0.4*zz(1))
  dkdzs = rkv(1)/zz(1)
  !
  !-----Determine Kv through PBL using profile method O'Brien (1970)
  !
  do k = 2,nz-1
    if (pbl.gt.zz(k)) then
      obk = rktop + ((pbl - zz(k))**2/(pbl - zz(1))**2)* &
                     (rkv(1) - rktop + (zz(k) - zz(1))*  &
                     (dkdzs + (2.*(rkv(1) - rktop)/      &
                     (pbl - zz(1)))))
      rkv(k) = amax1(obk,kvmin)
    else
      rkv(k) = kvmin
    endif
  enddo
  rkv(nz) = 0. 
  rkv(1) = amax1(rkv(1),kvmin)
  !
  return
 end subroutine kv_ob70
 


 subroutine kv_tke(nz,tv,uu,vv,zh,zf,xinf,tke,kvmin,kv)
  !-----------------------------------------------------------------------
  ! USES DIAGTUR
  !
  !     prevzato z preprocesoru WRF-CAMx v2.1
  !
  !     TKE, ktere asi bylo na vystupu z WRF:
  !       Mellor-Yamada-Janjic scheme: Eta operational scheme. One-dimensional
  !       prognostic turbulent kinetic energy scheme with local vertical mixing (2).
  !     ? Z ALADINa leze co ?
  !
  !-----Determines Kv at a given layer interface using the TKE methodology  
  !     employed in RAMS (Mellor and Yamada, 1974/1982; Helfand and Labraga,  
  !     1988).  Assumes that Kv follows diffusivity for heat (Kh)
  ! 
  !-----------------------------------------------------------------------
  implicit none
  integer,             intent(in   ) :: nz
  real, dimension(nz), intent(in   ) :: tv    ! virtual potential temperature [K]
  real, dimension(nz), intent(in   ) :: uu,vv ! u and v wind in cell centre   [m/s]
  real, dimension(nz), intent(in   ) :: zh    ! mid-layer elevation           [m AGL]
  real, dimension(nz), intent(in   ) :: zf    ! full-layer elevation          [m AGL]
  real,                intent(in   ) :: xinf  ! 
  real, dimension(nz), intent(in   ) :: tke
  real,                intent(in   ) :: kvmin
  real, dimension(nz), intent(  out) :: kv

  integer :: k
  real    :: dz,zlz,dtemp,du,dv,duv,xl,q,sm,sh
  real, parameter :: tkemin=1.e-6, g=9.81, vonk=0.4

  do k = 1,nz-1
    dz = (zh(k+1) - zh(k))
    zlz = 2.*g/(tv(k+1) + tv(k))
    dtemp = -zlz*(tv(k+1) - tv(k))/dz
    du = (uu(k+1) - uu(k))/dz
    dv = (vv(k+1) - vv(k))/dz
    duv = du*du + dv*dv

    xl = vonk*zf(k)/(1. + vonk*zf(k)/xinf)
    q = sqrt(2.*tke(k))
    if (dtemp.lt.0.) xl = min(xl,0.75*q/sqrt(-dtemp))

    call diagtur(tke(k),xl,duv,dtemp,sm,sh)
    kv(k) = sh*xl*q
    kv(k) = amax1(kv(k),kvmin)
  enddo
  kv(nz) = 0.
  
  return
 end subroutine kv_tke


 subroutine DIAGTUR(tke,xl,duv,dtemp,sm,sh)
  !-----------------------------------------------------------------------
  !
  ! USED by kv_tke   
  !  
  !-----Nondimensional eddy diffusivities (sm,sh) from Mellor-Yamada
  !     level 2.5 scheme modified for the case of growing turbulence
  !     by Helfand and Labraga (1995,JAS,v.45,p.113-132)
  !
  !-----------------------------------------------------------------------

  real, intent(in   ) :: tke
  real, intent(in   ) :: xl
  real, intent(in   ) :: duv
  real, intent(in   ) :: dtemp
  real, intent(  out) :: sm
  real, intent(  out) :: sh
 
  real, parameter :: a1=0.92, a2=0.74, b1=16.6, b2=10.1, c1=0.08, a16=6*a1

  real, parameter :: dd2 = a1*(1.-3.*c1),    &
                      d1 = 6.*a1*a2,          &
                      d2 = a2*(3.*b2+12.*a1), &
                      d3 = 6.*a1*a1,          &
                      d4 = 9.*a1*a2,          &
                      d5 = 2.*d3+d4
  
  real, parameter :: sh1 = a2*(3.*b2+b1+12.*a1)/b1, &
                     sm1 = a1*(-3.*c1*b1+b1+9.*a2+12.*a1)/(a2*(3.*b2+b1+3.*a1)), &
                     rf2 = (b1-6.*a1)/(3.*b2+b1+12.*a1), &
                     rf3 = (b1-6.*a1)/(3.*b2+b1+3.*a1), &
                     rf4 = (3.*c1*b1-b1+6.*a1)/(3.*c1*b1-b1-9.*a2-12.*a1)
  
  real, parameter :: tkemin2=2.e-7, cc2=-.5625

  real :: aa1, aa2, bb1, bb2, ccc, gh, gm, q2, q2qe2, qe2, qqq, rf, ri, she, sme, w, xlq

  q2 = 2.*tke
  xlq = xl*xl/q2
  
  gh = dtemp*xlq
  if (gh.lt.0.) gh = amax1(gh, cc2)
  gm = duv*xlq
  !
  !-----Level 2
  !      
  ri = -dtemp/amax1(duv,1.e-11)
  ri = amin1(ri,rf2)
  ccc = amax1(ri*(ri-0.3221)+0.03156,0.)
  rf = amin1(0.6588*(ri+0.1776-sqrt(ccc)),rf2)
  she = sh1*(rf-rf2)/(rf-1.)
  sme = sm1*(rf-rf4)/(rf-rf3)*she
  !
  !-----Equilibrium tke from diagnostic equation
  !
  qe2 = amax1(b1*xl*xl*(sme*duv+she*dtemp),tkemin2)
  q2qe2 = q2/qe2
  !
  !-----Growing turbulence case
  !
  if (qe2.gt.q2) then
    qqq = sqrt(q2qe2)
    sm = qqq*sme
    sh = qqq*she
  else
  !
  !-----Decaying turbulence case - level 2.5
  !
    aa1 = d1*gm
    bb1 = 1. - gh*d2
    aa2 = 1. + gm*d3 - gh*d4
    bb2 = -gh*d5
    w = aa1*bb2 - aa2*bb1
    sm = (bb2*a2 - bb1*dd2)/w
    sh = (aa1*dd2 - aa2*a2)/w
  endif
  
  return
 end subroutine diagtur

END MODULE module_vertical_diffusivity 
