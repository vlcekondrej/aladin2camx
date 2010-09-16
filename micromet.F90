      subroutine micromet(temp,temp0,press,press0,deltaz,wind,z0,pbl, &
                          ustar,eli,wstar)
      use module_standard_types ! remove _sp and (kind=sp) if module not used
!      use module_physical_constants, only: g=>grav, gamma=>R_DryAir/cp_DryAir
      use module_physical_constants, only: g=>grav, R_DryAir, cp_DryAir
! 
!     MICROMET calculates *surface* layer micro-meteorological flux-gradient;
!     Relationships and variables based on Louis (1979):
!        Louis J-F, 1979. A parametric model of vertical eddy fluxes in the atmosphere. Boundary-Layer Meteorology 17, 187–202

!    Numbers in brackets refer to Louis 1979.

!  
!     Input arguments:  
!        temp                Layer 1 temperature      [K] 
!        temp0               Surface temperature      [K]
!        press               Layer 1 pressure         [Pa]
!        press0              Surface pressure         [Pa]
!        deltaz              Layer 1 midpoint height  [m]
!        wind                Layer 1 total wind speed [m/s]
!        z0                  Surface roughness length [m]
!        pbl                 PBL depth                [m]
!             
!     Output arguments:  
!        ustar               friction velocity            [m/s]
!        eli                 inverse Monin-Obuchov length [1/m]
!        wstar               convective velocity scale    [m/s]
!             
      implicit none
!
      real(kind=sp), intent(in   ) :: temp,temp0,press,press0,deltaz,wind,z0,pbl
      real(kind=sp), intent(  out) :: ustar,eli,wstar

      real(kind=sp) :: theta,theta0,dtheta,thetabar,ri,zscale,cm,ch,fm,fh, &
                    &  ustar2,thstar,el,elabs
!      real(kind=sp), parameter :: g=9.8_sp, gamma=0.286_sp
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
 