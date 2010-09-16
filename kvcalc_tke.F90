      subroutine kv_tke(nz,tv,uu,vv,zh,zf,xinf,tke,kvmin,kv)
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
!
!-----------------------------------------------------------------------
!
      subroutine DIAGTUR(tke,xl,duv,dtemp,sm,sh)
!      
!-----Nondimensional eddy diffusivities (sm,sh) from Mellor-Yamada
!     level 2.5 scheme modified for the case of growing turbulence
!     by Helfand and Labraga (1995,JAS,v.45,p.113-132)

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
