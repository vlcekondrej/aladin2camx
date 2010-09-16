      subroutine kv_cmaq(nz,pbl,ustar,moli,wstar,zh,zf,thetav, &
                         uwind,vwind,kz0ut,eddyv)
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
      implicit none
!
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
      real :: fka,fkc,fkd,fke,free
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
      end subroutine kv_cmaq
!
!--------------------------------------------------------------------------
!
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
