      subroutine kv_acm2(nz,zz,uwind,vwind,temp,qv,qc,press,temps,z0, &
                         pbl,kvmin,rkv)
!
!-----Determines Kv using the CMAQ ACM2 methodology. This is a hybrid
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
 10   continue

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
