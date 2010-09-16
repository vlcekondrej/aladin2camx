subroutine kv_ob70(nz,kvmin,pbl,zz,tv,uu,vv,rkv)
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

