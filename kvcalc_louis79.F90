subroutine kv_l79(nz,t,u,v,h,h_i,xinf,kvmin,kv)!,lprnt)
 ! Purpose:
 !   Determines vertical diffusivity coefficient Kv at a given layer interface using the methodology of 
 !   Louis J-F, 1979. A parametric model of vertical eddy fluxes in the atmosphere. Boundary-Layer Meteorology 17, 187–202
 !   Assumes that Kv follows diffusivity for heat (Kh)
 
 !   Numbers in brackets refer to Louis 1979.
 
 USE Inter_Faces,only: diagtur2
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

!-----------------------------------------------------------------------

subroutine DIAGTUR2(xl,zz,dz,duv,dtemp,sh)
 ! Purpose:
 !   Nondimensional eddy diffusivities (sm,sh) from Louis (1979)

 ! Revisions: 
 !   Date        Programmer      Change
 !   ====        ==========      ======
 !   ??          ??              original code
 !
 !   2009-08-19  Ondrej Vlcek    cc1 = (zz+dz/zz)**0.333 
 !                               ( according to Louis 1979; original code: ((zz+dz)/zz)**0.333)

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
