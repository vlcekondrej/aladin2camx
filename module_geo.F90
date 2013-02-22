module module_geo
 use gctp2
 implicit none

 ! projection related variables
 real*8, dimension(2) :: CRDproj, CRDgeo
 integer*4            :: projSYS, projZONE, geoSYS, geoZONE
 real*8, dimension(15):: TPARproj, TPARGEO
 integer*4            :: projUNIT, projSPH, IPR, JPR, LEMSG, LPARM, geoUNIT,geoSPH
 integer*4            :: LN27, LN83, LENGTH, stat
 character*128        :: FN27, FN83
 NAMELIST /ProjSpec/ projSYS, projZONE, TPARproj, projUNIT, projSPH

contains

 subroutine initGeo()
  implicit none
  ! GCTP2 parameters; Earth radius for geographical projection is the same as for input projection
  ! IPR flag for printing error messages.
  !     = 0 ... error messages will be printed on logical unit LEMSG.
  !     <>0 ... error messages will not be printed.
  IPR   = 0
  LEMSG = 6
  ! JPR printout flag for printing projection parameters.
  !     = 0 ... projection parameters will be printed on logical unit LPARM
  !     <>0 ... projection parameters will not be printed.
  JPR   = 0
  LPARM = 6
  ! specify geographical projection in according to input one
  TPARgeo      = 0D0
  TPARgeo(1:2) = TPARproj(1:2)
  geoSPH       = projSPH
  geoSYS       = 0
  geoZONE      = 99999
  geoUNIT      = 4
 end subroutine initGeo

 subroutine geo2proj()
  implicit none
  call GTPZ0(CRDIN=CRDgeo, INSYS=geoSYS,INZONE=geoZONE,TPARIN=TPARgeo,INUNIT=geoUNIT,INSPH=geoSPH, &
       &     IPR=IPR, JPR=JPR, LEMSG=LEMSG, LPARM=LPARM, &
       &     CRDIO=CRDproj,IOSYS=projSYS,IOZONE=projZONE,TPARIO=TPARproj,IOUNIT=projUNIT, &
       &     LN27=LN27,LN83=LN83, FN27=FN27,FN83=FN83,LENGTH=LENGTH,IFLG=stat)
  if (stat/=0) then
    write(LEMSG,*)'__GTPZ0: Error occured; IFLG = ',stat,'Check manual'
    stop
  end if
 end subroutine geo2proj


 subroutine proj2geo()
  implicit none
  call GTPZ0(CRDIN=CRDproj, INSYS=projSYS,INZONE=projZONE,TPARIN=TPARproj,INUNIT=projUNIT,INSPH=projSPH, &
       &     IPR=IPR, JPR=JPR, LEMSG=LEMSG, LPARM=LPARM, &
       &     CRDIO=CRDgeo,IOSYS=geoSYS,IOZONE=geoZONE,TPARIO=TPARgeo,IOUNIT=geoUNIT, &
       &     LN27=LN27,LN83=LN83, FN27=FN27,FN83=FN83,LENGTH=LENGTH,IFLG=stat)
  if (stat/=0) then
    write(LEMSG,*)'__GTPZ0: Error occured; IFLG = ',stat,'Check manual'
    stop
  end if
 end subroutine proj2geo

 pure real*8 function dms2deg(dms) result deg
  implicit none
  real*8, intent(in   ) :: deg
  real*8                :: dms

  dms = 0D0
  dms = deg
 end function dddmmmsss2deg

 pure real*8 function deg2dms(deg) result dms
  implicit none
  real*8, intent(in   ) :: deg
  real*8                :: dms

  dms = 0D0
  dms = deg
 end function deg2dddmmmsss
end module module_geo
