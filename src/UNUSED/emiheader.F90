SUBROUTINE emiheader()
! --------------------------------------------------------------------
! writes header for initial conditions file                           |
! see "Initial Conditions File" in CAMx manual                        |
! --------------------------------------------------------------------
 USE module_global_variables
 USE module_standard_types
! USE module_utils, ONLY: testStop
 USE grib_api

 IMPLICIT NONE

 INTEGER :: g,l
 CHARACTER(LEN=4) :: name(10)
 CHARACTER(LEN=4) :: note(60)
 INTEGER          :: ione=1      ! Dummy variable = 1
 REAL(KIND=sp)    :: rdum=0.0_sp ! dummy real variable
 INTEGER          :: iutm=0      ! UTM zone; ignored for other projections
 ! Grid x/y-origin at southwest corner of domain (m or degrees longitude/latitude); The mothergrid starts with 0.,0.
 REAL(KIND=sp)    :: xorg(ngridnumber),yorg(ngridnumber) 
 INTEGER          :: idum=0      ! Dummy integer variable
 INTEGER          :: izero=0
 INTEGER          :: nz

 ! initialize variables
 ! first character variables
 ! times and dates
 ! times are in local wintertime => utc+1
 data name /'A','I','R','Q','U','A','L','I','T','Y'/
 data note /'C','A','M','x',' ','I','N','P','U','T', &
          & '','','','','','','','','','', &
          & '','','','','','','','','','', &
          & '','','','','','','','','','', &
          & '','','','','','','','','','', &
          & '','','','','','','','','',''/


 xorg(1)=(nest_xstart(1)-2)*Alad_dx
 yorg(1)=(nest_ystart(1)-2)*Alad_dy

 DO g=2, ngridnumber
     xorg(g)=(nest_xstart(g)-1)*Alad_dx - xorg(1)
     yorg(g)=(nest_ystart(g)-1)*Alad_dy - yorg(1)
 END DO

 xorg(1)=0.
 yorg(1)=0.


 ! write out header to binary file
 nz=1
 DO g=1,ngridnumber
     WRITE(emission_unit(g)) name,note,ione,nspec, &
         & aladin_met(1)%LT_YYJJJ,aladin_met(1)%LT_HHMI/100., &                ! beg.date (YYJJJ), beg.time (HH)
         & aladin_met(nAladFiles)%LT_YYJJJ,aladin_met(nAladFiles)%LT_HHMI/100. ! end date (YYJJJ), end time (HH)
     WRITE(emission_unit(g)) rdum,rdum,iutm, &
         & xorg(g),yorg(g),CAMx_dx(g),CAMx_dy(g),CAMx_nx(g),CAMx_ny(g),nz, &
         & idum,idum,rdum,rdum,rdum
     WRITE(emission_unit(g)) izero,izero,CAMx_nx(g),CAMx_ny(g)
     WRITE(emission_unit(g)) (mspec(:,l),l=1,nspec)
 END DO

END SUBROUTINE emiheader
