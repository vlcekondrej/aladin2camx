MODULE module_smoothing_handler
 USE module_standard_types
 IMPLICIT NONE

 INTERFACE SMOOTHER
     module procedure smoother_2D, smoother_3D, smoother_2D_dp, smoother_3D_dp 
 END INTERFACE 
 !-------------------------------------------------------------------------------
 ! ... More passes requires a larger stencil (currently 48 pt)
 integer (kind=idflt),         parameter :: smooth_passes = 1 ! how many smoothing passes are to be done
 real(kind=sp), dimension(2), parameter :: xnu    = (/ 0.50_sp , -0.50_sp /)
 real(kind=dp), dimension(2), parameter :: xnu_dp = (/ 0.50_dp , -0.50_dp /)
 !-------------------------------------------------------------------------------

 private
 public :: smoother

CONTAINS

 !--------------------------
 ! == SINGLE PRESISSION == |
 !--------------------------
 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 subroutine Smoother_2D (field2D, method)
   real(kind=sp)      , dimension(:,:), pointer, intent(inout) :: field2D
   integer(kind=idflt),                          intent(in   ) :: method
 
   select case (method)
       case (1)
           call Smt121_2D (field2D)
       case (2)
           call Smther_2D (field2D)
   endselect
   return
 end  subroutine Smoother_2D


 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 subroutine Smoother_3D (field3D, method)
   real(kind=sp)      , dimension(:,:,:), pointer, intent(inout) :: field3D
   integer(kind=idflt),                            intent(in   ) :: method
 
   select case (method)
       case (1)
           call Smt121_3D (field3D)
       case (2)
           call Smther_3D (field3D)
   endselect
   return
 end  subroutine Smoother_3D


 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !-------------------------------------------------------------------------------
 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


 subroutine SMT121_2D (cfld)
   real(kind=sp), dimension(:,:), pointer, intent(inout) :: cfld

   real(kind=sp), dimension(size(cfld,1),size(cfld,2)) :: tmpfld
   integer (kind=idflt) :: i, is, ie, j, js, je, loop
 
   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...Simple 1-2-1 smoother.
   SMOOTHING_PASSES: DO loop=1,smooth_passes
       tmpfld = cfld
       ! ...1-2-1 smoothing in the j direction first,
       LOOP_J: do j=js,je
           tmpfld(:,j) = 0.25*(cfld(:,j+1) + 2.*cfld(:,j) + cfld(:,j-1))
       END DO LOOP_j
       ! ...then 1-2-1 smoothing in the i direction last
       LOOP_i: do i=is,ie
           cfld(i,:) = 0.25*(tmpfld(i+1,:)+2.*tmpfld(i,:)+tmpfld(i-1,:))
       END DO LOOP_i
   END DO smoothing_passes
   RETURN
 END  subroutine SMT121_2D
 

 !-------------------------------------------------------------------------------
 subroutine SMT121_3D (cfld)
   real(kind=sp), dimension(:,:,:), pointer, intent(inout) :: cfld

   real(kind=sp), dimension(size(cfld,1),size(cfld,2),size(cfld,3)) :: tmpfld
   integer (kind=idflt) :: i, is, ie, j, js, je, loop

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...Simple 1-2-1 smoother.
   SMOOTHING_PASSES: DO loop=1,smooth_passes
       tmpfld = cfld
       ! ...1-2-1 smoothing in the j direction first,
       LOOP_J: DO j=js,je
           tmpfld(:,j,:) = 0.25*(cfld(:,j+1,:) + 2.*cfld(:,j,:) + cfld(:,j-1,:))
       END DO LOOP_j
       ! ...then 1-2-1 smoothing in the i direction last
       LOOP_i: do i=is,ie
           cfld(i,:,:) = 0.25*(tmpfld(i+1,:,:) + 2.*tmpfld(i,:,:) + tmpfld(i-1,:,:))
       ENDDO LOOP_i
   END DO smoothing_passes
   RETURN
 END  subroutine SMT121_3D


 !-------------------------------------------------------------------------------
 subroutine SMTHER_2D (cfld)
   real(kind=sp), dimension(:,:), pointer, intent(inout) :: cfld

   real(kind=sp), dimension(size(cfld,1),size(cfld,2)) :: tmpfld
   integer(kind=idflt) :: i, is, ie, j, js, je, loop, n

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...The odd number passes (n=1) of this are the "smoother", the even number
   !    passes are the "de-smoother" (note the different signs on xnu).
   SMOOTHING_PASSES: DO loop=1,smooth_passes*2
       n = 2-MOD(loop,2)
       tmpfld = cfld
       ! ...1st: filter the j-direction
       LOOP_j:  DO j = js, je
           tmpfld(:,j) = cfld(:,j) + xnu(n)*( (cfld(:,j+1)+cfld(:,j-1))*0.5 - cfld(:,j) )
       END DO LOOP_j
       ! ...2nd: filter the i-direction
       LOOP_i:  DO i = is, ie
           cfld(i,:) = tmpfld(i,:) + xnu(n)*( (tmpfld(i+1,:)+tmpfld(i-1,:))*0.5 - tmpfld(i,:) )
       END DO LOOP_i
   END DO SMOOTHING_PASSES
   RETURN
 END  SUBROUTINE SMTHER_2D
 

 !-------------------------------------------------------------------------------
 subroutine SMTHER_3D (cfld)
   real(kind=sp), dimension(:,:,:), pointer, intent(inout) :: cfld

   real(kind=sp), dimension(size(cfld,1),size(cfld,2),size(cfld,3)) :: tmpfld
   integer(kind=idflt) :: i, is, ie, j, js, je, loop, n

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...The odd number passes (n=1) of this are the "smoother", the even number
   !    passes are the "de-smoother" (note the different signs on xnu).
   SMOOTHING_PASSES: DO loop=1,smooth_passes*2
       n = 2-MOD(loop,2)
       tmpfld = cfld
       ! ...1st: filter the j-direction
       LOOP_j:  DO j = js, je
           tmpfld(:,j,:) = cfld(:,j,:) + xnu(n)*( (cfld(:,j+1,:)+cfld(:,j-1,:))*0.5 - cfld(:,j,:) )
       END DO LOOP_j
       ! ...2nd: filter the i-direction
       LOOP_i:  do i = is, ie
           cfld(i,:,:) = tmpfld(i,:,:) + xnu(n)*( (tmpfld(i+1,:,:)+tmpfld(i-1,:,:))*0.5 - tmpfld(i,:,:) )
       enddo LOOP_i
   END DO SMOOTHING_PASSES
   RETURN
 END  SUBROUTINE SMTHER_3D





 !--------------------------
 ! == DOUBLE PRESISSION == |
 !--------------------------
 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 subroutine Smoother_2D_dp (field2D, method)
   real(kind=dp)      , dimension(:,:), pointer, intent(inout) :: field2D
   integer(kind=idflt),                          intent(in   ) :: method
 
   select case (method)
       case (1)
           call Smt121_2D_dp (field2D)
       case (2)
           call Smther_2D_dp (field2D)
   endselect
   return
 end  subroutine Smoother_2D_dp


 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 subroutine Smoother_3D_dp (field3D, method)
   real(kind=dp)      , dimension(:,:,:), pointer, intent(inout) :: field3D
   integer(kind=idflt),                            intent(in   ) :: method
 
   select case (method)
       case (1)
           call Smt121_3D_dp (field3D)
       case (2)
           call Smther_3D_dp (field3D)
   endselect
   return
 end  subroutine Smoother_3D_dp


 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !-------------------------------------------------------------------------------
 !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


 subroutine SMT121_2D_dp (cfld)
   real(kind=dp), dimension(:,:), pointer, intent(inout) :: cfld

   real(kind=dp), dimension(size(cfld,1),size(cfld,2)) :: tmpfld
   integer (kind=idflt) :: i, is, ie, j, js, je, loop
 
   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...Simple 1-2-1 smoother.
   SMOOTHING_PASSES: DO loop=1,smooth_passes
       tmpfld = cfld
       ! ...1-2-1 smoothing in the j direction first,
       LOOP_J: do j=js,je
           tmpfld(:,j) = 0.25_dp*(cfld(:,j+1) + 2._dp*cfld(:,j) + cfld(:,j-1))
       END DO LOOP_j
       ! ...then 1-2-1 smoothing in the i direction last
       LOOP_i: do i=is,ie
           cfld(i,:) = 0.25_dp*(tmpfld(i+1,:)+2._dp*tmpfld(i,:)+tmpfld(i-1,:))
       END DO LOOP_i
   END DO smoothing_passes
   RETURN
 END  subroutine SMT121_2D_dp
 

 !-------------------------------------------------------------------------------
 subroutine SMT121_3D_dp (cfld)
   real(kind=dp), dimension(:,:,:), pointer, intent(inout) :: cfld

   real(kind=dp), dimension(size(cfld,1),size(cfld,2),size(cfld,3)) :: tmpfld
   integer (kind=idflt) :: i, is, ie, j, js, je, loop

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...Simple 1-2-1 smoother.
   SMOOTHING_PASSES: DO loop=1,smooth_passes
       tmpfld = cfld
       ! ...1-2-1 smoothing in the j direction first,
       LOOP_J: DO j=js,je
           tmpfld(:,j,:) = 0.25_dp*(cfld(:,j+1,:) + 2._dp*cfld(:,j,:) + cfld(:,j-1,:))
       END DO LOOP_j
       ! ...then 1-2-1 smoothing in the i direction last
       LOOP_i: do i=is,ie
           cfld(i,:,:) = 0.25_dp*(tmpfld(i+1,:,:) + 2._dp*tmpfld(i,:,:) + tmpfld(i-1,:,:))
       ENDDO LOOP_i
   END DO smoothing_passes
   RETURN
 END  subroutine SMT121_3D_dp


 !-------------------------------------------------------------------------------
 subroutine SMTHER_2D_dp (cfld)
   real(kind=dp), dimension(:,:), pointer, intent(inout) :: cfld

   real(kind=dp), dimension(size(cfld,1),size(cfld,2)) :: tmpfld
   integer(kind=idflt) :: i, is, ie, j, js, je, loop, n

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...The odd number passes (n=1) of this are the "smoother", the even number
   !    passes are the "de-smoother" (note the different signs on xnu_dp).
   SMOOTHING_PASSES: DO loop=1,smooth_passes*2
       n = 2-MOD(loop,2)
       tmpfld = cfld
       ! ...1st: filter the j-direction
       LOOP_j:  DO j = js, je
           tmpfld(:,j) = cfld(:,j) + xnu_dp(n)*( (cfld(:,j+1)+cfld(:,j-1))*0.5_dp - cfld(:,j) )
       END DO LOOP_j
       ! ...2nd: filter the i-direction
       LOOP_i:  DO i = is, ie
           cfld(i,:) = tmpfld(i,:) + xnu_dp(n)*( (tmpfld(i+1,:)+tmpfld(i-1,:))*0.5_dp - tmpfld(i,:) )
       END DO LOOP_i
   END DO SMOOTHING_PASSES
   RETURN
 END  SUBROUTINE SMTHER_2D_dp
 

 !-------------------------------------------------------------------------------
 subroutine SMTHER_3D_dp (cfld)
   real(kind=dp), dimension(:,:,:), pointer, intent(inout) :: cfld

   real(kind=dp), dimension(size(cfld,1),size(cfld,2),size(cfld,3)) :: tmpfld
   integer(kind=idflt) :: i, is, ie, j, js, je, loop, n

   is = 2
   ie = size(cfld,dim=1) - 1
   js = 2
   je = size(cfld,dim=2) - 1
   ! ...The odd number passes (n=1) of this are the "smoother", the even number
   !    passes are the "de-smoother" (note the different signs on xnu_dp).
   SMOOTHING_PASSES: DO loop=1,smooth_passes*2
       n = 2-MOD(loop,2)
       tmpfld = cfld
       ! ...1st: filter the j-direction
       LOOP_j:  DO j = js, je
           tmpfld(:,j,:) = cfld(:,j,:) + xnu_dp(n)*( (cfld(:,j+1,:)+cfld(:,j-1,:))*0.5_dp - cfld(:,j,:) )
       END DO LOOP_j
       ! ...2nd: filter the i-direction
       LOOP_i:  do i = is, ie
           cfld(i,:,:) = tmpfld(i,:,:) + xnu_dp(n)*( (tmpfld(i+1,:,:)+tmpfld(i-1,:,:))*0.5_dp - tmpfld(i,:,:) )
       enddo LOOP_i
   END DO SMOOTHING_PASSES
   RETURN
 END  SUBROUTINE SMTHER_3D_dp

END MODULE module_smoothing_handler
