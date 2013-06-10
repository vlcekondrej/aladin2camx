MODULE module_standard_types

 IMPLICIT NONE

 !-------------------------------------------------------------------------------
 ! ...processor ASCII kind
 !              =====
 integer, parameter :: asc = kind('ASCII')

 !-------------------------------------------------------------------------------
 ! ...processor INTEGER kinds
 !              =======
 !==> 8-bit signed integer ( 1.3+2 = huge() )
 INTEGER, PARAMETER :: byte_k = selected_int_kind(2)
 !==> 16-bit signed integer ( 3.3+4 = huge() )
 INTEGER, PARAMETER :: short_k = selected_int_kind(4)
 !==> 32-bit signed integer ( 2.1+9 = huge() )
 INTEGER, PARAMETER :: int_k = selected_int_kind(9)
 !==> 64-bit signed integer ( 4.2+18 = huge() )
 INTEGER, PARAMETER :: long_k = selected_int_kind(18)
 ! ...abbreviations
 INTEGER, PARAMETER :: i1b=byte_k
 INTEGER, PARAMETER :: i2b=short_k
 INTEGER, PARAMETER :: i4b=int_k
 INTEGER, PARAMETER :: i8b=long_k

 !-------------------------------------------------------------------------------
 ! ...processor REAL kinds
 !              ====
 !==> 32-bit (IEEE 754) real supports 10+-37.<6 digits>
 INTEGER, PARAMETER :: single_k = selected_real_kind(6,37)
 !==> 64-bit (IEEE 754) real supports 10+-307.<15 digits>
 INTEGER, PARAMETER :: double_k = selected_real_kind(15,307)
 !==> 128-bit real supports 10+-4931.<32 digits>
 INTEGER, PARAMETER :: quad_k = selected_real_kind(33,4931)
 ! ...abbreviations
 INTEGER, PARAMETER :: sp=single_k
 INTEGER, PARAMETER :: dp=double_k
 INTEGER, PARAMETER :: qp=quad_k

! !-------------------------------------------------------------------------------
! ! ...processor COMPLEX kinds
! !              =======
! INTEGER, PARAMETER :: scmplx_k = kind((1.0_sp,1.0_sp))
! INTEGER, PARAMETER :: dcmplx_k = kind((1.0_dp,1.0_dp))
! INTEGER, PARAMETER :: qcmplx_k = kind((1.0_qp,1.0_qp))
! ! ...abbreviations
! INTEGER, PARAMETER :: spc=scmplx_k
! INTEGER, PARAMETER :: dpc=dcmplx_k
! INTEGER, PARAMETER :: qpc=qcmplx_k

 !? k cemu to ?  !-------------------------------------------------------------------------------
 !? k cemu to ?  ! ...processor logical kinds
 !? k cemu to ?  !              =======
 !? k cemu to ?  !    logical kind using same storage as byte
 !? k cemu to ?  !-?  integer, parameter :: l_byte_k  = 1
 !? k cemu to ?  INTEGER, PARAMETER :: l_byte_k  = byte_k
 !? k cemu to ?  !    logical kind using same storage as short
 !? k cemu to ?  !-?  integer, parameter :: l_short_k = 2
 !? k cemu to ?  INTEGER, PARAMETER :: l_short_k = short_k
 !? k cemu to ?  !    logical kind using same storage as int
 !? k cemu to ?  !-?  integer, parameter :: l_int_k   = 4
 !? k cemu to ?  INTEGER, PARAMETER :: l_int_k   =  int_k
 !? k cemu to ?  !    logical kind using same storage as long
 !? k cemu to ?  !-?  integer, parameter :: l_long_k =  8
 !? k cemu to ?  INTEGER, PARAMETER :: l_long_k  = long_k
 !? k cemu to ?  ! ...abbreviations
 !? k cemu to ?  INTEGER, PARAMETER :: l1b=l_byte_k
 !? k cemu to ?  INTEGER, PARAMETER :: l2b=l_short_k
 !? k cemu to ?  INTEGER, PARAMETER :: l4b=l_int_k
 !? k cemu to ?  INTEGER, PARAMETER :: l8b=l_long_k

 !-------------------------------------------------------------------------------
 ! ...processor DEFAULT kinds
 !              =======
 INTEGER, PARAMETER :: idflt = kind(0)
 INTEGER, PARAMETER :: rdflt = kind(1.0)
 INTEGER, PARAMETER :: ddflt = kind(1.0d0)
 INTEGER, PARAMETER :: ldflt = kind(.true.)

END MODULE module_standard_types
