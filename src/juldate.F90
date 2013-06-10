FUNCTION juldate(YYYYMMDD,HHMISS)
 USE module_standard_types

 IMPLICIT NONE
 REAL(KIND=dp)       :: JULDATE
 INTEGER, INTENT(IN) :: YYYYMMDD,HHMISS

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 !                                                                             *
 !     Calculates the Julian date                                              *
 !       Julian date (JD) is the interval of time in days and                  *
 !       fractions of a day, since January 1, 4713 BC Greenwich noon           *
 !                                                                             *
 !     AUTHOR: Andreas Stohl (15 October 1993)                                 *
 !                                                                             *
 !     Variables:                                                              *
 !     DD             Day                                                      *
 !     HH             Hour                                                     *
 !     HHMISS         Hour, minute + second                                    *
 !     JA,JM,JY       help variables                                           *
 !     JULDATE        Julian Date                                              *
 !     JULDAY         help variable                                            *
 !     MI             Minute                                                   *
 !     MM             Month                                                    *
 !     SS             Second                                                   *
 !     YYYY           Year                                                     *
 !     YYYYMMDDHH     Date and Time                                            *
 !                                                                             *
 !     Constants:                                                              *
 !     IGREG          help constant                                            *
 !                                                                             *
 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 
 INTEGER :: YYYY,MM,DD,HH,MI,SS
 INTEGER :: JULDAY,JY,JM,JA,IGREG
 PARAMETER (IGREG=15+31*(10+12*1582))
 
 YYYY=YYYYMMDD/10000
 MM=(YYYYMMDD-10000*YYYY)/100
 DD=YYYYMMDD-10000*YYYY-100*MM
 HH=HHMISS/10000
 MI=(HHMISS-10000*HH)/100
 SS=HHMISS-10000*HH-100*MI
 
 IF (YYYY.EQ.0) STOP 'There is no Year Zero.'
 IF (YYYY.LT.0) YYYY=YYYY+1
 IF (MM.GT.2) THEN
   JY=YYYY
   JM=MM+1
 ELSE
   JY=YYYY-1
   JM=MM+13
 ENDIF
 JULDAY=INT(365.25*JY)+INT(30.6001*JM)+DD+1720995
 IF (DD+31*(MM+12*YYYY).GE.IGREG) THEN
   JA=INT(0.01*JY)
   JULDAY=JULDAY+2-JA+INT(0.25*JA)
 ENDIF
 
 JULDATE=DBLE(REAL(JULDAY)) + DBLE(REAL(HH)/24.) + DBLE(REAL(MI)/1440.) + DBLE(REAL(SS)/86400.)
 
END FUNCTION juldate






FUNCTION julday(dummydate)
 USE module_standard_types

 IMPLICIT NONE
 INTEGER            :: julday
 INTEGER,INTENT(IN) :: dummydate ! YYYYMMDD

 !-----------------------------------------------------------
 ! EN: calculates order of day from the begining of the year |
 ! CZ: spocte poradi dne v roce                              |
 !-----------------------------------------------------------

 INTEGER :: ref_year 
 REAL(KIND=dp) :: date_act,date_ref
 REAL(KIND=dp) :: juldate
 CHARACTER*8 :: chdate


 write(chdate,'(I8)') dummydate 
 chdate(1:8)=chdate(1:4)//'0101'

 read(chdate(1:8),'(I8)') ref_year 

 date_ref=juldate(ref_year,000000)
 date_act=juldate(dummydate,000000)

 julday=date_act-date_ref+1

 RETURN
END FUNCTION julday
