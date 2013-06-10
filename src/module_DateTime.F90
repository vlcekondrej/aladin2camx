MODULE module_DateTime
IMPLICIT NONE

TYPE tDateTime
  INTEGER :: y,m,d,h,mi
  REAL :: s
END TYPE tDateTime

INTERFACE ASSIGNMENT (=)
  MODULE PROCEDURE  assign_DateTimeIN_to_DateTimeOUT
END INTERFACE ASSIGNMENT (=)
PRIVATE assign_DateTimeIN_to_DateTimeOUT


INTERFACE OPERATOR (.LT.)
  MODULE PROCEDURE  dt1_LT_dt2
END INTERFACE OPERATOR (.LT.)
PRIVATE dt1_LT_dt2

INTERFACE OPERATOR (.LE.)
  MODULE PROCEDURE  dt1_LE_dt2
END INTERFACE OPERATOR (.LE.)
PRIVATE dt1_LE_dt2

! = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
CONTAINS
! = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

 SUBROUTINE null_DateTime(dt)
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(INOUT) :: dt
   dt%y = 0
   dt%m = 0
   dt%d = 0
   dt%h = 0
   dt%mi= 0
   dt%s = 0.
 END SUBROUTINE null_DateTime

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 PURE FUNCTION Compare_DateTime(dt1,dt2) RESULT(vysledek)
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(IN) :: dt1, dt2
   INTEGER :: vysledek
   ! Compare_DateTime < 0 for dt1<dt2
   !                  =          =
   !                  >          >
   vysledek = 0
   IF (dt1%y.LT.dt2%y) THEN
       vysledek = -6
   ELSEIF (dt1%y.GT.dt2%y) THEN
       vysledek =  6
   ELSEIF (dt1%m.LT.dt2%m) THEN
       vysledek = -5
   ELSEIF (dt1%m.GT.dt2%m) THEN
       vysledek =  5
   ELSEIF (dt1%d.LT.dt2%d) THEN
       vysledek = -4
   ELSEIF (dt1%d.GT.dt2%d) THEN
       vysledek =  4
   ELSEIF (dt1%h.LT.dt2%h) THEN
       vysledek = -3
   ELSEIF (dt1%h.GT.dt2%h) THEN
       vysledek =  3
   ELSEIF (dt1%mi.LT.dt2%mi) THEN
       vysledek = -2
   ELSEIF (dt1%mi.GT.dt2%mi) THEN
       vysledek =  2
   ELSEIF (dt1%s.LT.dt2%s) THEN
       vysledek = -1
   ELSEIF (dt1%s.GT.dt2%s) THEN
       vysledek =  1
   END IF
 END FUNCTION Compare_DateTime

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 LOGICAL PURE FUNCTION DT1_LT_DT2(dt1,dt2)
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(IN) :: dt1, dt2
   dt1_lt_dt2 = .FALSE.
   IF ( compare_DateTime(dt1,dt2)<0 ) dt1_lt_dt2 = .TRUE.
 END FUNCTION DT1_LT_DT2

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 LOGICAL PURE FUNCTION DT1_LE_DT2(dt1,dt2)
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(IN) :: dt1, dt2
   dt1_le_dt2 = .FALSE.
   IF ( compare_DateTime(dt1,dt2)<=0 ) dt1_le_dt2 = .TRUE.
 END FUNCTION DT1_LE_DT2

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 LOGICAL PURE FUNCTION LEAP_YEAR(YEAR)
   IMPLICIT NONE
   INTEGER, INTENT(IN  ):: YEAR
   ! Date         Author
   ! ----------   -----------
   ! 2008-08      O. Vlcek      original code
   ! 2011-01-06   O. Vlcek      oprava chyby - roky delitelne 400 se nebraly jako trestupne

   ! LEAP_YEAR returns .TRUE., if YEAR is a leap year accordingly to Gregorian calendar:
   !   1) Rok je prestupny, pokud je delitelny cislem 4 (1996, 2004)
   !   2) Vyjimka c.1 : Rok neni prestupny, pokud je delitelny cislem 100 (1900, 2100)
   !   3) Vyjimka c.2 z Vyjimky c.1: Rok je prestupny, pokud je delitelny cislem 400 (2000, 2400)

   IF (MOD(year,400) == 0) THEN
     LEAP_YEAR = .TRUE.
   ELSE IF (MOD(year,100) == 0) THEN
     LEAP_YEAR = .FALSE.
   ELSE IF(MOD(YEAR,4) == 0) THEN
     LEAP_YEAR = .TRUE.
   ELSE
       LEAP_YEAR = .FALSE.
   END IF
 END FUNCTION  LEAP_YEAR

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 SUBROUTINE DateTime_plus_min(dt,mi)
   ! pricte, nebo odecte minuty od danneho data
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(INOUT) :: dt
   INTEGER, INTENT(IN   ) :: mi

   INTEGER, PARAMETER             :: YearMonths=12
   INTEGER, DIMENSION(YearMonths) :: MonthDays_reg, MonthDays_leap, MonthDays
   INTEGER                        :: restD


   DATA MonthDays_reg /31,28,31,30,31,30,31,31,30,31,30,31/
   DATA MonthDays_leap/31,29,31,30,31,30,31,31,30,31,30,31/

   IF (mi>=0) THEN
     dt%mi = dt%mi + mi
     dt%h = dt%h + dt%mi/60
     dt%mi = MOD(dt%mi,60)
     restD = dt%h/24
     dt%h = MOD(dt%h,24)
   ELSE
     dt%mi = dt%mi + mi
     dt%h = dt%h + dt%mi/60
     dt%mi = MOD(dt%mi,60)
     IF ( dt%mi < 0 ) THEN
       dt%mi = 60. + dt%mi
       dt%h = dt%h - 1
     END IF
     restD = dt%h/24
     dt%h = MOD(dt%h,24)
     IF ( dt%h < 0 ) THEN
       restD = restD - 1
       dt%h = dt%h + 24
     END IF
   END IF    

   DO WHILE (restD > 0)
       MonthDays = MonthDays_reg
       IF (LEAP_YEAR(dt%y)) MonthDays = MonthDays_leap

       IF (restD > (MonthDays(dt%m)-dt%d)) THEN ! musim se posunout na prvni den nasledujiciho mesice
           restD = restD - (MonthDays(dt%m)-dt%d) - 1
           dt%d = 1
           IF (dt%m < YearMonths) THEN
               dt%m = dt%m + 1
           ELSE
               dt%m = 1
               dt%y = dt%y + 1
               MonthDays = MonthDays_reg
               IF (LEAP_YEAR(dt%y)) MonthDays = MonthDays_leap
           END IF
       ELSE
           dt%d = dt%d + restD
           restD = 0
       END IF
   END DO

   DO WHILE (restD < 0)
       MonthDays = MonthDays_reg
       IF (LEAP_YEAR(dt%y)) MonthDays = MonthDays_leap

       IF (abs(restD) >= (dt%d)) THEN ! musim se posunout na posledni den predchoziho mesice
           restD = restD + dt%d 
           IF (dt%m > 1) THEN
               dt%m = dt%m - 1
           ELSE
               dt%m = YearMonths
               dt%y = dt%y - 1
               MonthDays = MonthDays_reg
               IF (LEAP_YEAR(dt%y)) MonthDays = MonthDays_leap
           END IF
           dt%d = MonthDays(dt%m)
       ELSE
           dt%d = dt%d + restD
           restD = 0
       END IF
   END DO
 END SUBROUTINE DateTime_plus_min

 ! * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 PURE SUBROUTINE assign_DateTimeIN_to_DateTimeOUT(dtOUT,dtIN)
   IMPLICIT NONE
   TYPE(tDateTime), INTENT(IN   ) :: dtIN
   TYPE(tDateTime), INTENT(  OUT) :: dtOUT
   dtOUT%y  = dtIN%y
   dtOUT%m  = dtIN%m
   dtOUT%d  = dtIN%d
   dtOUT%h  = dtIN%h
   dtOUT%mi = dtIN%mi
   dtOUT%s  = dtIN%s
 END SUBROUTINE assign_DateTimeIN_to_DateTimeOUT

END MODULE module_DateTime
