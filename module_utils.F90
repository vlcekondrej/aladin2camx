MODULE module_utils
 USE module_global_variables
 ! different useful subroutines
 IMPLICIT NONE

 CONTAINS

  INTEGER FUNCTION getFreeUnitNo()
  ! returns smallest number that is not associated with an opened file
    IMPLICIT NONE
    INTEGER, PARAMETER :: maxUnitNo=100
    INTEGER :: i
    LOGICAL :: LAnswer
    DO i=1, maxUnitNo
        INQUIRE(UNIT=i, OPENED=LAnswer)
        IF (.NOT.LAnswer) THEN
            getFreeUnitNo=i
            RETURN
        END IF
    END DO
  END FUNCTION getFreeUnitNo
 
  SUBROUTINE TestStop(istat,message,ounit)
  ! if istat/=0, writes message to standard output and stops program  
    IMPLICIT NONE
    INTEGER,          INTENT(IN   )           :: istat
    CHARACTER(LEN=*), INTENT(IN   )           :: message
    INTEGER,          INTENT(IN   ), OPTIONAL :: ounit

    IF (istat /= 0) THEN
        IF (PRESENT(ounit)) THEN
            WRITE(ounit, *) message
        ELSE
            WRITE(*, *) message
        END IF
        CLOSE(logFileUnit)
        STOP
    END IF
  END SUBROUTINE TestStop

  SUBROUTINE if_not_add_char(text,znak)
  ! subroutine if "znak" is not on the last position of string "text", subroutine "if_not_ad_char" adds it. 
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: text
    CHARACTER(LEN=1), INTENT(IN   ) :: znak
    INTEGER :: pozice, delka

    delka  = LEN_TRIM(text)
    pozice = SCAN(text,znak,BACK=.TRUE.)
    IF (delka /= pozice) text=TRIM(text)//znak
  END SUBROUTINE if_not_add_char

END MODULE module_utils


