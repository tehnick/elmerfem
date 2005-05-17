PROGRAM CHECK
  IMPLICIT NONE
  CHARACTER*100 :: A
  INTEGER :: I
  REAL*8 :: R, Tol
  COMPLEX :: C

  OPEN(1,FILE='out3.txt')
  READ(1,*) A, I, C
  CLOSE(1)

  OPEN(2,FILE='TARGET.RESULT')
  READ(2,*) R, Tol
  CLOSE(2)

  OPEN(3,FILE='TEST.RESULT', STATUS='UNKNOWN')
  IF( ABS( REAL(C)-R ) < Tol ) THEN
     WRITE(3,'(2F8.1)' ) -1.0, 0.0     !    Passed
  ELSE
     WRITE(3,'(2F8.1)' )  1.0, 0.0     !    Failed
  END IF
  CLOSE(3)

END PROGRAM CHECK
