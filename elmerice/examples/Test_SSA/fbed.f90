FUNCTION fbed(x,y) 
USE types
IMPLICIT NONE
REAL(KIND=dp) :: x, y, fbed
REAL(KIND=dp) :: fsurf

fbed = fsurf(x,y) -1000.0d0

END FUNCTION fbed
