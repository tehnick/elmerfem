PROGRAM WriteTest

    USE BinIO

    IMPLICIT NONE
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
    INTEGER, PARAMETER :: fu = 10
    INTEGER :: a, stat, i
    REAL(dp) :: v(3)

    a = 5
    v = (/ -7.13_DP, 0.01_DP, HUGE(v) /)

    CALL BinOpen( fu, "writetest.out", "write", stat )
    IF ( stat /= 0 ) STOP

    CALL BinWriteInt4( fu, a, stat )
    IF ( stat /= 0) STOP

    CALL BinWriteString( fu, "Hello World!", stat )
    IF ( stat /= 0 ) STOP

    DO i = 1, SIZE( v )
        CALL BinWriteDouble( fu, v(i), stat )
        IF ( stat /= 0 ) STOP
    END DO

    CALL BinClose( fu, stat )
    IF ( stat /= 0 ) STOP

    OPEN( 10, file="writetest.out", position="append" )
    WRITE( 10, '(A)' ) ''
    WRITE( 10, '(A)' ) "Humhum"
    CLOSE( 10 )

    CALL BinOpen( fu, "writetest.out", "append", stat )
    IF ( stat /= 0 ) STOP

    DO i = 1, SIZE( v )
        CALL BinWriteDouble( fu, v(i), stat )
        IF ( stat /= 0 ) STOP
    END DO

    CALL BinClose( fu, stat )
    IF ( stat /= 0 ) STOP

END PROGRAM WriteTest
