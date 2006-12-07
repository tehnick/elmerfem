!  Copyright (c) December 5 2006 - , CSC - Scientific Computing Ltd., Finland
! 
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
! 
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program (in file fem/GPL-2); if not, write to the 
!  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
!  Boston, MA 02110-1301, USA.
!
!
! Wrappers for a bunch of C functions defined in binio.c to read/write
! from/to a binary stream in a portable (independent of endianess) manner.
!
! The OPTIONAL 'Status' argument, if PRESENT, will be set to > 0 for error (use
! StrErrorF to create meaningful error messages), 0 for sucess and (for the
! BinRead* procedures) -1 for end-of-file.  If .NOT.PRESENT(Status), then, on
! error, an error message will be written and the program terminated.
!
MODULE BinIO

    IMPLICIT NONE
    PRIVATE

    PUBLIC :: BinOpen
    PUBLIC :: BinClose
    PUBLIC :: BinWriteInt4
    PUBLIC :: BinReadInt4
    PUBLIC :: BinWriteDouble
    PUBLIC :: BinReadDouble
    PUBLIC :: BinWriteString
    PUBLIC :: BinReadString
    PUBLIC :: StrErrorF

CONTAINS

    SUBROUTINE HandleStatus( Status, Status_, MsgPrefix )
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER, INTENT(IN) :: Status_
        CHARACTER(100), INTENT(IN) :: MsgPrefix
        INTEGER, PARAMETER :: STDERR = 0
        CHARACTER(100) :: Msg

        IF ( PRESENT( Status ) ) THEN
            Status = Status_
        ELSE
            IF ( Status_ > 0 ) THEN
                CALL StrErrorF( Status_, Msg )
                WRITE( STDERR, * ) TRIM(MsgPrefix) // ": " // TRIM(Msg)
                STOP
            END IF
        END IF
    END SUBROUTINE HandleStatus


    SUBROUTINE BinOpen( Unit, File, Action, Status )
        INTEGER, INTENT(IN) :: Unit
        CHARACTER(*), INTENT(IN) :: File
        CHARACTER(*), INTENT(IN) :: Action ! "write", "append" or "read"
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinOpen_( Unit, TRIM(File), LEN_TRIM(File), Action, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Can't open file " &
                                            // TRIM(File) )
    END SUBROUTINE BinOpen


    SUBROUTINE BinClose( Unit, Status )
        INTEGER, INTENT(IN) :: Unit
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_
        
        CALL BinClose_( Unit, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Can't close file" )
    END SUBROUTINE BinClose


    SUBROUTINE BinWriteInt4( Unit, a, Status )
        INTEGER, INTENT(IN) :: Unit
        INTEGER, INTENT(IN) :: a
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinWriteInt4_( Unit, a, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error writing Int4" )
    END SUBROUTINE BinWriteInt4


    SUBROUTINE BinReadInt4( Unit, a, Status )
        INTEGER, INTENT(IN) :: Unit
        INTEGER, INTENT(OUT) :: a
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinReadInt4_( Unit, a, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error reading Int4" )
    END SUBROUTINE BinReadInt4


    SUBROUTINE BinWriteDouble( Unit, a, Status )
        INTEGER, INTENT(IN) :: Unit
        DOUBLE PRECISION, INTENT(IN) :: a
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinWriteDouble_( Unit, a, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error writing Double" )
    END SUBROUTINE BinWriteDouble


    SUBROUTINE BinReadDouble( Unit, a, Status )
        INTEGER, INTENT(IN) :: Unit
        DOUBLE PRECISION, INTENT(OUT) :: a
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinReadDouble_( Unit, a, Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error reading Double" )
    END SUBROUTINE BinReadDouble


    ! Write 's' to file pointed to by 'unit', and append a '\0'.
    SUBROUTINE BinWriteString( UNIT, s, Status )
        INTEGER, INTENT(IN) :: Unit
        CHARACTER(*), INTENT(IN) :: s
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinWriteString_( Unit, s, LEN(s), Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error writing string" )
    END SUBROUTINE BinWriteString

    ! Read bytes up to next NULL byte (but no more than len(s)) into 's'
    ! from file pointed to by 'unit', and pad with spaces. The NULL byte
    ! will be removed from the stream, but not added to 's'.
    SUBROUTINE BinReadString( Unit, s, Status )
        INTEGER, INTENT(IN) :: Unit
        CHARACTER(*), INTENT(OUT) :: s
        INTEGER, OPTIONAL, INTENT(OUT) :: Status
        INTEGER :: Status_

        CALL BinReadString_( Unit, s, LEN(s), Status_ )
        CALL HandleStatus( Status, Status_, "BINIO: Error reading string" )
    END SUBROUTINE BinReadString

    ! Return a string representation of the error code 'e' (as returned in a
    ! status variable) in 's'. (Actually just a wrapper to the C function
    ! strerror().)
    SUBROUTINE StrErrorF( e, s )
        INTEGER, INTENT(IN) :: e
        CHARACTER(*), INTENT(OUT) :: s

        CALL StrErrorF_( e, s, LEN(s) )
    END SUBROUTINE StrErrorF

END MODULE BinIO
