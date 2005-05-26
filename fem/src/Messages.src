!/******************************************************************************
! *
! *       ELMER, A Computational Fluid Dynamics Program.
! *
! *       Copyright 1st April 1995 - , Center for Scientific Computing,
! *                                    Finland.
! *
! *       All rights reserved. No part of this program may be used,
! *       reproduced or transmitted in any form or by any means
! *       without the written permission of CSC.
! *
! *****************************************************************************/
!
!/******************************************************************************
! *
! * Message output routines.
! *
! ******************************************************************************
! *
! *                     Author:       Juha Ruokolainen
! *
! *                    Address: Center for Scientific Computing
! *                                Tietotie 6, P.O. BOX 405
! *                                  02101 Espoo, Finland
! *                                  Tel. +358 0 457 2723
! *                                Telefax: +358 0 457 2302
! *                              EMail: Juha.Ruokolainen@csc.fi
! *
! *                       Date: 2002
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

MODULE Messages

   IMPLICIT NONE

   CHARACTER(LEN=512) :: Message
DLLEXPORT Message
   INTEGER, PRIVATE :: i
   LOGICAL :: OutputPrefix=.FALSE., OutputCaller=.TRUE.
   LOGICAL :: OutputLevelMask(0:31) = (/ (.TRUE.,i=1,32) /)
   INTEGER :: MaxOutputLevel=32, MinOutputLevel=0, OutputPE = 0

CONTAINS

!-----------------------------------------------------------------------
   SUBROUTINE Info( Caller, String, noAdvance, Level )
DLLEXPORT Info
!-----------------------------------------------------------------------
     CHARACTER(LEN=*) :: Caller, String
     INTEGER, OPTIONAL :: Level
     LOGICAL, OPTIONAL :: noAdvance
!-----------------------------------------------------------------------

     LOGICAL :: nadv, nadv1 = .FALSE.
     INTEGER :: n
     SAVE nadv1

!-----------------------------------------------------------------------

     IF ( OutputPE /= 0 ) RETURN

     IF ( PRESENT( Level ) ) THEN
       IF ( .NOT. OutputLevelMask(Level) ) RETURN
     END IF

     nadv = .FALSE.
     IF ( PRESENT( noAdvance ) ) nadv = noAdvance

     IF ( OutputPrefix .AND. .NOT. nadv1 ) THEN
        WRITE( *,'(A)', ADVANCE = 'NO' ) 'INFO:: '
     END IF

     IF ( OutputCaller .AND. .NOT. nadv1 ) THEN
        WRITE( *,'(A)', ADVANCE = 'NO' ) TRIM(Caller) // ': '
     END IF

     IF ( nadv ) THEN
        WRITE( *,'(A)', ADVANCE = 'NO' )  TRIM(String)
     ELSE
        WRITE( *,'(A)', ADVANCE = 'YES' ) TRIM(String)
     END IF
     nadv1 = nadv

!    CALL FLUSH(6)
!-----------------------------------------------------------------------
   END SUBROUTINE Info
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
   SUBROUTINE Warn( Caller, String, noAdvance )
DLLEXPORT Warn
!-----------------------------------------------------------------------
     CHARACTER(LEN=*) :: Caller, String
     LOGICAL, OPTIONAL :: noAdvance
!-----------------------------------------------------------------------

     LOGICAL :: nadv, nadv1 = .FALSE.
     SAVE nadv1

!-----------------------------------------------------------------------
     IF ( .NOT. OutputLevelMask(2) ) RETURN

     nadv = .FALSE.
     IF ( PRESENT( noAdvance ) ) nadv = noAdvance

     IF ( nadv ) THEN
        WRITE( *, '(A,A,A,A)', ADVANCE='NO' ) &
          'WARNING:: ', TRIM(Caller), ': ', TRIM(String)
     ELSE
        IF ( .NOT. nadv1 ) THEN
           WRITE( *, '(A,A,A,A)', ADVANCE='YES' ) &
             'WARNING:: ', TRIM(Caller), ': ', TRIM(String)
        ELSE
           WRITE( *, '(A)', ADVANCE='YES' ) TRIM(String)
        END IF
     END IF
     nadv1 = nadv
!    CALL FLUSH(6)
!-----------------------------------------------------------------------
   END SUBROUTINE Warn
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
   SUBROUTINE Error( Caller, String, noAdvance )
DLLEXPORT Error
!-----------------------------------------------------------------------
     CHARACTER(LEN=*) :: Caller, String
     LOGICAL, OPTIONAL :: noAdvance
!-----------------------------------------------------------------------

     LOGICAL :: nadv, nadv1 = .FALSE.
     SAVE nadv1

!-----------------------------------------------------------------------
     IF ( .NOT. OutputLevelMask(1) ) RETURN

     nadv = .FALSE.
     IF ( PRESENT( noAdvance ) ) nadv = noAdvance

     IF ( nadv ) THEN
        WRITE( *, '(A,A,A,A)', ADVANCE='NO' ) &
          'ERROR:: ', TRIM(Caller), ': ', TRIM(String )
     ELSE
        IF ( .NOT. nadv1 ) THEN
           WRITE( *, '(A,A,A,A)', ADVANCE='YES' ) &
             'ERROR:: ', TRIM(Caller), ': ', TRIM(String)
        ELSE
           WRITE( *, '(A)', ADVANCE='YES' ) TRIM(String)
        END IF
     END IF
     nadv1 = nadv
!    CALL FLUSH(6)
!-----------------------------------------------------------------------
   END SUBROUTINE Error
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
   SUBROUTINE Fatal( Caller, String, noAdvance )
DLLEXPORT Fatal
!-----------------------------------------------------------------------
     CHARACTER(LEN=*) :: Caller, String
     LOGICAL, OPTIONAL :: noAdvance
!-----------------------------------------------------------------------

     LOGICAL :: nadv, nadv1 = .FALSE.
     SAVE nadv1

!-----------------------------------------------------------------------
     IF ( .NOT. OutputLevelMask(0) ) STOP

     nadv = .FALSE.
     IF ( PRESENT( noAdvance ) ) nadv = noAdvance

     IF ( nadv ) THEN
        WRITE( *, '(A,A,A,A)', ADVANCE='NO' ) &
          'ERROR:: ', TRIM(Caller), ': ', TRIM(String )
     ELSE
        IF ( .NOT. nadv1 ) THEN
           WRITE( *, '(A,A,A,A)', ADVANCE='YES' ) &
             'ERROR:: ', TRIM(Caller), ': ', TRIM(String)
        ELSE
           WRITE( *, '(A)', ADVANCE='YES' ) TRIM(String)
        END IF
        STOP
     END IF
     nadv1 = nadv
!    CALL FLUSH(6)
!-----------------------------------------------------------------------
   END SUBROUTINE Fatal
!-----------------------------------------------------------------------

END MODULE Messages
