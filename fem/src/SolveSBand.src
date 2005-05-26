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
! * Call LAPACK symmetric band matrix solvers.
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
! *                       Date: 08 Jun 1997
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

       SUBROUTINE SolveSBandLapack( N,M,A,X,Subband,Band )
DLLEXPORT SolveSBandLapack

       IMPLICIT NONE

       INTEGER :: N,M,Subband,Band
       DOUBLE PRECISION :: A(Band,N),X(N,M)

       INTEGER :: IPIV(N),INFO

       IF ( N .LE. 0 ) RETURN

       INFO = 0
       CALL DPBTRF( 'L',N,Subband,A,Band,INFO )
        IF ( info /= 0 ) THEN
         PRINT*,'ERROR: SolveSymmetricBand: singular matrix. LAPACK DPBTRF info: ',info
          STOP
        END IF

       INFO = 0
       CALL DPBTRS( 'L',N,Subband,M,A,Band,X,N,INFO )
        IF ( info /= 0 ) THEN
         PRINT*,'ERROR: SolveSymmetricBand: singular matrix. LAPACK DPBTRS info: ',info
          STOP
        END IF

       END


       SUBROUTINE SolveComplexSBandLapack( N,M,A,X,Subband,Band )
DLLEXPORT SolveSBandLapack

       IMPLICIT NONE

       INTEGER :: N,M,Subband,Band
       DOUBLE COMPLEX :: A(Band,N),X(N,M)

       INTEGER :: IPIV(N),INFO

       IF ( N .LE. 0 ) RETURN

       INFO = 0
       CALL ZPBTRF( 'L',N,Subband,A,Band,INFO )
        IF ( info /= 0 ) THEN
         PRINT*,'ERROR: SolveSymmetricBand: singular matrix. LAPACK ZPBTRF info: ',info
          STOP
        END IF

       INFO = 0
       CALL ZPBTRS( 'L',N,Subband,M,A,Band,X,N,INFO )
        IF ( info /= 0 ) THEN
         PRINT*,'ERROR: SolveSymmetricBand: singular matrix. LAPACK ZPBTRS info: ',info
          STOP
        END IF

       END
