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
! * Parallel solver utilities for *Solver - routines
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
! *                       Date: 02 Apr 2001
! *
! *                Modified by:
! *
! *       Date of modification:
! *
! *****************************************************************************/

MODULE ParallelUtils

     USE SparIterSolve

     IMPLICIT NONE

CONTAINS

!-------------------------------------------------------------------------------
    SUBROUTINE ParallelInitMatrix( Solver, Matrix )
DLLEXPORT ParallelInitMatrix
!-------------------------------------------------------------------------------
       TYPE(Solver_t) :: Solver
       TYPE(Matrix_t), POINTER :: Matrix
!-------------------------------------------------------------------------------
       INTEGER :: i, j, DOFs
!-------------------------------------------------------------------------------

       IF ( ParEnv % PEs <= 1 .OR. .NOT. ASSOCIATED(Solver % Matrix) ) RETURN
       DOFs = Solver % Variable % DOFs

       ALLOCATE( Matrix % Perm( Solver % Mesh % NumberOfNodes * DOFs) )
       ALLOCATE( Matrix % INVPerm( Solver % Mesh % NumberOfNodes * DOFs) )

       Matrix % Perm    = 0
       Matrix % INVPerm = 0

       DO i=1,Solver % Mesh % NumberOfNodes
          IF ( Solver % Variable % Perm(i) /= 0 )  THEN
             DO j=1,DOFs
                Matrix % Perm( (i-1)*DOFs+j ) = &
                   DOFs * (Solver % Variable % Perm(i)-1) + j
             END DO
          END IF
       END DO

       DO i=1,Solver % Mesh % NumberOfNodes * DOFs
          IF ( Matrix % Perm(i) /= 0 ) THEN
             Matrix % INVPerm(Matrix % Perm(i)) = i
          END IF
       END DO

       Matrix % ParMatrix => &
          ParInitMatrix( Matrix, Solver % Mesh % Nodes, DOFs )
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelInitMatrix
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    SUBROUTINE ParallelInitSolve( Matrix, x, b, r, DOFs, Mesh  )
DLLEXPORT ParallelInitSolve
!-------------------------------------------------------------------------------
       TYPE(Mesh_t), POINTER :: Mesh
       INTEGER :: DOFs
       REAL(KIND=dp) :: x(:), b(:), r(:)
       TYPE(Matrix_t), POINTER :: Matrix
!-------------------------------------------------------------------------------
       CALL SParInitSolve( Matrix, x, b, r, DOFs, Mesh % Nodes )
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelInitSolve
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    SUBROUTINE ParallelUpdateSolve( Matrix, x, r )
DLLEXPORT ParallelUpdateSolve
!-------------------------------------------------------------------------------
       REAL(KIND=dp) :: x(:), r(:)
       TYPE(Matrix_t), POINTER :: Matrix
!-------------------------------------------------------------------------------
       CALL SParUpdateSolve( Matrix, x, r )
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelUpdateSolve
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    SUBROUTINE ParallelMatrixVector( Matrix, x, b, Update, UseMassVals )
DLLEXPORT ParallelMatrixVector
!-------------------------------------------------------------------------------
      REAL(KIND=dp) :: x(:), b(:)
      LOGICAL, OPTIONAL :: Update, UseMassVals
      TYPE(Matrix_t), POINTER :: Matrix
!-------------------------------------------------------------------------------
      INTEGER :: i,ipar(1)
      REAL(KIND=dp), POINTER :: Mx(:), Mr(:), Mb(:)

      TYPE(SplittedMatrixT), POINTER :: SP
      TYPE(Matrix_t), POINTER :: saveptrif(:), saveptrnb(:), saveptrin
!-------------------------------------------------------------------------------
      GlobalData => Matrix % ParMatrix
      IF ( PRESENT( Update ) ) THEN
         IF ( Update ) THEN
            IF ( PRESENT(UseMassVals) ) THEN
               IF ( UseMassVals ) THEN

                  SP => GlobalData % SplittedMatrix
                  ALLOCATE( saveptrif( ParEnv % PEs ) )
                  ALLOCATE( saveptrnb( ParEnv % PEs ) )
                  ALLOCATE( saveptrIn )
                  DO i=1,ParEnv % PEs
                  IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                     saveptrif(i) % values => SP % IfMatrix(i) % values
                  end if
                  IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                     saveptrnb(i) % values => SP % NbsIfMatrix(i) % values
                  endif
                  END DO
                  saveptrin % values => SP % InsideMatrix % values

                  DO i=1,ParEnv % PEs
                    IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                         SP % IfMatrix(i) % Values => SP % IfMatrix(i) % MassValues
                    END IF

                    IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                         SP % NbsIfMatrix(i) % Values => SP % NbsIfMatrix(i) % MassValues 
                    END IF
                  END DO
                  SP % InsideMatrix % Values => SP % InsideMatrix % MassValues
               END IF

               Mx => GlobalData % SplittedMatrix % TmpXVec 
               Mr => GlobalData % SplittedMatrix % TmpRVec 
               CALL SParMatrixVector( Mx, Mr, ipar )
               CALL SParUpdateResult( Matrix, x, b, .FALSE. )
            ELSE
               Mx => GlobalData % SplittedMatrix % TmpXVec 
               Mr => GlobalData % SplittedMatrix % TmpRVec 
               CALL SParMatrixVector( Mx, Mr, ipar )
               CALL SParUpdateResult( Matrix, x, b, .FALSE. )
            END IF

            IF ( PRESENT(UseMassVals) ) THEN
               IF ( UseMassVals ) THEN
                  DO i=1,ParEnv % PEs
                  IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                   SP % IfMatrix(i) % values =>  saveptrif(i) % values
                  end if
                  IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                   SP % NbsIfMatrix(i) % values =>  saveptrnb(i) % values 
                  end if
                  END DO
                  SP % InsideMatrix % values => saveptrin % values 
                  DEALLOCATE( saveptrif )
                  DEALLOCATE( saveptrnb )
                  DEALLOCATE( saveptrIn )
               END IF
            END IF
         ELSE
            IF ( PRESENT(UseMassVals) ) THEN
               IF ( UseMassVals ) THEN

                  SP => GlobalData % SplittedMatrix
                  ALLOCATE( saveptrif( ParEnv % PEs ) )
                  ALLOCATE( saveptrnb( ParEnv % PEs ) )
                  ALLOCATE( saveptrIn )
                  DO i=1,ParEnv % PEs
                  IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                     saveptrif(i) % values => SP % IfMatrix(i) % values
                  end if
                  IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                     saveptrnb(i) % values => SP % NbsIfMatrix(i) % values
                  endif
                  END DO
                  saveptrin % values => SP % InsideMatrix % values

                  DO i=1,ParEnv % PEs
                    IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                         SP % IfMatrix(i) % Values => SP % IfMatrix(i) % MassValues
                    END IF

                    IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                         SP % NbsIfMatrix(i) % Values => SP % NbsIfMatrix(i) % MassValues 
                    END IF
                  END DO
                  SP % InsideMatrix % Values => SP % InsideMatrix % MassValues
               END IF
            END IF

            CALL SParMatrixVector( x, b, ipar )

            IF ( PRESENT(UseMassVals) ) THEN
               IF ( UseMassVals ) THEN
                  DO i=1,ParEnv % PEs
                  IF ( SP % IfMatrix(i) % NumberOfRows /= 0 ) THEN
                   SP % IfMatrix(i) % values =>  saveptrif(i) % values
                  end if
                  IF ( SP % NbsIfMatrix(i) % NumberOfRows /= 0 ) THEN
                   SP % NbsIfMatrix(i) % values =>  saveptrnb(i) % values 
                  end if
                  END DO
                  SP % InsideMatrix % values => saveptrin % values 
                  DEALLOCATE( saveptrif )
                  DEALLOCATE( saveptrnb )
                  DEALLOCATE( saveptrIn )
               END IF
            END IF
         END IF
      ELSE
         CALL SParMatrixVector( x, b, ipar )
      END IF
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelMatrixVector
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    SUBROUTINE ParallelUpdateResult( Matrix, x, r )
DLLEXPORT ParallelUpdateResult
!-------------------------------------------------------------------------------
       REAL(KIND=dp) :: x(:), r(:)
       TYPE(Matrix_t), POINTER :: Matrix
!-------------------------------------------------------------------------------
       CALL SParUpdateResult( Matrix, x, r, .TRUE. )
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelUpdateResult
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------

    FUNCTION ParallelMatrix( A,x,b,r ) RESULT(M)
DLLEXPORT ParallelMatrix
!-------------------------------------------------------------------------------
       TYPE(Matrix_t), POINTER :: A, M
       REAL(KIND=dp),  POINTER, OPTIONAL :: x(:),b(:),r(:)
!-------------------------------------------------------------------------------

       M => A % ParMatrix % SplittedMatrix % InsideMatrix
       IF ( PRESENT(x) ) THEN
          b => M % RHS
          x => A % ParMatrix % SplittedMatrix % TmpXVec
          r => A % ParMatrix % SplittedMatrix % TmpRVec
       END IF
!-------------------------------------------------------------------------------
    END FUNCTION ParallelMatrix
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    FUNCTION ParallelNorm( n, x ) RESULT(s)
DLLEXPORT ParallelNorm
!-------------------------------------------------------------------------------
      INTEGER :: n
      REAL(KIND=dp) :: x(:),s
!-------------------------------------------------------------------------------
      s = SParNorm( n, x, 1 )
!-------------------------------------------------------------------------------
    END FUNCTION ParallelNorm
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    FUNCTION ParallelDOT( n, x, y ) RESULT(s)
DLLEXPORT ParallelDOT
!-------------------------------------------------------------------------------
      INTEGER :: n
      REAL(KIND=dp) :: x(:),y(:),s
!-------------------------------------------------------------------------------
      s = SParDotProd( n, x, 1, y, 1 )
!-------------------------------------------------------------------------------
    END FUNCTION ParallelDOT
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
    SUBROUTINE ParallelGlobalNumbering(Nodes,NewNodes,IntCnts,IntArray,Reorder)
DLLEXPORT ParallelGlobalNumbering
!-------------------------------------------------------------------------------
       TYPE(Nodes_t) :: Nodes
       INTEGER :: NewNodes,IntCnts(:),IntArray(:),Reorder(:)
!-------------------------------------------------------------------------------
        CALL SparGlobalNumbering( Nodes,NewNodes,IntCnts,IntArray,Reorder )
!-------------------------------------------------------------------------------
    END SUBROUTINE ParallelGlobalNumbering
!-------------------------------------------------------------------------------

END MODULE ParallelUtils
