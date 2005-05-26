!/*****************************************************************************
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
! ****************************************************************************/
!
!/*****************************************************************************
! *
! * Subroutine for reducing a linsystem DOFs by eliminating DOFs correspondig to
! * known DOF values.
! *
! *****************************************************************************
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
! *                       Date:  13 Sep 2002
! *
! *                Modified by:
! *
! ****************************************************************************/
!------------------------------------------------------------------------------
 RECURSIVE INTEGER FUNCTION EliminateDirichlet( Model, Solver, A, b, x, n, DOFs, Norm )
!DEC$ATTRIBUTES DLLEXPORT :: EliminateDirichlet
!------------------------------------------------------------------------------
!******************************************************************************
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  TYPE(Matrix_t), POINTER :: A
!     INPUT: Linear equation matrix information
!
!  INTEGER :: DOFs
!     INPUT: Number of degrees of freedon of the equation
!
!  INTEGER :: n
!     INPUT: Length of unknown vector
!
!  REAL(KIND=dp) :: x(n)
!     INPUT: The unknown in the linear equation
!
!  REAL(KIND=dp) :: b(n)
!     INPUT: The right hand side of the linear equation
!
!  REAL(KIND=dp) :: Norm
!     INPUT: The norm to determine the convergence of the nonlinear system
!
!******************************************************************************


  USE Types
  USE Lists
  USE SolverUtils
  USE CRSmatrix
  USE GeneralUtils

  IMPLICIT NONE
  
  TYPE(model_t)  :: Model
  TYPE(solver_t) :: Solver
  TYPE(matrix_t), POINTER :: A
  INTEGER :: DOFs, n
  REAL(KIND=dp) :: b(n), x(n), Norm
!------------------------------------------------------------------------------

  TYPE(Matrix_t), POINTER :: Bmatrix, Cmatrix, SaveMatrix
  REAL(KIND=dp), POINTER :: F(:), U(:)
  REAL(KIND=dp) ::  CPUtime, TotTime, at,s,r

  LOGICAL :: stat, GotIt, EigAnal

  INTEGER :: istat, ii, i, j, k, l, m, t, Dirichlet, Level = 0, DOF
  INTEGER, POINTER :: Permutation(:), DCount(:), PermSave(:)
  LOGICAL :: AllocationsDone = .FALSE., Eigen = .FALSE., &
             Mass = .FALSE., Damp=.FALSE.

  COMPLEX(KIND=dp), ALLOCATABLE :: TempEigenVectors(:,:)

  SAVE Bmatrix, AllocationsDone

!------------------------------------------------------------------------------

  TotTime = CPUtime()

!------------------------------------------------------------------------------
   BMatrix => A % EMatrix

   Mass = .FALSE.
   IF ( ASSOCIATED( A % MassValues ) ) THEN
      IF ( SIZE( A % MassValues ) == SIZE( A % Values ) ) Mass = .TRUE.
   END IF

   Damp = .FALSE.
   IF ( ASSOCIATED( A % DampValues ) ) THEN
      IF ( SIZE( A % DampValues ) == SIZE( A % Values ) ) Damp = .TRUE.
   END IF

   IF ( ASSOCIATED( BMatrix ) ) THEN
      IF ( Mass .AND. .NOT. ASSOCIATED(BMatrix % MassValues) ) THEN
         ALLOCATE( Bmatrix % MassValues( SIZE(BMatrix % Values) ) )
      END IF
   END IF

   IF ( ASSOCIATED( BMatrix ) ) THEN
      IF ( Damp .AND. .NOT. ASSOCIATED(BMatrix % DampValues) ) THEN
         ALLOCATE( Bmatrix % DampValues( SIZE(BMatrix % Values) ) )
      END IF
   END IF

   IF ( .NOT. ASSOCIATED( BMatrix ) ) THEN
      ALLOCATE( Permutation( A % NumberOfRows ) )

      Permutation = 0
      Dirichlet = 0

      l = 0
      DO i=1,A % NumberOFRows
         s = A % Values( A % Diag(i) )
         A % Values( A % Diag(i) ) = 0.0d0 
         j = A % Rows(i)
         k = A % Rows(i+1)-1
         IF ( ALL( A % Values(j:k) == 0.0d0 ) ) THEN
            Dirichlet = Dirichlet + 1
         ELSE
            l = l + 1
            Permutation(i) = l
         END IF
         A % Values( A % Diag(i) ) = s
      END DO

!     IF ( Dirichlet <= 0 ) THEN
!         DEALLOCATE( Permutation )
!         EliminateDirichlet = 0
!         RETURN
!     END IF

      Bmatrix => AllocateMatrix()

      Bmatrix % NumberOFRows = A % NumberOFRows - Dirichlet
      ALLOCATE( Bmatrix % Rows( Bmatrix % NumberOfRows + 1 ), &
                Bmatrix % Diag( Bmatrix % NumberOfRows ) )

      j = 0
      k = 1
      DO i=1, A % NumberOFRows 
         IF ( Permutation(i) /= 0 ) THEN
            j = j + 1
            Bmatrix % Rows(j) = k
            DO l = A % Rows(i),A % Rows(i+1)-1
               t = A % Cols(l)
               IF ( Permutation(t) /= 0 ) k = k + 1
            END DO
         END IF
      END DO
      Bmatrix % Rows(Bmatrix % NumberOFRows+1) = k

      IF ( Mass ) THEN
         ALLOCATE( Bmatrix % MassValues(k-1) )
      END IF
      IF ( Damp ) THEN
         ALLOCATE( Bmatrix % DampValues(k-1) )
      END IF
      ALLOCATE( Bmatrix % Values(k-1), Bmatrix % Cols(k-1), &
          BMatrix % RHS( Bmatrix % NumberOfRows ) )

      ALLOCATE( Dcount( A % NumberOfRows ) )
      IF ( Permutation(1) == 0 ) THEN
         DCount(1) = 1
      ELSE
         DCount(1) = 0
      END IF

      DO i=2, A % NumberOFRows 
         IF ( Permutation(i) /= 0 ) THEN
            DCount(i) = DCount(i-1)
         ELSE
            DCount(i) = DCount(i-1) + 1
         END IF
      END DO

      j = 0
      k = 1
      DO i=1, A % NumberOFRows 
        IF ( Permutation(i) /= 0 ) THEN
           j = j + 1
           DO l = A % Rows(i),A % Rows(i+1)-1
              t = A % Cols(l)
              IF ( Permutation( t ) /= 0 ) THEN
                 Bmatrix % Cols(k) = t - DCount(t)
                 k = k + 1
              END IF
           END DO
        END IF
      END DO

     DEALLOCATE( DCount )

      DO i=1,Bmatrix % NumberOfRows
         DO j=Bmatrix % Rows(i), Bmatrix % Rows(i+1)-1
            IF ( Bmatrix % Cols(j) == i ) THEN
               Bmatrix % Diag(i) = j
            END IF
         END DO
      END DO
      CALL CRS_SortMatrix( BMatrix )

      A % EMatrix => BMatrix
      A % Perm => Permutation
   END IF

   Permutation => A % Perm
   F => BMatrix % RHS
   ALLOCATE( U( Bmatrix % NumberOfRows ) )

   j  = 0
   k  = 1
   IF ( Solver % Matrix % Complex ) THEN
      DO i=1, A % NumberOFRows,2
        IF ( Permutation(i) /= 0 ) THEN
           j = j + 1
           t = (i + 1) / 2
           F(2*j-1) = B(2*t-1)
           F(2*j-0) = B(2*t-0)
           U(2*j-1) = X(2*t-1)
           U(2*j-0) = X(2*t-0)

           k  = BMatrix % Rows(2*j-1)

           DO l = A % Rows(2*t-1), A % Rows(2*t)-1,2
              m = A % Cols(l)
              IF ( Permutation(m) == 0 ) THEN
                 m = (m + 1) / 2
                 s = A % Values(l) * b(2*m-1) - A % Values(l+1) * b(2*m)
                 F(2*j-1) = F(2*j-1) - s / A % Values( A % Diag(2*m-1) )
                 s = A % Values(l) * b(2*m) - A % Values(l+1) * b(2*m-1)
                 F(2*j) = F(2*j) - s / A % Values( A % Diag(2*m) )
              ELSE
                 Bmatrix % Values(k)   = A % Values(l)
                 Bmatrix % Values(k+1) = A % Values(l+1)
                 IF ( Mass ) THEN
                    Bmatrix % MassValues(k)   = A % MassValues(l)
                    Bmatrix % MassValues(k+1) = A % MassValues(l+1)
                 END IF
                 IF ( Damp ) THEN
                    Bmatrix % DampValues(k)   = A % DampValues(l)
                    Bmatrix % DampValues(k+1) = A % DampValues(l+1)
                 END IF
                 k = k + 2
              END IF
           END DO
        END IF
      END DO

      DO i=1,BMatrix % NumberOfRows,2
         k = BMatrix % Rows(i+1)
         DO j=BMatrix % Rows(i),BMatrix % Rows(i+1)-1,2
            BMatrix % Values(k)   = -BMatrix % Values(i+1)
            BMatrix % Values(k+1) =  BMatrix % Values(i)
            k = k + 2
         END DO
      END DO
   ELSE
      DO i=1, A % NumberOFRows 
        IF ( Permutation(i) /= 0 ) THEN
           j = j + 1
           F(j) = B(i) 
           U(j) = X(i)
           DO l = A % Rows(i), A % Rows(i+1)-1
              t = A % Cols(l)
              IF ( Permutation(t) == 0 ) THEN
                 F(j) = F(j) - A % Values(l) * b(t) / A % Values( A % Diag(t) )
              ELSE
                 Bmatrix % Values(k) = A % Values(l)
                 IF ( Mass ) THEN
                    Bmatrix % MassValues(k) = A % MassValues(l)
                 END IF
                 IF ( Damp ) THEN
                    Bmatrix % DampValues(k) = A % DampValues(l)
                 END IF
                 k = k + 1
              END IF
           END DO
        END IF
      END DO
   END IF
!------------------------------------------------------------------------------
!   Solve the system
!------------------------------------------------------------------------------
  Bmatrix % Lumped    = A % Lumped
  Bmatrix % Complex   = A % Complex
  Bmatrix % Symmetric = A % Symmetric

!print*,k, size(a % values), a % numberofrows, bmatrix % numberofrows

  IF ( ParEnv % PEs > 1 ) THEN
     PermSave => Solver % Variable % Perm
     CMatrix => Solver % Matrix
     Solver % Matrix => BMatrix

     ALLOCATE( Solver % Variable % Perm( SIZE(PermSave) ) )
     Solver % Variable % Perm = 0
     DO i=1,SIZE( PermSave )
       j = PermSave(i)
       IF ( j > 0 ) THEN
          DOF = DOFs * (j-1) + 1
          IF ( Permutation(DOF) > 0 ) THEN
              Solver % Variable % Perm(i) = ( Permutation(DOF)-1 ) / DOFs + 1
          END IF
       END IF
     END DO

     IF ( .NOT. ASSOCIATED( BMatrix % ParMatrix ) ) THEN
        CALL ParallelInitMatrix( Solver, BMatrix )
      END IF
  END IF


  at = CPUtime()
  CALL SolveLinearSystem( Bmatrix, F, U, Norm, DOFs, Solver )
  at = CPUtime() - at
  WRITE( Message, * ) 'Time used in SolveLinearSystem (CPU): ', at
  CALL Info( 'Dirichlet', Message, Level=5 )

  IF ( ParEnv % PEs > 1 ) THEN
     Solver % Matrix => CMatrix
     DEALLOCATE( Solver % Variable % Perm )
     Solver % Variable % Perm => PermSave
  END IF

!------------------------------------------------------------------------------
!   Map the result back to original nodes
!------------------------------------------------------------------------------

  IF ( Solver % NOFEigenValues > 0 ) THEN
     ALLOCATE( TempEigenVectors( Solver % NOFEigenValues, A % NumberOfRows ) )
     TempEigenVectors = Solver % Variable % EigenVectors

     j = 0
     IF ( Solver % Matrix % Complex ) THEN
        DO i=1,A % NumberOFRows / 2
           IF ( Permutation(i) == 0 ) THEN
              DO k=1,Solver % NOFEigenValues
                 Solver % Variable % EigenVectors(k,i) = DCMPLX( &
                     B(2*i) / A % Values( A % Diag(2*i) ), B(2*i+1) / A % Values( A % Diag(2*i+1) ) )
              END DO
           ELSE
              j = j + 1
              DO k=1,Solver % NOFEigenValues
                 Solver % Variable % EigenVectors(k,i) = TempEigenVectors(k,j)
              END DO
           END IF
        END DO
     ELSE
        DO i=1,A % NumberOFRows
           IF ( Permutation(i) == 0 ) THEN
              DO k=1,Solver % NOFEigenValues
                 Solver % Variable % EigenVectors(k,i) = B(i) / A % Values( A % Diag(i) )
              END DO
           ELSE
              j = j + 1
              DO k=1,Solver % NOFEigenValues
                 Solver % Variable % EigenVectors(k,i) = TempEigenVectors(k,j)
              END DO
           END IF
        END DO
     END IF
  ELSE
     j = 0
     DO i=1,A % NumberOFRows
        IF ( Permutation(i) == 0 ) THEN
           X(i) = B(i) / A % Values( A % Diag(i) )
        ELSE
           j = j + 1
           X(i) = U(j)
        END IF
     END DO
  END IF

  IF ( Solver % NOFEigenValues > 0 ) DEALLOCATE( TempEigenVectors )
!------------------------------------------------------------------------------

! CALL FreeMatrix( Bmatrix )
  DEALLOCATE( U )

  TotTime = CPUtime() - TotTime

  WRITE( Message, * ) 'Total time spent in DirichletReduction (CPU): ', TotTime
  CALL Info( 'Dirichlet', ' ', Level=5 )
  CALL Info( 'Dirichlet', Message, Level=5 )
  CALL Info( 'Dirichlet', ' ', Level=5 )

  EliminateDirichlet = 1

END FUNCTION EliminateDirichlet
