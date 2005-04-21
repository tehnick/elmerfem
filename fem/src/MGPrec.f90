#include <huti_fdefs.h>

!------------------------------------------------------------------------------
  SUBROUTINE MultigridPrec( u,v,ipar )
!   DLLEXPORT MultigridPrec
!------------------------------------------------------------------------------
!******************************************************************************
! 
!  DESCRIPTION:
!
!  ARGUMENTS:
!
!    REAL(KIND=dp) :: u,v
!
!    INTEGER :: ipar(:)
!      INPUT: structure holding info from (HUTIter-iterative solver package)
!
!******************************************************************************
    USE Multigrid

    INTEGER, DIMENSION(*) :: ipar
    REAL(KIND=dp), DIMENSION(*) :: u,v

    INTEGER :: n, DOFs
    TYPE(Solver_t), POINTER :: PSolver

    PSolver => CurrentModel % Solver

    n = HUTI_NDIM
    IF ( PSolver % Matrix % Complex ) n=2*n

    u(1:n) = 0.0d0
    DOFs =  n / COUNT(PSolver % Variable % Perm>0)
    CALL MultiGridSolve( GlobalMatrix, u(1:n), v(1:n), &
          DOFs,  PSolver, PSolver % MultiGridLevel, FirstCall)

    FirstCall = .FALSE.

  END SUBROUTINE MultigridPrec
!------------------------------------------------------------------------------


