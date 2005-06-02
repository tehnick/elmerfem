
!
! Subroutines to implement Conjugate Gradient iterative method
!
! $Id: huti_cg.src,v 1.2 2005/06/02 14:53:40 vierinen Exp $























































































































































!*************************************************************************
!*************************************************************************
!
! These subroutines are based on a book by Barret et al.:
! Templates for the Solution of Linear Systems: Building Blocks for
!  Iterative Methods, 1993.
!
! All matrix-vector operations are done externally, so we do not need
! to know about the matrix structure (sparse or dense). So has the
! memory allocation for the working arrays done also externally.

!*************************************************************************
! Work array is used in the following order:
! work(:,1) = z
! work(:,2) = p
! work(:,3) = q
! work(:,4) = r
!
! Definitions to make the code more understandable and to make it look
! like the pseudo code (these are commond to all precisions)
!




!*************************************************************************
  
!*************************************************************************
!*************************************************************************
! Double complex version
!*************************************************************************
!*************************************************************************

subroutine  huti_zcgsolv  ( ndim, wrkdim, xvec, rhsvec, &
                          ipar, dpar, work, matvecsubr, pcondlsubr, &
                          pcondrsubr, dotprodfun, normfun, stopcfun )


  implicit none

  external matvecsubr, pcondlsubr, pcondrsubr
  external dotprodfun, normfun, stopcfun
  double complex :: dotprodfun
  double precision :: normfun
  double precision :: stopcfun

  ! Parameters

  integer :: ndim, wrkdim
  double complex, dimension(ndim) :: xvec, rhsvec
  integer, dimension(50) :: ipar
  double precision, dimension(10) :: dpar
  double complex, dimension(ndim,wrkdim) :: work

  ! Local variables

  double complex :: alpha, beta, rho, oldrho
  integer iter_count
  double precision :: residual, rhsnorm, precrhsnorm

  !
  ! End of variable declarations
  !*********************************************************************

  !*********************************************************************
  ! The actual CG begins here (look the pseudo code in the
  ! Templates...-book, page 15)
  !
  ! First the initialization part
  !

  iter_count = 1

  ! Norms of right-hand side vector are used in convergence tests

  if ( ipar(12) .eq. 1 .or. & 
       ipar(12) .eq. 3 ) then
     rhsnorm = normfun( ipar(3), rhsvec, 1 )
  end if
  if ( ipar(12) .eq. 4 ) then
     call pcondlsubr( work(:,2), rhsvec, ipar )
     precrhsnorm = normfun( ipar(3), work(:,2), 1 )
  end if

  ! The following applies for all matrix operations in this solver

  ipar(6) = 0

  ! Generate vector xvec if needed

  if ( ipar(14) .eq. 0 ) then
     call  huti_zrandvec   ( xvec, ipar )
  else if ( ipar(14) .ne. 1 ) then
     xvec = 1
  end if

  call matvecsubr( xvec, work(:,4), ipar )

  work(:,4) = rhsvec - work(:,4)

  !
  ! This is where the loop starts (that is we continue from here after
  ! the first iteration)
  !

300 continue

  call pcondlsubr( work(:,3), work(:,4), ipar )
  call pcondrsubr( work(:,1), work(:,3), ipar )

  rho = dotprodfun( ipar(3), work(:,4), 1, work(:,1), 1 )
  if ( rho .eq. 0 ) then
     ipar(30) = 20
     go to 1000
  end if

  if ( iter_count .eq. 1 ) then
     work(:,2) = work(:,1)
  else
     beta = rho / oldrho
     work(:,2) = work(:,1) + beta * work(:,2)
  end if

  call matvecsubr( work(:,2), work(:,3), ipar )

  alpha = rho / dotprodfun( ipar(3), work(:,2), 1, work(:,3), 1 )

  xvec = xvec + alpha * work(:,2)
  work(:,4) = work(:,4) - alpha * work(:,3)

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call matvecsubr( xvec, work(:,1), ipar )
     work(:,1) = work(:,1) - rhsvec
     residual = normfun( ipar(3), work(:,1), 1 )
  case (1)
     call matvecsubr( xvec, work(:,1), ipar )
     work(:,1) = work(:,1) - rhsvec
     residual = normfun( ipar(3), work(:,1), 1 ) / rhsnorm
  case (2)
     residual = normfun( ipar(3), work(:,4), 1 )
  case (3)
     residual = normfun( ipar(3), work(:,4), 1 ) / rhsnorm
  case (4)
     residual = normfun( ipar(3), work(:,4), 1 ) / precrhsnorm
  case (5)
     work(:,1) = alpha * work(:,2)
     residual = normfun( ipar(3), work(:,1), 1 )
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,4), ipar, dpar )
  case default
     call matvecsubr( xvec, work(:,1), ipar )
     work(:,1) = work(:,1) - rhsvec
     residual = normfun( ipar(3), work(:,1), 1 )
  end select

  !
  ! Print debugging output if desired
  !

  if ( ipar(5) .ne. 0 ) then
     if ( mod(iter_count, ipar(5)) .eq. 0 ) then
        write (*, '(I8, E11.4)') iter_count, residual
     end if
  end if

  if ( residual .lt. dpar(1) ) then
     ipar(30) = 1
     go to 1000
  end if

  oldrho = rho

  !
  ! Return back to the iteration loop (without initialization)
  !

  iter_count = iter_count + 1
  if ( iter_count .gt. ipar(10) ) then
     ipar(30) = 2
     go to 1000
  end if

  go to 300

  !
  ! This is where we exit last time (after enough iterations or breakdown)
  !

1000 continue
  if ( ipar(5) .ne. 0 ) then
     write (*, '(I8, E11.4)') iter_count, residual
  end if

  ipar(31) = iter_count
  return

  ! End of execution
  !*********************************************************************

end subroutine  huti_zcgsolv 

!*************************************************************************
