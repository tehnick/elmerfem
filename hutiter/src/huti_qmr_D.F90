# 1 "huti_qmr.src"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "huti_qmr.src"

!
! Subroutine to implement QMR iterative method (double complex)
!
! $Id: huti_qmr.src,v 1.1.1.1 2005/04/15 10:31:18 vierinen Exp $

# 1 "huti_intdefs.h" 1
# 8 "huti_qmr.src" 2
# 1 "huti_fdefs.h" 1
# 9 "huti_qmr.src" 2

!*************************************************************************
!
! This subroutine is based on a paper by Freund and Nachtigal:
! An Implementation of the QMR Method Based on Coupled Two-Term
! Recurrences, 1994 (SIAM J. Sci. Comput, March 1994)
! and a book by Barret et al.:
! Templates for the Solution of Linear Systems: Building Blocks for
! Iterative Methods, 1993.
!
! All matrix-vector operations are done externally, so we do not need
! to know about the matrix structure (sparse or dense). Memory allocation
! for the working arrays has also been done externally.
!
!*************************************************************************
! Work array is used in the following order:
! work(:,1) = v
! work(:,2) = v tilde
! work(:,3) = y
! work(:,4) = y tilde
! work(:,5) = w
! work(:,6) = w tilde
! work(:,7) = z
! work(:,8) = z tilde
! work(:,9) = p
! work(:,10) = p tilde
! work(:,11) = q
! work(:,12) = d
! work(:,13) = s
! work(:,14) = r
!
!*************************************************************************
! Definitions to make the code more understandable and to make it look
! like the pseudo code
!
# 75 "huti_qmr.src"
!*************************************************************************

!*************************************************************************
!*************************************************************************
! Double precision version
!*************************************************************************
!*************************************************************************

subroutine huti_dqmrsolv ( ndim, wrkdim, xvec, rhsvec, &
                          ipar, dpar, work, matvecsubr, pcondlsubr, &
                          pcondrsubr, dotprodfun, normfun, stopcfun )

  implicit none

  ! Parameters

  external matvecsubr, pcondlsubr, pcondrsubr
  external dotprodfun, normfun, stopcfun
  double precision :: dotprodfun
  double precision :: normfun
  double precision :: stopcfun

  integer :: ndim, wrkdim
  double precision, dimension(ndim) :: xvec, rhsvec
  integer, dimension(50) :: ipar
  double precision, dimension(10) :: dpar
  double precision, dimension(ndim,wrkdim) :: work

  ! Local variables

  double precision :: beta, gamma, oldgamma, delta, rho, rhonext
  double precision :: psi, theta, oldtheta, eta, epsilon
  integer iter_count
  double precision :: residual, rhsnorm, precrhsnorm

  !
  ! End of variable declarations
  !*********************************************************************

  !*********************************************************************
  ! The actual QMR begins here (look the pseudo code in the
  ! Templates..-book on page 24)
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
     call pcondlsubr( work(:,9), rhsvec, ipar )
     precrhsnorm = normfun( ipar(3), work(:,9), 1 )
  end if

  ! Generate vector xvec if needed

  if ( ipar(14) .eq. 0 ) then
     call huti_drandvec ( xvec, ipar )
  else if ( ipar(14) .ne. 1 ) then
     xvec = 1
  end if

  ipar(6) = 0
  call matvecsubr( xvec, work(:,14), ipar )
  work(:,14) = rhsvec - work(:,14)
  work(:,2) = work(:,14)
  ipar(6) = 0
  call pcondlsubr( work(:,3), work(:,2), ipar )

  work(:,6) = work(:,14)
  ipar(6) = 0
  call pcondrsubr( work(:,7), work(:,6), ipar )

  rho = normfun( ipar(3), work(:,3), 1 )
  psi = normfun( ipar(3), work(:,7), 1 )
  oldgamma = 1
  eta = -1

  !
  ! This is where the loop starts (that is we continue from here after
  ! the first iteration)
  !

300 continue

  if (( rho .eq. 0 ) .or. ( psi .eq. 0 )) then
     ipar(30) = 10
     go to 1000
  end if

  work(:,1) = work(:,2) / rho
  work(:,3) = work(:,3) / rho
  work(:,5) = work(:,6) / psi
  work(:,7) = work(:,7) / psi
  delta = dotprodfun( ipar(3), work(:,7), 1, work(:,3), 1 )

  if ( delta .eq. 0 ) then
     ipar(30) = 11
     go to 1000
  end if

  ipar(6) = 0
  call pcondrsubr( work(:,4), work(:,3), ipar )

  ipar(6) = 1
  call pcondlsubr( work(:,8), work(:,7), ipar )

  if ( iter_count .eq. 1 ) then
     work(:,9) = work(:,4)
     work(:,11) = work(:,8)
  else
     work(:,9) = work(:,4) - ((psi * delta)/epsilon) * work(:,9)
     work(:,11) = work(:,8) - ((rho * delta)/epsilon) * work(:,11)
  end if

  ipar(6) = 0
  call matvecsubr( work(:,9), work(:,10), ipar )
  epsilon = dotprodfun( ipar(3), work(:,11), 1, work(:,10), 1 )
  if ( epsilon .eq. 0 ) then
     ipar(30) = 12
     go to 1000
  end if

  beta = epsilon / delta
  if ( beta .eq. 0 ) then
     ipar(30) = 13
     go to 1000
  end if

  work(:,2) = work(:,10) - beta * work(:,1)

  ipar(6) = 0
  call pcondlsubr( work(:,3), work(:,2), ipar )

  rhonext = normfun( ndim, work(:,3), 1 )

  ipar(6) = 1
  call matvecsubr( work(:,11), work(:,6), ipar )

  work(:,6) = work(:,6) - beta * work(:,5)

  ipar(6) = 1
  call pcondrsubr( work(:,7), work(:,6), ipar )

  psi = normfun( ipar(3), work(:,7), 1 )
  theta = rhonext / (oldgamma * abs( beta ))
  gamma = 1 / sqrt( 1 + theta * theta )
  if ( gamma .eq. 0 ) then
     ipar(30) = 14
     go to 1000
  end if

  eta = -1 * ( eta * rho * gamma * gamma ) / ( beta * oldgamma * oldgamma )

  if ( iter_count .eq. 1 ) then
     work(:,12) = eta * work(:,9)
     work(:,13) = eta * work(:,10)
  else
     work(:,12) = eta * work(:,9) + ( oldtheta * gamma ) * ( oldtheta * gamma ) * work(:,12)
     work(:,13) = eta * work(:,10) + ( oldtheta * gamma ) * ( oldtheta * gamma ) * work(:,13)
  end if
  xvec = xvec + work(:,12)
  work(:,14) = work(:,14) - work(:,13)

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call matvecsubr( xvec, work(:,4), ipar )
     work(:,4) = work(:,4) - rhsvec
     residual = normfun( ipar(3), work(:,4), 1 )
  case (1)
     call matvecsubr( xvec, work(:,4), ipar )
     work(:,4) = work(:,4) - rhsvec
     residual = normfun( ipar(3), work(:,4), 1 ) / rhsnorm
  case (2)
     residual = normfun( ipar(3), work(:,14), 1 )
  case (3)
     residual = normfun( ipar(3), work(:,14), 1 ) / rhsnorm
  case (4)
     residual = normfun( ipar(3), work(:,14), 1 ) / precrhsnorm
  case (5)
     residual = normfun( ipar(3), work(:,12), 1 )
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,14), ipar, dpar )
  case default
     call matvecsubr( xvec, work(:,4), ipar )
     work(:,4) = work(:,4) - rhsvec
     residual = normfun( ipar(3), work(:,4), 1 )
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

  rho = rhonext
  oldgamma = gamma
  oldtheta = theta

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

end subroutine huti_dqmrsolv

!*********************************************************************
