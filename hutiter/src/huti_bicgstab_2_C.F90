# 1 "huti_bicgstab_2.src"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "huti_bicgstab_2.src"

!
! Subroutine to implement BiConjugate Gradient Stabilised (2) iteration
!
! $Id: huti_bicgstab_2.src,v 1.1.1.1 2005/04/15 10:31:18 vierinen Exp $

# 1 "huti_intdefs.h" 1
# 8 "huti_bicgstab_2.src" 2
# 1 "huti_fdefs.h" 1
# 9 "huti_bicgstab_2.src" 2

!*************************************************************************
!*************************************************************************
!
! This subroutine is based on a paper by Henk A. Van der Vorst:
! Parallel Iterative Solution Methods for Linear Systems arising from
! Discretized PDEs. This is the Bi-CGSTAB(2) version.
!
! All matrix-vector operations are done externally, so we do not need
! to know about the matrix structure (sparse or dense). Memory allocation
! for the working arrays has also been done externally.

!*************************************************************************
! Work array is used in the following order:
! work(:,1) = r tilde (zero)
! work(:,2) = u
! work(:,3) = t1v
! work(:,4) = v
! work(:,5) = s
! work(:,6) = w
! work(:,7) = t
! work(:,8) = r
!
!*************************************************************************
! Definitions to make the code more understandable and to make it look
! like the pseudo code
!
# 57 "huti_bicgstab_2.src"
!*************************************************************************
!*************************************************************************
! Complex version
!*************************************************************************
!*************************************************************************

subroutine huti_cbicgstab_2solv ( ndim, wrkdim, xvec, rhsvec, &
                            ipar, dpar, work, matvecsubr, pcondlsubr, &
                            pcondrsubr, dotprodfun, normfun, stopcfun )


  implicit none

  external matvecsubr, pcondlsubr, pcondrsubr
  external dotprodfun, normfun, stopcfun
  complex :: dotprodfun
  real :: normfun
  real :: stopcfun

  ! Parameters

  integer :: ndim, wrkdim
  complex, dimension(ndim) :: xvec, rhsvec
  integer, dimension(50) :: ipar
  double precision, dimension(10) :: dpar
  complex, dimension(ndim,wrkdim) :: work

  ! Local variables

  complex :: rho, oldrho, alpha, beta, omega1, omega2
  complex :: tau, delta, myy
  integer :: iter_count

  real :: residual, rhsnorm, precrhsnorm

  !
  ! End of variable declarations
  !*********************************************************************

  !*********************************************************************
  ! The actual BiCGSTAB begins here (look the pseudo code in the
  ! Templates...-book, page 27)
  !
  ! First the initialization part
  !

  iter_count = 1

  ! The following applies for all matrix operations in this solver

  ipar(6) = 0

  ! Norms of right-hand side vector are used in convergence tests

  if ( ipar(12) .eq. 1 .or. &
       ipar(12) .eq. 3 ) then
     rhsnorm = normfun( ipar(3), rhsvec, 1 )
  end if
  if ( ipar(12) .eq. 4 ) then
     call pcondlsubr( work(:,3), rhsvec, ipar )
     precrhsnorm = normfun( ipar(3), work(:,3), 1 )
  end if

  ! Generate vector xvec if needed

  if ( ipar(14) .eq. 0 ) then
     call huti_crandvec ( xvec, ipar )
  else if ( ipar(14) .ne. 1 ) then
     xvec = 1
  end if

  call pcondrsubr( work(:,2), xvec, ipar )
  call matvecsubr( work(:,2), work(:,8), ipar )
  work(:,2) = rhsvec - work(:,8)
  call pcondlsubr( work(:,8), work(:,2), ipar )
  work(:,1) = work(:,8)
  work(:,2) = 0
  oldrho = 1; omega2 = 1; alpha = 0

  !
  ! This is where the loop starts (that is we continue from here after
  ! the first iteration)
  !

300 continue

  oldrho = -omega2 * oldrho

  !
  ! This is the even BiCG step
  !

  rho = dotprodfun( ipar(3), work(:,1), 1, work(:,8), 1 )
  if ( rho .eq. 0 ) then
     ipar(30) = 45
     go to 1000
  end if

  beta = ( rho * alpha ) / oldrho
  oldrho = rho
  work(:,2) = work(:,8) - beta * work(:,2)

  call pcondrsubr( work(:,4), work(:,2), ipar )
  call matvecsubr( work(:,4), work(:,3), ipar )
  call pcondlsubr( work(:,4), work(:,3), ipar )

  alpha = oldrho / dotprodfun( ipar(3), work(:,1), 1, work(:,4), 1 )
  work(:,8) = work(:,8) - alpha * work(:,4)

  call pcondrsubr( work(:,5), work(:,8), ipar )
  call matvecsubr( work(:,5), work(:,3), ipar )
  call pcondlsubr( work(:,5), work(:,3), ipar )

  xvec = xvec + alpha * work(:,2)

  !
  ! This is the odd BiCG step
  !

  rho = dotprodfun( ipar(3), work(:,1), 1, work(:,5), 1 )
  if ( rho .eq. 0 ) then
     ipar(30) = 45
     go to 1000
  end if

  beta = ( rho * alpha ) / oldrho
  oldrho = rho
  work(:,4) = work(:,5) - beta * work(:,4)

  call pcondrsubr( work(:,6), work(:,4), ipar )
  call matvecsubr( work(:,6), work(:,3), ipar )
  call pcondlsubr( work(:,6), work(:,3), ipar )

  alpha = oldrho / dotprodfun( ipar(3), work(:,1), 1, work(:,6), 1 )
  work(:,2) = work(:,8) - beta * work(:,2)
  work(:,8) = work(:,8) - alpha * work(:,4)
  work(:,5) = work(:,5) - alpha * work(:,6)

  call pcondrsubr( work(:,7), work(:,5), ipar )
  call matvecsubr( work(:,7), work(:,3), ipar )
  call pcondlsubr( work(:,7), work(:,3), ipar )

  !
  ! This is the GCR(2) part
  !

  omega1 = dotprodfun( ipar(3), work(:,8), 1, work(:,5), 1 )
  myy = dotprodfun( ipar(3), work(:,5), 1, work(:,5), 1 )
  delta = dotprodfun( ipar(3), work(:,5), 1, work(:,7), 1 )
  tau = dotprodfun( ipar(3), work(:,7), 1, work(:,7), 1 )
  omega2 = dotprodfun( ipar(3), work(:,8), 1, work(:,7), 1 )

  tau = tau - ( delta * delta ) / myy
  omega2 = ( omega2 - ( delta * omega1 ) / myy ) / tau
  omega1 = ( omega1 - delta * omega2 ) / myy

  xvec = xvec + omega1 * work(:,8) + omega2 * work(:,5) + alpha * work(:,2)
  work(:,8) = work(:,8) - omega1 * work(:,5) - omega2 * work(:,7)

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call pcondrsubr( work(:,5), xvec, ipar )
     call matvecsubr( work(:,5), work(:,3), ipar )
     work(:,3) = work(:,3) - rhsvec
     call pcondlsubr( work(:,5), work(:,3), ipar )
     residual = normfun( ipar(3), work(:,5), 1 )
  case (1)
     call pcondrsubr( work(:,5), xvec, ipar )
     call matvecsubr( work(:,5), work(:,3), ipar )
     work(:,3) = work(:,3) - rhsvec
     call pcondlsubr( work(:,5), work(:,3), ipar )
     residual = normfun( ipar(3), work(:,5), 1 ) / rhsnorm
  case (2)
     residual = normfun( ipar(3), work(:,8), 1 )
  case (3)
     residual = normfun( ipar(3), work(:,8), 1 ) / rhsnorm
  case (4)
     residual = normfun( ipar(3), work(:,8), 1 ) / precrhsnorm
  case (5)
     work(:,3) = omega1 * work(:,8) + omega2 * work(:,5) + alpha * work(:,2)
     residual = normfun( ipar(3), work(:,3), 1 )
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,8), ipar, dpar )
  case default
     call pcondrsubr( work(:,5), xvec, ipar )
     call matvecsubr( work(:,5), work(:,3), ipar )
     work(:,3) = work(:,3) - rhsvec
     call pcondlsubr( work(:,5), work(:,3), ipar )
     residual = normfun( ipar(3), work(:,5), 1 )
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

  work(:,2) = work(:,2) - omega1 * work(:,4) - omega2 * work(:,6)

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

end subroutine huti_cbicgstab_2solv

!*************************************************************************
