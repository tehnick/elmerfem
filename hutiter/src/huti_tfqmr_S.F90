# 1 "huti_tfqmr.src"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "huti_tfqmr.src"
!
! Subroutines to implement Transpose Free QMR iteration
!
! $Id: huti_tfqmr.src,v 1.1.1.1 2005/04/15 10:31:18 vierinen Exp $

# 1 "huti_intdefs.h" 1
# 7 "huti_tfqmr.src" 2
# 1 "huti_fdefs.h" 1
# 8 "huti_tfqmr.src" 2

!*************************************************************************
!*************************************************************************
!
! These subroutines are based on a paper by Roland W. Freund:
! A Transpose-Free Quasi-Minimal Residual Algorithm for Non-Hermitian
! Linear Systems, 1993 (SIAM J. Sci. Comput, March 1993)
!
! All matrix-vector operations are done externally, so we do not need
! to know about the matrix structure (sparse or dense). Memory allocation
! for the working arrays has also been done externally.

!*************************************************************************
! Work array is used in the following order:
! work(:,1) = v
! work(:,2) = y
! work(:,3) = y new
! work(:,4) = r tilde (zero)
! work(:,5) = t1v (temporary for matrix-vector operations)
! work(:,6) = t2v (temporary for matrix-vector operations)
! work(:,7) = w
! work(:,8) = d
! work(:,9) = r
! work(:,10) = trv (temporary vector for residual computations)
!
!*************************************************************************
! Definitions to make the code more understandable and to make it look
! like the pseudo code
!
# 61 "huti_tfqmr.src"
! This is the magic ratio for upperb and tolerance used in upper bound
! convergence test


!*************************************************************************
!*************************************************************************
! Single precision version
!*************************************************************************
!*************************************************************************

subroutine huti_stfqmrsolv ( ndim, wrkdim, xvec, rhsvec, ipar,&
                            dpar, work, matvecsubr, pcondlsubr, pcondrsubr, &
                            dotprodfun, normfun, stopcfun )


  implicit none

  external matvecsubr, pcondlsubr, pcondrsubr
  external dotprodfun, normfun, stopcfun
  real :: dotprodfun
  real :: normfun
  real :: stopcfun

  ! Parameters

  integer :: ndim, wrkdim
  real, dimension(ndim) :: xvec, rhsvec
  integer, dimension(50) :: ipar
  double precision, dimension(10) :: dpar
  real, dimension(ndim,wrkdim) :: work

  ! Local variables

  real :: rho, oldrho, eta, tau, gamma, oldgamma, alpha
  real :: beta, c
  integer :: iter_count

  real :: residual, upperb, rhsnorm, precrhsnorm

  !
  ! End of variable declarations
  !*********************************************************************

  !*********************************************************************
  ! The actual TFQMR begins here (look the pseudo code in the
  ! A Transpose-Free...-paper, algorithm 5.1)
  !
  ! First the initialization part
  !

  iter_count = 1

  ! The following applies for all matrix operations in this solver

  ipar(6) = 0

  ! Norms of right-hand side vector are used in convergence tests

  if ( ipar(12) .eq. 1 .or. &
       ipar(12) .eq. 3 .or. &
       ipar(12) .eq. 6 ) then
     rhsnorm = normfun( ipar(3), rhsvec, 1 )
  end if
  if ( ipar(12) .eq. 4 ) then
     call pcondlsubr( work(:,8), rhsvec, ipar )
     precrhsnorm = normfun( ipar(3), work(:,8), 1 )
  end if

  !
  ! Part 1A - 1C
  !

  ! Generate vector xvec if needed

  if ( ipar(14) .eq. 0 ) then
     call huti_srandvec ( xvec, ipar )
  else if ( ipar(14) .ne. 1 ) then
     xvec = 1
  end if

  call pcondrsubr( work(:,8), xvec, ipar )
  call matvecsubr( work(:,8), work(:,9), ipar )
  work(:,8) = rhsvec - work(:,9)
  call pcondlsubr( work(:,9), work(:,8), ipar )

  work(:,2) = work(:,9); work(:,7) = work(:,9)
  call pcondrsubr( work(:,1), work(:,2), ipar )
  call matvecsubr( work(:,1), work(:,8), ipar )
  call pcondlsubr( work(:,1), work(:,8), ipar )
  work(:,6) = work(:,1)

  work(:,8) = 0
  tau = normfun( ipar(3), work(:,9), 1 )
  oldgamma = 0; gamma = 0; eta = 0

  work(:,4) = work(:,9)
  oldrho = dotprodfun ( ipar(3), work(:,4), 1, work(:,9), 1 )
  if ( oldrho .eq. 0 ) then
     ipar(30) = 30
     go to 1000
  end if

  !
  ! This is where the loop starts (that is we continue from here after
  ! the first iteration)
  !
  !
  ! Part 2A
  !

300 continue

  alpha = oldrho / dotprodfun( ipar(3), work(:,4), 1, work(:,1), 1 )
  work(:,3) = work(:,2) - alpha * work(:,1)

  !
  ! Part 2B
  !
  !
  ! This is the inner loop from 2n-1 to 2n
  !

  ! First the 2n-1 case

  ! Note: We have already MATRIX * work(:,2) in work(:,6)

  work(:,7) = work(:,7) - alpha * work(:,6)
  gamma = ( normfun( ipar(3), work(:,7), 1 )) / tau
  c = 1 / sqrt( 1 + gamma * gamma )
  tau = tau * gamma * c

  work(:,8) = work(:,2) + ((oldgamma * oldgamma * eta) / alpha) * work(:,8)
  eta = c * c * alpha
  xvec = xvec + eta * work(:,8)

  oldgamma = gamma

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 )
  case (1)
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 ) / rhsnorm
  case (2)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), ipar )
     residual = normfun( ipar(3), work(:,10), 1 )
  case (3)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), ipar )
     residual = normfun( ipar(3), work(:,10), 1 ) / rhsnorm
  case (4)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), ipar )
     residual = normfun( ipar(3), work(:,10), 1 ) / precrhsnorm
  case (5)
     work(:,9) = eta * work(:,8)
     residual = normfun( ipar(3), work(:,9), 1 )
  case (6)
     upperb = real( sqrt( 2.0 * iter_count ) * tau / rhsnorm)
     if ( ( upperb / dpar(1) ) .lt. 10.0 ) then
        call pcondrsubr( work(:,10), xvec, ipar )
        call matvecsubr( work(:,10), work(:,9), ipar )
        work(:,10) = work(:,9) - rhsvec
        call pcondlsubr( work(:,9), work(:,10), ipar )
        residual = normfun( ipar(3), work(:,9), 1 ) / rhsnorm
     else
        residual = upperb
     end if
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,9), ipar, dpar )
  case default
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 )
  end select

  if ( residual .lt. dpar(1) ) then
     ipar(30) = 1
     go to 1000
  end if

  !
  ! And then the 2n case
  !

  call pcondrsubr( work(:,5), work(:,3), ipar )
  call matvecsubr( work(:,5), work(:,9), ipar )
  call pcondlsubr( work(:,5), work(:,9), ipar )

  work(:,7) = work(:,7) - alpha * work(:,5)
  gamma = ( normfun( ipar(3), work(:,7), 1 )) / tau
  c = 1 / sqrt( 1 + gamma * gamma )
  tau = tau * gamma * c

  work(:,8) = work(:,3) + ((oldgamma * oldgamma * eta) / alpha) * work(:,8)
  eta = c * c * alpha
  xvec = xvec + eta * work(:,8)

  oldgamma = gamma

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 )
  case (1)
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 ) / rhsnorm
  case (2)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), ipar )
     residual = normfun( ipar(3), work(:,10), 1 )
  case (3)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), ipar )
     residual = normfun( ipar(3), work(:,10), 1 ) / rhsnorm
  case (4)
     call matvecsubr( xvec, work(:,9), ipar )
     work(:,9) = work(:,9) - rhsvec
     call pcondlsubr( work(:,10), work(:,9), 1 )
     residual = normfun( ipar(3), work(:,10), 1 ) / precrhsnorm
  case (5)
     work(:,9) = eta * work(:,8)
     residual = normfun( ipar(3), work(:,9), 1 )
  case (6)
     upperb = real( sqrt( 2.0 * iter_count ) * tau / rhsnorm)
     if ( ( upperb / dpar(1) ) .lt. 10.0 ) then
        call pcondrsubr( work(:,10), xvec, ipar )
        call matvecsubr( work(:,10), work(:,9), ipar )
        work(:,10) = work(:,9) - rhsvec
        call pcondlsubr( work(:,9), work(:,10), ipar )
        residual = normfun( ipar(3), work(:,9), 1 ) / rhsnorm
     else
        residual = upperb
     end if
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,9), ipar, dpar )
  case default
     call pcondrsubr( work(:,10), xvec, ipar )
     call matvecsubr( work(:,10), work(:,9), ipar )
     work(:,10) = work(:,9) - rhsvec
     call pcondlsubr( work(:,9), work(:,10), ipar )
     residual = normfun( ipar(3), work(:,9), 1 )
  end select

  if ( residual .lt. dpar(1) ) then
     ipar(30) = 1
     go to 1000
  end if

  !
  ! Produce debugging output if desired
  !

  if ( ipar(5) .ne. 0 ) then
     if ( mod(iter_count, ipar(5)) .eq. 0 ) then
        if ( ipar(12) .eq. 6 ) then
           write (*, '(I8, 2E17.7)') iter_count, residual, upperb
        else
           write (*, '(I8, E17.7)') iter_count, residual
        end if
     end if
  end if

  !
  ! Part 2C
  !

  rho = dotprodfun( ipar(3), work(:,4), 1, work(:,7), 1 )
  beta = rho / oldrho
  work(:,3) = work(:,7) + beta * work(:,3)
  call pcondrsubr( work(:,6), work(:,3), ipar )
  call matvecsubr( work(:,6), work(:,9), ipar )
  call pcondlsubr( work(:,6), work(:,9), ipar )

  ! Note: we still have MATRIX * work(:,3) in work(:,5)

  work(:,1) = work(:,6) + beta * work(:,5) + beta * beta * work(:,1)
  work(:,2) = work(:,3)

  oldrho = rho

  !
  ! Return next time back to the iteration loop (without initialization)
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

  ! Compute the unpreconditioned xvec

  call pcondrsubr( work(:,10), xvec, ipar )
  xvec = work(:,10)

  if ( ipar(5) .ne. 0 ) then
     if ( ipar(12) .eq. 6 ) then
        write (*, '(I8, 2E17.7)') iter_count, residual, upperb
     else
        write (*, '(I8, E17.7)') iter_count, residual
     end if
  end if
  ipar(31) = iter_count

  return

  ! End of execution
  !*********************************************************************

end subroutine huti_stfqmrsolv

!*************************************************************************
