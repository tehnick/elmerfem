# 1 "huti_bicgstab.src"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "huti_bicgstab.src"
!
! Subroutine to implement BiConjugate Gradient Stabilised iteration
!
! $Id: huti_bicgstab.src,v 1.1.1.1 2005/04/15 10:31:18 vierinen Exp $

# 1 "huti_intdefs.h" 1
# 7 "huti_bicgstab.src" 2
# 1 "huti_fdefs.h" 1
# 8 "huti_bicgstab.src" 2

!*************************************************************************
!*************************************************************************
!
! This subroutine is based on a book by Barret et al.:
! Templates for the Solution of Linear Systems: Building Blocks for
! Iterative Methods, 1993.
!
! All matrix-vector operations are done externally, so we do not need
! to know about the matrix structure (sparse or dense). Memory allocation
! for the working arrays has also been done externally.

!*************************************************************************
! Work array is used in the following order:
! work(:,1) = r tilde (zero)
! work(:,2) = p
! work(:,3) = p tilde
! work(:,4) = v
! work(:,5) = s
! work(:,6) = s tilde
! work(:,7) = t
! work(:,8) = r
!
!*************************************************************************
! Definitions to make the code more understandable and to make it look
! like the pseudo code
!
# 56 "huti_bicgstab.src"
!*************************************************************************
!*************************************************************************
! Complex version
!*************************************************************************
!*************************************************************************

subroutine huti_cbicgstabsolv ( ndim, wrkdim, xvec, rhsvec, &
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

  complex :: rho, oldrho, alpha, beta, omega
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
     call pcondlsubr( work(:,2), rhsvec, ipar )
     precrhsnorm = normfun( ipar(3), work(:,2), 1 )
  end if

  ! Generate vector xvec if needed

  if ( ipar(14) .eq. 0 ) then
     call huti_crandvec ( xvec, ipar )
  else if ( ipar(14) .ne. 1 ) then
     xvec = 1
  end if

  call matvecsubr( xvec, work(:,8), ipar )
  work(:,8) = rhsvec - work(:,8)
  work(:,1) = work(:,8)
  work(:,2) = 0; work(:,4) = 0
  oldrho = 1; omega = 1; alpha = 0

  !
  ! This is where the loop starts (that is we continue from here after
  ! the first iteration)
  !

300 continue

  rho = dotprodfun( ipar(3), work(:,1), 1, work(:,8), 1 )
  if ( rho .eq. 0 ) then
     ipar(30) = 35
     go to 1000
  end if

  beta = ( rho * alpha ) / ( oldrho * omega )
  work(:,2) = work(:,8) + beta * ( work(:,2) - omega * work(:,4) )

  call pcondlsubr( work(:,4), work(:,2), ipar )
  call pcondrsubr( work(:,3), work(:,4), ipar )
  call matvecsubr( work(:,3), work(:,4), ipar )

  alpha = rho / dotprodfun( ipar(3), work(:,1), 1, work(:,4), 1 )
  work(:,5) = work(:,8) - alpha * work(:,4)

  residual = normfun( ipar(3), work(:,5), 1 )
  if ( residual .lt. 1.17549435E-38 ) then
     xvec = xvec + alpha * work(:,3)
     ipar(30) = 36
     go to 1000
  end if

  call pcondlsubr( work(:,7), work(:,5), ipar )
  call pcondrsubr( work(:,6), work(:,7), ipar )
  call matvecsubr( work(:,6), work(:,7), ipar )

  omega = ( dotprodfun( ipar(3), work(:,7), 1, work(:,5), 1 ) ) / &
          ( dotprodfun( ipar(3), work(:,7), 1, work(:,7), 1 ) )
  xvec = xvec + alpha * work(:,3) + omega * work(:,6)
  work(:,8) = work(:,5) - omega * work(:,7)

  !
  ! Check the convergence against selected stopping criterion
  !

  select case (ipar(12))
  case (0)
     call matvecsubr( xvec, work(:,6), ipar )
     work(:,3) = work(:,6) - rhsvec
     residual = normfun( ipar(3), work(:,3), 1 )
  case (1)
     call matvecsubr( xvec, work(:,6), ipar )
     work(:,3) = work(:,6) - rhsvec
     residual = normfun( ipar(3), work(:,3), 1 ) / rhsnorm
  case (2)
     residual = normfun( ipar(3), work(:,8), 1 )
  case (3)
     residual = normfun( ipar(3), work(:,8), 1 ) / rhsnorm
  case (4)
     residual = normfun( ipar(3), work(:,8), 1 ) / precrhsnorm
  case (5)
     work(:,3) = alpha * work(:,3) + omega * work(:,6)
     residual = normfun( ipar(3), work(:,3), 1 )
  case (10)
     residual = stopcfun( xvec, rhsvec, work(:,8), ipar, dpar )
  case default
     call matvecsubr( xvec, work(:,6), ipar )
     work(:,3) = work(:,6) - rhsvec
     residual = normfun( ipar(3), work(:,3), 1 )
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

  if ( omega .eq. 0 ) then
     ipar(30) = 37
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

end subroutine huti_cbicgstabsolv

!*************************************************************************
