!/***************************************************************************
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
! ***************************************************************************/
!
!
!********************************************************************
!
! File: SParIterGlobals.f90
! Software: ELMER
! File info: This module contains global variables (or pointers to them)
!            needed by the parallel version of the ELMER iterative solver.
!
! Author: Jouni Malinen, Juha Ruokolainen
!
! $Id: SParIterGlobals.f90,v 1.3 2002/09/27 07:57:44 jpr Exp $
!
!********************************************************************

MODULE SParIterGlobals

  USE Types

  IMPLICIT NONE

  TYPE HUTICtlT
     INTEGER :: Method
     INTEGER :: Precond
     DOUBLE PRECISION :: Tolerance
     INTEGER :: MaxIter
     INTEGER :: DebugLevel
  END TYPE HUTICtlT


  TYPE ErrInfoT
     INTEGER :: HUTIStatus
  END TYPE ErrInfoT

  ! Following is in correct place

  TYPE (ParEnv_t), TARGET :: ParEnv
DLLEXPORT ParEnv
  TYPE (SParIterSolverGlobalD_t), POINTER :: PIGpntr
  TYPE (SParIterSolverGlobalD_t), POINTER :: GlobalData
END MODULE SParIterGlobals

