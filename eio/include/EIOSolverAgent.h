/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , Center for Scientific Computing,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: Center for Scientific Computing
*                         Tietotie 6, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Data base interface (EIO)
Author(s):  Harri Hakula 10.03.98
************************************************************************/

#ifndef EIOSOLVERAGENT_H
#define EIOSOLVERAGENT_H

#include "EIOModelManager.h"

const int solverFiles = 4;

class EIOSolverAgent
{
public:
  EIOSolverAgent(EIOModelManager *mm);
  ~EIOSolverAgent();

  int createSolver();
  int openSolver();
  int closeSolver();

  int writeDescription(int& linsys, int& procs);
  int readDescription(int& linsys, int& procs);
  int writeSolverRecord(int& equation,
			int& main_type,
			int& sub_type,
			int& precond_type,
			int& stabilization,
			int& max_iter,
			double& stop_tol,
			double& steady_stop_tol,
			int& linearization,
			int& lin_max_iter,
			double& lin_stop_tol,
			int& lin_use_picard,
			int& lin_use_newton,
			int& newton_after_iter,
			double& newton_after_tol);
  int readSolverRecord(int& equation,
		       int& main_type,
		       int& sub_type,
		       int& precond_type,
		       int& stabilization,
		       int& max_iter,
		       double& stop_tol,
		       double& steady_stop_tol,
		       int& linearization,
		       int& lin_max_iter,
		       double& lin_stop_tol,
		       int& lin_use_picard,
		       int& lin_use_newton,
		       int& newton_after_iter,
		       double& newton_after_tol);
  int writeTimestepDescription(int& dependence,int& rlen);
  int readTimestepDescription(int& dependence,int& rlen);
  int writeTimestepRecord(int& type,
			       int *nof_timesteps,
			       double *timestep_sizes,
			       int *output_intervals,
			       int& steady_max_iter);
  int readTimestepRecord(int& type,
			       int *nof_timesteps,
			       double *timestep_sizes,
			       int *output_intervals,
			       int& steady_max_iter);
private:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream solverFileStream[solverFiles];
  // Sizes
  int len;
};

#endif /* EIOSOLVERAGENT_H */
