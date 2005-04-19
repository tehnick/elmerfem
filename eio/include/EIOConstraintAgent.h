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

#ifndef EIOCONSTRAINTAGENT_H
#define EIOCONSTRAINTAGENT_H

#include "EIOModelManager.h"

const int constraintFiles = 2;

class EIOConstraintAgent
{
public:
  EIOConstraintAgent(EIOModelManager *mm);
  ~EIOConstraintAgent();
  int openConstraints();
  int closeConstraints();
  int descriptor(int& cCount);

  int nextConstraint(int& tag, int& field, 
		     int *constraintType, double *constraintValue);
protected:
  EIOModelManager *manager;
  fstream constraintFileStream[constraintFiles];
  int constraintCount;
};
#endif  /* EIOCONSTRAINTAGENT_H */

