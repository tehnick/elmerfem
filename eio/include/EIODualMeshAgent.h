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

#ifndef EIODUALMESHAGENT_H
#define EIODUALMESHAGENT_H

#include "EIOModelManager.h"

const int dualMeshFiles = 2;

class EIODualMeshAgent
{
public:
  EIODualMeshAgent(EIOModelManager *mm);
  ~EIODualMeshAgent();

  int createMesh(const char *dir);
  int openMesh(const char *dir);
  int closeMesh();
  int read_nextElementConnections(int& tag, int& type, int* nodes);
  int write_elementConnections(int& tag, int& type, int* nodes);

private:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream meshFileStream[dualMeshFiles];

  // Sizes
  int nodeCount;
  int elementCount;
  int boundaryElementCount;
  int elementTypes;
  int *elementTypeTags;
  int *elementTypeCount;

  void readHeader();
};

#endif /* EIODUALMESHAGENT_H */
