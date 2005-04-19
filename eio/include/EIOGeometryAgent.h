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

#ifndef EIOGEOMETRYAGENT_H
#define EIOGEOMETRYAGENT_H

#include "EIOModelManager.h"

const int geometryFiles = 6;

class EIOGeometryAgent
{
public:
  EIOGeometryAgent(EIOModelManager *mm);
  ~EIOGeometryAgent();
  int createGeometry();
  int openGeometry();
  int closeGeometry();

  int setDescriptor(int& bodyC, int& boundaryC, int& outerC, int& innerC,
		    int& vertexC, int& maxLooplen, int& loopC);
  int descriptor(int& bodyC, int& boundaryC, int& outerC, int& innerC,
		 int& vertexC, int& maxLooplen, int& loopC);

  int writeBody(int& tag, int& meshControl, int& loopC, int *loopv);
  int nextBody(int& tag, int& meshControl, int& loopC, int *loopv);

  int writeLoop(int& tag, int& filed, int *nodes);
  int nextLoop(int& tag, int& filed, int *nodes);

  int writeElement(int& tag, int& cTag, int& meshControl, int& type,
		   int& nodeC, int *nodes);
  int nextElement(int& tag, int& cTag, int& meshControl, int& type,
		  int& nodeC, int *nodes);

  int writeNode(int& tag, int& cTag, double *coord);
  int nextNode(int& tag, int& cTag, double *coord);

  int writeBoundary(int& tag, int& left, int& right);
  int nextBoundary(int& tag, int& left, int& right);
protected:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream geometryFileStream[geometryFiles];

  // Sizes
  int bodies;
  int boundaries;
  int outer;
  int inner;
  int vertices;
  int loops;
  int maxloop;
};

#endif /* EIOGEOMETRYAGENT_H */
