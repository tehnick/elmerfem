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

#ifndef EIOPARTWRITER_H
#define EIOPARTWRITER_H

#include "EIOModelManager.h"

const int partWriterFiles = 5;

class EIOPartWriter
{
public:
  EIOPartWriter(int& partCount, EIOModelManager *mm);
  ~EIOPartWriter();

  int createPartitioning(const char *dir);
  int closePartitioning();
  
  int activatePart(int part);
  int deactivatePart();

  // WRITE
  int write_descriptor(int& nodeC,
		       int& sharedC,
		       int& elementC, 
		       int& borderC, 
		       int& boundaryC, 
		       int& usedElementTypes,
		       int* elementTypeTags,
		       int* elementCountByType);

  int write_node(int& tag, int& type, double *coord, int& partC, int *parts);
  int write_element(int& tag, int& body, int& type, int *nodes, int& border);

private:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream meshFileStream[partWriterFiles];
  // Sizes
  char newdir[PATH_MAX];

  int parts;
  int me;

  int nodeCount;
  int elementCount;
  int boundaryElementCount;
  int elementTypes;
  int *elementTypeTags;
  int *elementTypeCount;
  //
  void openStreams();
  void closeStreams();
};

#endif /* EIOPARTWRITER_H */
