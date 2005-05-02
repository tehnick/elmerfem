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

#ifndef EIOPARTREADER_H
#define EIOPARTREADER_H

#include "EIOModelManager.h"
using namespace std;
#include <fstream>

const int partReaderFiles = 5;

struct cache_node
{
  int tag;
  int type;
  double x,y;
};

class EIOPartReader
{
public:
  EIOPartReader(int& partCount, EIOModelManager *mm);
  ~EIOPartReader();

  int openPartitioning(int& part);
  int closePartitioning(int& part);

  // READ
  int read_descriptor(int& nodeC,
		      int& sharedC,
		      int& elementC, 
		      int& borderC, 
		      int& usedElementTypes,
		      int* elementTypeTags,
		      int* elementCountByType);

  // Reading nodes and elements is more compilated than writing them, since
  // we must cache first.

  int read_nextSharedNode(int& tag, int& partC, int *parts);
  int read_borderElements(int& len, int* tags);

  int 
  read_nextElementConnections(int& tag, int& body, int& type, int* nodes);
  int read_nextElementCoordinates(
       int& tag, int& body, int& type, int* nodes, double *coord);

private:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream meshFileStream[partReaderFiles];

  // Sizes
  char newdir[PATH_MAX];

  int parts;
  int me;

  int nodeCount;
  int sharedNodeCount;
  int elementCount;
  int boundaryElementCount;
  int elementTypes;
  int *elementTypeTags;
  int *elementTypeCount;

  // Storage
  int dim;
  cache_node *clist;

  //
  void openStreams();
  void closeStreams();
};

#endif /* EIOPARTREADER_H */
