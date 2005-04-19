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

#ifndef EIOMESHAGENT_H
#define EIOMESHAGENT_H

#include "EIOModelManager.h"

struct cache_node
{
  int tag;
  int constraint;
  double x,y,z;
};


class EIOMeshAgent
{
public:
  EIOMeshAgent(EIOModelManager *mm, int split=0, int part=0);
  ~EIOMeshAgent();

  int createMesh(const char *dir);
  int openMesh(const char *dir);
  int closeMesh();
  
  // READ
  int read_descriptor(int& nodeC, int& elementC, int& boundaryElementC, 
		      int& usedElementTypes, int* usedElementTypeTags,
		      int* usedElementTypeCount);
  int read_nextElementConnections(int& tag, int& body, int& type, int *pdofs, int* nodes);
  int read_nextElementCoordinates(int& tag, int& body, int& type, int* nodes,
			     double *coord);
  int read_nextBoundaryElement(int& tag, int& boundary,
                               int& leftElement, int& rightElement,
                               int& type, int* nodes, double* coord);
  int read_allNodes(int *tags,double* coord);

  // WRITE
  int write_descriptor(int& nodeC, int& elementC, int& boundaryElementC, 
		       int& usedElementTypes,
		       int* elementTypeTags,
		       int* elementCountByType);
  int write_node(int& tag, int& type, double* coord);
  int write_elementConnections(int& tag, int& body, int& type, int* nodes);
  int write_boundaryElement(int& tag, int& boundary, 
			    int& leftElement, int& rightElement, 
			    int& type, int* nodes);
  // NEW
  int read_partDescriptor(int& shared);
  int read_sharedNode(int& tag, 
			 int& constraint,      
			 double *coord, 
			 int& partcount, 
			 int *parts);

private:
  // We "use" ModelManager in every Agent.
  EIOModelManager *manager;

  // All streams
  fstream *meshFileStream;
  // Sizes
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

  int sharedNodeCount;
  int borderElementCount;

  // Storage
  cache_node *clist;

  int dim;

  // Setting
  int parallel;
  int meshFiles;

  void cache_nodes();

  int copy_coords(double *target, const int address);
  cache_node * search_node(const int address);
};

#endif /* EIOMESHAGENT_H */
