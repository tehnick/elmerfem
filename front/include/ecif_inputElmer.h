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
Program:    ELMER Front 
Module:     ecif_inputElmer.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Elmer DB reader

************************************************************************/

#ifndef _ECIF_INPUT_ELMER 
#define _ECIF_INPUT_ELMER

#include "ecif_def.h"
#include "ecif_input.h"


struct ecf_ObjectData_X;

class InputElmer : public Input
{       
public:        
  InputElmer(enum ecif_modelDimension m_dim,
             ifstream& infile, char* filename);   
  bool readMeshGeometry();
  bool processMeshFileData();

protected:
  enum ecif_modelDimension findMeshModelDimension();
  bool readMeshHeader();
  Rc readMeshBoundaryElements();
  Rc readMeshBulkElements();
  Rc readMeshNodes(int nof_nodes);
  bool resetMeshData();
};
 
#endif

