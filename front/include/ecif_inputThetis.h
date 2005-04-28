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
Module:     ecif_inputThetis.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   A base class for input classes. 
  These classes are used for reading CAD-specific files.

************************************************************************/

#ifndef _ECIF_INPUT_THETIS 
#define _ECIF_INPUT_THETIS

#include "ecif_def.h"
#include "ecif_input.h"


// Virtual base class for input - actual class depends on type of the file to read in.

class InputThetis : public Input
{       
public:        
  InputThetis(enum ecif_modelDimension m_dim,
              ifstream& infile, char* filename);   
  virtual bool processMeshFileData();
  bool readMeshGeometry();
protected:
  enum ecif_modelDimension findModelDimension(ifstream& infile);
  bool read_boundary();
  Rc readMeshBoundaryElements(int nof_bndr_elements);
  Rc readMeshElements(int nof_elements);
  Rc readMeshNodes(int nof_nodes);
} ;
 
#endif
