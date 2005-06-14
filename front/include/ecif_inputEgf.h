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
Module:     ecif_inputEgf.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Elmer Geometry Format "cadi" file reader

************************************************************************/

#ifndef _ECIF_INPUT_EGF 
#define _ECIF_INPUT_EGF


#include "ecif_def.h"
#include "ecif_input.h"
#include "ecif_inputFront.h"


class Model;
struct emf_ObjectData_X;

class InputEgf : public InputFront
{       
public:        
  InputEgf(enum ecif_modelDimension m_dim, ifstream& infile, char* filename);   
protected:
  bool readCadGeometry();
  static int readEgfGeometryCB(void** user_data);
  static int readEgfGeometryMsgCB(char* msg_buffer);
  static int readIncludeFile(InputEgf* egf_input, emf_ObjectData_X* od);
  //static int readEgfEdge(InputEgf* egf_input, emf_ObjectData_X* object_data);
  //static int readEgfEdgeLoop(InputEgf* egf_input, emf_ObjectData_X* object_data);
  static int readEgfHeader(InputEgf* egf_input, emf_ObjectData_X* object_data);
};
 
#endif
