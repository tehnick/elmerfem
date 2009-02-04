/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: CSC - IT Center for Science Ltd.
*                         Keilaranta 14, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front 
Module:     ecif_inputEmf.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Elmer model file reader (.emf files)

************************************************************************/

#ifndef _ECIF_INPUT_EMF 
#define _ECIF_INPUT_EMF

#include "ecif_def.h"
#include "ecif_input.h"
#include "ecif_inputFront.h"
 
struct emf_ObjectData_X;

class InputEmf : public InputFront
{       
public:        
  InputEmf(enum ecif_modelDimension m_dim, ifstream& infile, char* filename);   
  void copyEmfParameters();
protected:
  static int copyEmfParametersCallBack(void** user_data);
  bool readCadGeometry();
  static int readEmfGeometryCallBack(void** user_data);
  static int readEmfGeometryMsgCallBack(char* msg_buffer);
  //static int readEmfElement(InputEmf* emf_input, emf_ObjectData_X* object_data, ecif_topologyType element_type);
  //static int readEmfElementLoop(InputEmf* emf_input, emf_ObjectData_X* object_data);
  static int readEmfHeader(InputEmf* emf_input, emf_ObjectData_X* object_data);
  static int readEmfParameter(emf_ObjectData_X* object_data,
                              ecif_parameterType parameter_type,
                              const Parameter*& param,
                              bool add_to_model);
  static int readEmfTimestamps(emf_ObjectData_X* object_data);

};
 
#endif
