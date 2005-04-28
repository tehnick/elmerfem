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
Module:     ecif_modelParameter.h
Language:   C++
Date:       13.02.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for (user defined) model (generic) paremeters.

************************************************************************/

#ifndef _ECIF_MODEL_PARAMETER_
#define _ECIF_MODEL_PARAMETER_

#include "ecif_def.h"
#include "ecif_parameter.h"


// ****** ModelParameter parameter class ******
class ModelParameter : public Parameter{
public:
  ModelParameter();
  ModelParameter(int pid);
  ModelParameter(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "ModelParameter"; }
  const char* getArrayName() { return "ModelParameter"; }
  const char* getEmfName() { return "Model Parameter"; }
  const char* getSifName() { return ""; }
  ecif_parameterType getParameterType() { return ECIF_MODEL_PARAMETER; }
  static void initClass(Model* model);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
