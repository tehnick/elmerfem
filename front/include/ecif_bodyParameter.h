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
Module:     ecif_constant.h
Language:   C++
Date:       24.01.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for body parameters.

************************************************************************/

#ifndef _ECIF_BODY_PARAMETER_
#define _ECIF_BODY_PARAMETER_

#include "ecif_parameter.h"


// ****** BodyParameter parameter class ******
class BodyParameter : public Parameter{
public:
  BodyParameter();
  BodyParameter(int pid);
  BodyParameter(int pid, int parent_id, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "BodyParameter"; }
  const char* getArrayName() { return "BodyParameter"; }
  const char* getEmfName() { return "Body Parameter"; }
  const char* getSifName() { return "Body Parameter"; }
  ecif_parameterType getParameterType() { return ECIF_BODY_PARAMETER; }
  static void initClass(Model* model);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
