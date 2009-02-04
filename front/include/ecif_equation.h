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
Module:     ecif_equation.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for equationbody-force.

************************************************************************/

#ifndef _ECIF_EQUATION_
#define _ECIF_EQUATION_

#include "ecif_parameter.h"


// ****** Equation class ******
class Equation : public Parameter {
public:
  Equation();
  Equation(int pid);
  Equation(int pid, char* values, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Equation"; }
  const char* getArrayName() { return "Equation"; }
  const char* getEmfName() { return "Equation"; }
  const char* getSifName() { return SIF_EQUATION; }
  ecif_parameterType getParameterType() { return ECIF_EQUATION; }
  static void initClass(Model* model);
  virtual ostream& output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc);
  virtual ostream& outputSolverTargetFields_sif(ostream& out, short indent_size, short indent_level, const char* source_eq_name, NameSet& targetFieldNames);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
  ostream& output_equationWithVariables_sif(ostream& out, short indent_size, short indent_level,
                                            ParameterField* equation_pf,
                                            ParameterField* equation_vars_pf);
};


#endif
