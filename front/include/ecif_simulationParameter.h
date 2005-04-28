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
Module:     ecif_simulationParameter.h
Language:   C++
Date:       13.02.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A Base class for (user defined) simulation paremeters.

************************************************************************/

#ifndef _ECIF_SIMULATION_PARAMETER_
#define _ECIF_SIMUALTION_PARAMETER_

#include "ecif_parameter.h"


// ****** SimulationParameter parameter class ******
class SimulationParameter : public Parameter{
public:
  SimulationParameter();
  SimulationParameter(int pid);
  SimulationParameter(int pid, char* data_string, char* param_name);
  int getLastId() {return last_id;}
  void setLastId(int lid) {last_id = lid;}
  const char* getGuiName() { return "Simulation"; }
  const char* getArrayName() { return "SimulationParameter"; }
  const char* getEmfName() { return "Simulation Parameter"; }
  const char* getSifName() { return ""; }
  ecif_parameterType getParameterType() { return ECIF_SIMULATION_PARAMETER; }
  static void initClass(Model* model);
  void setName(char* param_name);
protected:
  static int last_id;
  static Model* model;
};


#endif
