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
Module:     ecif_boundaryParameter.cpp
Language:   C++
Date:       24.01.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Implementation

************************************************************************/

#include "ecif_boundaryParameter.h"
#include "ecif_model.h"
#include "ecif_parameterField.h"

//Initialize static class variables.
int BoundaryParameter::last_id = 0;
Model* BoundaryParameter::model = NULL;


// Constructors
BoundaryParameter::BoundaryParameter()
{
}


BoundaryParameter::BoundaryParameter(int pid) : Parameter(pid)
{
}


BoundaryParameter::BoundaryParameter(int pid, int parent_id, char* data_string, char* param_name)
{
  setData(pid, parent_id, data_string, param_name);
}


void
BoundaryParameter::initClass(Model* mdl)
{
  BoundaryParameter::model = mdl;
  BoundaryParameter::last_id = 0;
}


void
BoundaryParameter::setName(char* param_name)
{
  Parameter::setName(param_name, "BoundaryParameter");
}
