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
Module:     ecif_bodyParameter.cpp
Language:   C++
Date:       24.01.01
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Implementation

************************************************************************/

#include "ecif_bodyParameter.h"
#include "ecif_model.h"
#include "ecif_parameterField.h"

//Initialize static class variables.
int BodyParameter::last_id = 0;
Model* BodyParameter::model = NULL;


// Constructors
BodyParameter::BodyParameter()
{
}


BodyParameter::BodyParameter(int pid) : Parameter(pid)
{
}


BodyParameter::BodyParameter(int pid, int parent_id, char* data_string, char* param_name)
{
  setData(pid, parent_id, data_string, param_name);
}


void
BodyParameter::initClass(Model* mdl)
{
  BodyParameter::model = mdl;
  BodyParameter::last_id = 0;
}


void
BodyParameter::setName(char* param_name)
{
  Parameter::setName(param_name, "BodyParameter");
}
