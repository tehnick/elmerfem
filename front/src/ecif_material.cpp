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
Module:     ecif_material.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Implementation

************************************************************************/

#include "ecif_material.h"
#include "ecif_parameterField.h"

//Initialize static class variables.
int Material::last_id = 0;
Model* Material::model = NULL;


// Constructors
Material::Material()
{
}


Material::Material(int pid) : Parameter(pid)
{
}


Material::Material(int pid, int parent_id,  char* data_string, char* param_name)
{
  setData(pid, parent_id, data_string, param_name);
}


void
Material::initClass(Model* mdl)
{
  Material::model = mdl;
  Material::last_id = 0;
}


void
Material::setName(char* param_name)
{
  Parameter::setName(param_name, "Material");
}



void
Material::setValue(char* data_string) {

  Parameter::setValue(data_string);

  // Check status (must have Density!)
  status = STATUS_OK;

#if 0
  if ( NULL == getParameterField(EFN_DENSITY) ){
    status = MATERIAL_DENSITY_MISSING;
  }
#endif

}




