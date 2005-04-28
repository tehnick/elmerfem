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
Module:     ecif_solver.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Implementation

************************************************************************/

#include "ecif_model.h"
#include "ecif_parameterField.h"
#include "ecif_solverControl.h"
#include "ecif_userinterface.h"

//Initialize static class variables.
int SolverControl::last_id = 0;
Model* SolverControl::model = NULL;


// Constructors
SolverControl::SolverControl()
{
}


SolverControl::SolverControl(int pid) : Parameter(pid)
{
}


SolverControl::SolverControl(int pid, char* data_string, char* param_name)
{
  setData(pid, data_string, param_name);
}


void
SolverControl::initClass(Model* mdl)
{
  SolverControl::model = mdl;
  SolverControl::last_id = 0;
}


// Outputs all fields with some exceptions (ECHO_ON, CHECK_KEYWORDS)
//
// NOTE: This is used for Simulation block output
//
ostream&
SolverControl::output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc)
{
  char QM = '\"';
 
  for (short i = 0; i < nofFields; i++) {

    ParameterField* pf = fields[i];

    // Check that field exists and it should be output
    if ( !pf->isActiveInstance() ||
         pf->getNofDataStrings() == 0 ||
         ( !soc.outputAll && !pf->isSifOutputField() )
       )
      continue;

    char* fld_name = (char*)pf->getSifName();
    
    // Skip fields which are not output here!!!
    if ( LibFront::in(fld_name, SIF_ECHO_ON) ||
         LibFront::in(fld_name, SIF_CHECK_KEYWORDS) ||
         LibFront::in(fld_name, SIF_RELOAD_INPUT_FILE)
       ) {
      continue;
    }
    
    pf->output_sif(out, indent_size, 1 + indent_level, soc.sectionName);

  }

  return out;
}

void
SolverControl::setName(char* param_name)
{
  Parameter::setName(param_name, "SolverControl");
}




