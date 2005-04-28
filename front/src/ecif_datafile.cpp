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
Module:     ecif_datafile.cpp
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Implementation

************************************************************************/

#include "ecif_datafile.h"
#include "ecif_model.h"
#include "ecif_parameterField.h"

//Initialize static class variables.
int Datafile::last_id = 0;
Model* Datafile::model = NULL;


// Constructors
Datafile::Datafile()
{
}


Datafile::Datafile(int pid) : Parameter(pid)
{
}


Datafile::Datafile(int pid, char* data_string, char* param_name)
{
  setData(pid, data_string, param_name);
}


void
Datafile::initClass(Model* mdl)
{
  Datafile::model = mdl;
  Datafile::last_id = 0;
}


// Method prints all datafile parameter's fields into output stream.
// Output of parameter name and id number is controlled by
// output_parameter_type flag
// Check that GebhardtFactors and Viewfactors filenames
// are really needed, ie. mode has DiffuseGray boundary conditions
ostream&
Datafile::output_sif(ostream& out, short indent_size, short indent_level, SifOutputControl& soc)
{
  if (soc.outputType) {

    LibFront::output_string(out, indent_size, indent_level, getSifName());

    if (soc.outputId)
      out << " " << id;

    out << endl;
  } else {
    indent_level++;
  }

  bool has_diff_gray = model->modelHasDiffuseGrayRadiation();

  for (short i = 0; i < nofFields; i++) {

    ParameterField* pf = fields[i];

    if ( LibFront::in(pf->getSifName(), "Gebhardt Factors") ||
         LibFront::in(pf->getSifName(), "View Factors") 
       ) {

      if (!has_diff_gray) {
        continue;
      }
    }

    // Check that field exists and it should be output
    if ( !pf->isActiveInstance() || 
         pf->getNofDataStrings() == 0 ||
         ( !soc.outputAll && !pf->isSifOutputField() )
         ) {
      continue;
    }

    pf->output_sif(out, indent_size, indent_level, soc.sectionName);

  }

  return out;
}


void
Datafile::setName(char* param_name)
{
  Parameter::setName(param_name, "Datafile");
}



