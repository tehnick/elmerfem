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
Module:     ecif_inputIdeasWF.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Class reads I-deas wireframe-type input files. 

************************************************************************/

#ifndef _ECIF_INPUT_IDEAS_WF_ 
#define _ECIF_INPUT_IDEAS_WF_

#include "ecif_inputIdeas.h"

//*****
class InputIdeasWF : public InputIdeas
{
public:        
  InputIdeasWF(enum ecif_modelDimension m_dim,
               ifstream& in_file, char* in_filename);  
protected:
  IdNumberTable bodyNumbers;
  bool readCadGeometry(); 
  char* readBodyName(char* fileline); 
  int readBodyNbr(char* fileline); 
  int readWireFrameBody();
  ecif_geometryType readGeomType(char* s); 
  bool readLine(Body* body, char* buffer); 
  bool readNurbs(Body* body, char* buffer); 
}; 

#endif
