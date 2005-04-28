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
Module:     ecif_inputFront.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Elmer model file (.emf files) reader

************************************************************************/

#ifndef _ECIF_INPUT_FRONT
#define _ECIF_INPUT_FRONT

#include "front_egfdefs.h"

#include "ecif_def.h"
#include "ecif_def_stl.h"
#include "ecif_input.h"

struct emf_ObjectData_X;


class InputFront : public Input
{
public:
  InputFront(enum ecif_modelDimension m_dim, ifstream& infile, const char* filename);
  static bool geometryIsChecked() { return isChecked;}
protected:
  static bool addElementComponent(ecif_Element_X& tx, ecif_geometryType gtype);
  static bool addFunctionComponentData( ecif_DllArg& da ,ecif_ElementComponent_X& tx);
  static ecif_geometryType getElementGeometryType(const char* type, ecif_Element_X& tx);
  virtual bool readCadHeader() {return true;} // Header for the cad geometry file
  static int readBody(emf_ObjectData_X* object_data);
  static bool readColor(emf_ObjectData_X* object_data, Color4 color);
  static int readEdge(emf_ObjectData_X* object_data);
  static int readElementGeometry1D(emf_ObjectData_X* object_data, ecif_ElementComponent_X& tx);
  static int readElementGeometry2D(emf_ObjectData_X* object_data, ecif_ElementComponent_X& tx);
  static int readElementGeometry3D(emf_ObjectData_X* object_data, ecif_ElementComponent_X& tx);
  static int readElementGroup(emf_ObjectData_X* object_data);
  static int readElementLoop(emf_ObjectData_X* object_data);
  static int readFace(emf_ObjectData_X* object_data);
  static int readIncludeFile(emf_ObjectData_X* object_data);
  static bool readName(emf_ObjectData_X* od, char*& name);
  static int readVertex(emf_ObjectData_X* object_data);
  static int readVertexTable(emf_ObjectData_X* object_data);
  static bool InputFront::storeMatcData(MatcValueTable& matcTable, const char* key, emf_ObjectData_X* od);
  static int unknownFieldMsg(emf_ObjectData_X* object_data, bool is_fatal = true);
  static int unknownObjectMsg(emf_ObjectData_X* object_data, bool is_fatal = true);

  static void readNumericVector(int read_count, int*& target_vector, int init_count = 0, int init_value = 0);
  static void readNumericVector(int read_count, double*& target_vector, int init_count = 0, double init_value = 0.0);
  static void readPoint3(int read_count, Point3& point, double init_value = 0.0);

  static double inputUnit;  // Input geometry scale: 1.0 <-> meters, 0.001 <--> mm etc.
  static bool isChecked;
  static bool isEgfInput;
  static bool isEmfInput;
};

#endif
