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
Module:     ecif_inputAbaqus.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A base class for input of Abaqus type (*.inp) mesh files.

************************************************************************/

#ifndef _ECIF_INPUT_ABAQUS
#define _ECIF_INPUT_ABAQUS

#include "ecif_input.h"

// Abaqus input file keywords
const char ABQ_ELEMENT[] = "ELEMENT";
const char ABQ_ELSET[] = "ELSET";
const char ABQ_HEADING[] = "HEADING";
const char ABQ_MATERIAL[] = "MATERIAL";
const char ABQ_NODE[] = "NODE";
const char ABQ_NSET[] = "NSET";
const char ABQ_SOLID_SECTION[] = "SOLID SECTION";

const short MAX_NOF_ABAQUS_ELEMENT_SETS = 9999;
const short MAX_NOF_ABAQUS_NODE_SETS = 9999;


struct AbaqusElementSet {
  AbaqusElementSet();
  ~AbaqusElementSet();
  int firstElementId;
  int parentTag;
  int extParentTag;
  char* name;
  int nofElements;
};

inline
AbaqusElementSet::AbaqusElementSet()
{
  firstElementId = NO_INDEX;
  parentTag = NO_INDEX;
  extParentTag = NO_INDEX;
  name = NULL;
  nofElements = 0;
}

inline
AbaqusElementSet::~AbaqusElementSet()
{
  delete[] name;
}


struct AbaqusNodeSet {
  AbaqusNodeSet();
  ~AbaqusNodeSet();
  char* name;
  int nofNodes;
};

inline
AbaqusNodeSet::AbaqusNodeSet()
{
  name = NULL;
  nofNodes = 0;
}

inline
AbaqusNodeSet::~AbaqusNodeSet()
{
  delete[] name;
}



const int ABQ_LINEWIDTH = 80;

class Model;

//*****
class InputAbaqus : public Input
{
public:
  InputAbaqus(enum ecif_modelDimension m_dim,
             ifstream& infile, char* filename);
  ~InputAbaqus();
  bool readMeshGeometry();
protected:
  int elementCodeCounters[1 + MAX_NOF_ELEM_CODES];
  AbaqusElementSet  elementSets[MAX_NOF_ABAQUS_ELEMENT_SETS];
  AbaqusNodeSet  nodeSets[MAX_NOF_ABAQUS_NODE_SETS];
  int nofElementSets;
  int nofNodeSets;
  //
  AbaqusElementSet* addElementSet(char* elem_set_name);
  void countNofBoundaryAndBulkElements();
  bool create_mesh_geometry();
  int findElementCode(char* abaqus_name);
  AbaqusElementSet* findElementSet(char* elem_set_name);
  enum ecif_modelDimension findCadModelDimension() { return ECIF_ND; }
  enum ecif_modelDimension findMeshModelDimension();
  bool isAxisymmetricElement(char* abaqus_name);
  bool next_is_keyword_line();
  Rc readElements(AbaqusElementSet* set, int elem_code,
                  bool count_only, int& elem_counter);
  bool readElementSet(AbaqusElementSet* set, int& body_counter, int& bndr_counter, bool count_only);
  bool readElementSetName(istrstream* strm, char* buffer, int buffer_len);
  bool readElementType(istrstream* strm, char* buffer, int buffer_len);
  bool readKeyword(istrstream* strm, char* buffer, int buffer_len);
  bool readKeywordValue(istrstream* strm, char* keyword, char* buffer, int buffer_len);
  Rc readNodes(bool count_only, int& node_counter);
  bool readNodeSet(char* node_set_name, bool count_only);
  bool readNodeSetName(istrstream* strm, char* buffer, int buffer_len);
  Rc readMeshData(int& elem_counter, int& node_counter, bool count_only);
  bool readMeshHeader();
  void skip_comment_lines();
} ;

#endif
