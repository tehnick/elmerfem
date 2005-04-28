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
Module:     ecif_inputFidap.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A base class for input of Fidap type (*.fidap) mesh files.

************************************************************************/

#ifndef _ECIF_INPUT_FIDAP
#define _ECIF_INPUT_FIDAP

#include "ecif_input.h"


enum fidapKeywordId {
  FIDAP_NO_ID = -1,
  FIDAP_VERSION_ID,
  FIDAP_NODAL_COORDINATES_ID,
  FIDAP_ELEMENT_GROUPS_ID
};


struct FidapElementGroupInfo {
  int id;
  int elementType;
  int nofElements;
  int nofNodes;
  char entityName[80];
  bool isBulk;
  bool isBoundary;
  bool isEdge;
  int parentTag;
  int fidap_geometry;
  int fidap_type;
  const int* fidap_reorder; // Pointer to reorder vector
};


class Model;

//*****
class InputFidap : public Input
{
public:
  InputFidap(enum ecif_modelDimension m_dim,
             ifstream& infile, char* filename);
  ~InputFidap();
  bool readMeshGeometry();
protected:
  int elementCodeCounters[1 + MAX_NOF_ELEM_CODES];
  int nofElementGroups;
  //
  enum ecif_modelDimension findCadModelDimension() { return ECIF_ND; }
  enum ecif_modelDimension findMeshModelDimension();
  enum fidapKeywordId next_is_keyword_line(char* line_buffer);
  Body* findExistingBody(FidapElementGroupInfo* gi);
  BodyElement* findExistingBoundary(FidapElementGroupInfo* gi);
  Rc findMaxExternalIds(int& max_ext_nd_id, int& max_ext_el_id);
  Rc readElements(FidapElementGroupInfo& group_info, int& node_counter, int& max_ext_id, bool count_only);
  Rc readElementGroup(int& body_counter, int& bndr_counter, int& element_counter, int& max_ext_elem_id, bool count_only);
  bool readElementGroupInfo(FidapElementGroupInfo& group_info);
  bool readKeyword(char* line_buffer, char* buffer, int buffer_len);
  bool readKeywordValue(char* line_buffer, char* keyword, char* buffer, int buffer_len);
  bool readNumericKeywordValue(char* line_buffer, char* keyword, int& value);
  bool readStringKeywordValue(char* line_buffer, char* keyword, char* value_buffer);
  Rc readNodes(int nof_nodes, int& node_counter, int& max_ext_id, bool count_only);
  Rc readMeshData(int& elem_counter, int& node_counter);
  bool readMeshHeader(int& dimension);
  bool setElementType(FidapElementGroupInfo& group_info);

  ecif_modelDimension dataDimension;
} ;

#endif
