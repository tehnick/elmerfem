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
Module:     ecif_bodyelement1D.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   One dimensional bodyelemet ie. vertex. 

************************************************************************/

#ifndef _ECIF_BODYELEMENT1D_
#define _ECIF_BODYELEMENT1D_

#include "ecif_bodyElement.h"


class BodyElement1D : public BodyElement
{   
public:
  BodyElement1D();
  BodyElement1D(int tag);
  BodyElement1D(int tag, GcPoint* point);
  BodyElement1D(GcPoint* point);
  BodyElement1D(ecif_Element_X& trx_element);
  BodyElement1D(ecif_Vertex_X& trx_vertex);
  ~BodyElement1D();

  int addMeshElement(int elem_id, short direction) { nodeId = elem_id; return 1;}
  bool checkOuterBoundaries() { return false;}
  void convertVertexIds2Vertices() {}
  BodyElement* createElement(int nof_vertices, int* vertex_ids, ecif_geometryType gt);
  int compareOrientation(BodyElement* oe) { return 1;}
  void draw(Renderer* renderer, flagName geometry_type, int body_id);
  virtual void draw(Renderer* renderer, flagName geometry_type, int body_id, int direction) {draw(renderer,geometry_type,body_id);}
  virtual void draw(Renderer* renderer, flagName geometry_type, int body_id, int direction, bool is_first_loop) {draw(renderer,geometry_type,body_id);}
  matchType findCommonBoundary(BodyElement* other_element,
                               BodyElement*& common );
  int findMeshBorderNodes(int buf_size, int* ids_buffer);
  int getMeshElementId(int index);
  int getNofVertices() { return 1; }
  BodyElementList* getOuterBoundary() { return NULL;}
  double getParamArea(Geometry* gp) { return 0.0;}
  ParamValues* getParamValues(Geometry* gp) { return NULL;}
  bool hasInside(BodyElement* other_element) { return false;}
  void init(char* be_name = NULL);
  static void initClass(Model* model);
  bool isBemBoundary() { return false; }
  bool isInnerBoundary() {return false;}
  bool isOk() {return true;}
  void markActiveMeshObjects(bool*& active_object_flags) {};
  //ostream& output_emf(ostream& out, short indent_size, short indent_level, bool isOnSymmAxis);
  ostream& output_mif(ostream& out);
  GcPoint* param2Point(double u_p, double v_p = 0) { return NULL;}  
  ParamPair* point2Param(GcPoint* p) { return NULL;}

protected:
  int getLastTag() {return last_tag;}
  void initLabelData();
  int newTag() { return ++last_tag;}
  int nofCurrentMeshHSources;
  void setLastTag(int ltag) { last_tag = ltag; }

  static int last_tag;
  int nodeId;
};
 
  
#endif

