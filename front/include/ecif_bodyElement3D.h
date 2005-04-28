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
Module:     ecif_bodyelement3D.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Derived from Bodyelement-class. 
            Handles 3D-bodyelements.

************************************************************************/

#ifndef _ECIF_BODYELEMENT3D_
#define _ECIF_BODYELEMENT3D_

#include "ecif_bodyElement.h"

                                  
class BodyElement3D : public BodyElement
{     
public:
  BodyElement3D();
  BodyElement3D(int nof_vertices, int* vertex_ids, Geometry* pGmtr, int code = 0, char* name = 0);
  BodyElement3D(int nof_vertices, int* vertex_ids);
  BodyElement3D(int nof_vertices, int* vertex_tags, ecif_FaceGeometry_X* params);
  BodyElement3D(ecif_Element_X& trx_element);
  BodyElement3D(int tag, int parent1_tag, int parent2_tag, int nof_mesh_elements);
  BodyElement3D(int parent1_tag, int parent2_tag, int nof_mesh_elements);
  ~BodyElement3D();

  int addAllPendingSubElements();
  void addCoveringElement(BodyElement* se, beStatus se_stat);
  void addMeshBorderAsSubElement();
  int addPendingEdge(int edge_tag);
  int addPendingVertex(int vertex_tag);
  bool checkOuterBoundaries();
  int compareOrientation(BodyElement* oe);
  BodyElement* createElement(int nof_vertices, int* vertex_tags, ecif_geometryType gt);
  //void draw(Renderer* renderer, flagName geometry_type, int body_id, int direction, bool is_first_loop);
  int findMeshBorderNodes(int buf_size, int* ids_buffer);
  BodyElement* getSubElement(int index);
  void init(char* be_name = NULL);
  static void initClass(Model* model);
  //bool isInnerBoundary();
  bool isOk();
  bool isOnSamePlane(GcPoint& start_p1,
                     GcPoint& end_p1, GcPoint& end_p2) {return false;}
  matchType matchToLinear(BodyElement* be2, BodyElement*& common);
  matchType matchToNurbs(BodyElement* be2, BodyElement*& common);

protected:
  int calcDirection(BodyElement* sub_element);
  BodyElementList* findOuterBoundary();
  int getLastTag() { return last_tag; }
  int newTag() { return ++last_tag;}
  void setLastTag(int ltag) { last_tag = ltag;}

  static int last_tag;
  IdArray pendingEdgeTags;
  IdArray pendingVertexTags;
};

#endif
