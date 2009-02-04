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
Module:     ecif_input.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   A base class for input classes.
  These classes are used for reading CAD-specific files.

************************************************************************/

#ifndef _ECIF_INPUT_
#define _ECIF_INPUT_

#include "ecif_def.h"


// Virtual base class for input - actual class depends on type of the file to read in.

class Input
{
public:
  friend class Control;
  Input(enum ecif_modelDimension m_dim,
        ifstream& infile, char* filename);
  ~Input();
  bool doBreak();
  void eat_white(istream& strm);
  ecif_modelDimension getModelDimension() { return modelDimension;}
  static void initClass(Model* model);
  ecif_modelDimension loadMesh();
  virtual bool processMeshFileData();
  enum ecif_modelDimension  readCadFile();
  static void readFileLine(ifstream& infile, char* buffer, int linepos = 1);
  ecif_modelDimension readMeshFile();
  void reset(strstream& strm);
  void setModelDimension(int dimension);

protected:
  BodyElement* createBodyElement2D(Body* parent_body, int body_layer, ecif_EdgeGeometry_X& uP);
  BodyElement* createBodyElement2D(Body* parent_body, int body_layer, Point3& p1, Point3& p2);
  BodyElement* createBodyElement2D(Body* parent_body, int body_layer, int nof_points, Point3* points);
  virtual ecif_modelDimension findCadModelDimension() { return modelDimension;}
  virtual ecif_modelDimension findMeshModelDimension() { return modelDimension;}
  virtual ecif_modelDimension findModelDimension(ecif_modelDimension geom_dim);
  virtual bool readCadGeometry() {return false;}
  virtual bool readCadHeader() {return false;}
  virtual bool readMeshGeometry() {return false;}
  virtual bool readMeshHeader() {return false;}

private:
  BodyElement* createBodyElement2D(Body* parent_body, int body_layer, int v1_id, int v2_id);
  BodyElement* createBodyElement2D(Body* parent_body, int body_layer, int nof_vertices, int* vertex_ids);

protected:
  static Control* theControlCenter;
  static Model* model;

  ifstream& infile;
  char* infileName;
  bool bulkElementsAllocated;
  bool bndrElementsAllocated;
  bool edgeElementsAllocated;
  bool vrtxElementsAllocated;
  BoundBox meshBoundingBox;
  enum ecif_modelDimension inputDimension;
  enum ecif_modelDimension modelDimension;
  int maxExternalElementId;
  int maxExternalNodeId;
  int nofInputBoundaryElements;
  int nofInputBulkElements;
  int nofInputEdgeElements;
  int nofInputVertexElements;
  int nofElements;
  int nofNodes;
  int nofBulkElements;
  int nofBoundaryElements;
  int nofEdgeElements;
  int nofVertexElements;

} ;

#endif
