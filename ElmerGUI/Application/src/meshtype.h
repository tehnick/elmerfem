/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ElmerGUI mesh_t (Elmer mesh structure)                                   *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - IT Center for Science Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#ifndef MESHTYPE_H
#define MESHTYPE_H

enum GenTypes {
  GEN_UNKNOWN,
  GEN_TETLIB,
  GEN_NGLIB,
  GEN_ELMERGRID
};

enum PdeTypes { 
  PDE_UNKNOWN,
  PDE_BOUNDARY,
  PDE_BULK
 };

// node class
class node_t {
 public:
  node_t();
  ~node_t();

  void setX(int, double);
  double getX(int);
  void setXvec(double*);
  double* getXvec(void);
  void setIndex(int);
  int getIndex(void);

 private:
  double x[3];                     // 3d-coordinates
  int index;                       // optional tag
};

// base element class
class element_t {
 public:
  element_t();
  ~element_t();

  void setNature(int);
  int getNature(void);
  void setCode(int);
  int getCode(void);
  void setNodes(int);
  int getNodes(void);
  void setIndex(int);
  int getIndex(void);
  void setSelected(int);
  int getSelected(void);
  int getNodeIndex(int);
  void setNodeIndex(int, int);
  int* getNodeIndexes();
  void newNodeIndexes(int);
  void deleteNodeIndexes(void);

 private:
  int nature;                      // PDE_BULK, ...
  int code;                        // element code for Elmer (504, 808, ...)
  int nodes;                       // number of nodes
  int index;                       // bc/mat index as defined in input file
  int selected;                    // element is selected or not
  int* node;                       // list of nodes
};

// zero dimensional elements
class point_t: public element_t {
 public:
  point_t();
  ~point_t();

  void setSharp(bool);
  bool isSharp(void);
  void setEdges(int);
  int getEdges(void);
  void setEdgeIndex(int, int);
  int getEdgeIndex(int);
  void newEdgeIndexes(int);
  void deleteEdgeIndexes(void);

 private:
  bool sharp_point;                // marker
  int edges;                       // number of parent edges
  int* edge;                       // list of parent edges
};

// one dimensional elements
class edge_t: public element_t {
 public:
  edge_t();
  ~edge_t();

  void setSharp(bool);
  bool isSharp(void);
  void setPoints(int);
  int getPoints(void);
  void setPointIndex(int, int);
  int getPointIndex(int);
  void newPointIndexes(int);
  void deletePointIndexes(void);
  void setSurfaces(int);
  int getSurfaces(void);
  void setSurfaceIndex(int, int);
  int getSurfaceIndex(int);
  void newSurfaceIndexes(int);
  void deleteSurfaceIndexes(void);

 private:
  bool sharp_edge;                 // marker
  int points;                      // number of child points
  int* point;                      // list of points
  int surfaces;                    // number of parent surfaces
  int* surface;                    // list of parent surfaces
};

// two dimensional elements
class surface_t: public element_t {
 public:
  surface_t();
  ~surface_t();

  void setEdges(int);
  int getEdges(void);
  void setEdgeIndex(int, int);
  int getEdgeIndex(int);
  void newEdgeIndexes(int);
  void deleteEdgeIndexes(void);
  void setElements(int);
  int getElements(void);
  void setElementIndex(int, int);
  int getElementIndex(int);
  void newElementIndexes(int);
  void deleteElementIndexes(void);
  void setNormalVec(double*);
  double* getNormalVec(void);
  double getNormal(int);
  void setNormal(int, double);
  void setVertexNormalVec(int, double*);
  void addVertexNormalVec(int, double*);
  void subVertexNormalVec(int, double*);
  double* getVertexNormalVec(int);

 private:
  int edges;                       // number of child edges  
  int* edge;                       // list of child edges
  int elements;                    // number of parent elements
  int* element;                    // list of parent elements
  double normal[3];                // unit (outward) normal
  double vertex_normals[4][3];     // unit (outward) normal on corner points
};

// mesh class
class mesh_t {
 public:
  mesh_t();
  ~mesh_t();

  bool isUndefined(void);
  void clear(void);
  bool load(char*);
  bool save(char*);
  double* boundingBox(void);
  void setCdim(int);
  int getCdim(void);
  void setDim(int);
  int getDim(void);
  void setNodes(int);
  int getNodes(void);
  void setPoints(int);
  int getPoints(void);
  void setEdges(int);
  int getEdges(void);
  void setSurfaces(int);
  int getSurfaces(void);
  void setElements(int);
  int getElements(void);
  node_t* getNode(int);
  void setNodeArray(node_t*);
  void newNodeArray(int);
  void deleteNodeArray(void);
  point_t* getPoint(int);
  void setPointArray(point_t*);
  void newPointArray(int);
  void deletePointArray(void);
  edge_t* getEdge(int);
  void setEdgeArray(edge_t*);
  void newEdgeArray(int);
  void deleteEdgeArray(void);
  surface_t* getSurface(int);
  void setSurfaceArray(surface_t*);
  void newSurfaceArray(int);
  void deleteSurfaceArray(void);
  element_t* getElement(int);
  void setElementArray(element_t*);
  void newElementArray(int);
  void deleteElementArray(void);

 private:
  void setDefaults();

  int cdim;                        // model coordinate dimension
  int dim;                         // model max element dimension
  int nodes;                       // number of nodes
  int points;                      // number of point elements
  int edges;                       // number of edge elements
  int surfaces;                    // number of surface elements
  int elements;                    // number of volume elements

  node_t* node;                    // array of nodes
  point_t* point;                  // array of point elements
  edge_t* edge;                    // array of edge elements
  surface_t* surface;              // array of surface elements
  element_t* element;              // array of volume elements

};

#endif // MESHTYPE_H
