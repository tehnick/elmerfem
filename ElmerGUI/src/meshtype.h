// Elmer mesh structure

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
  double x[3];                     // 3d-coordinates
  int index;                       // optional tag
  int elements;                    // nof elements connected to the node
  int *element;                    // list of element indices
};

// base element class
class element_t {
 public:
  int nature;                      // PDE_BULK, ...
  int code;                        // element code for Elmer (504, 808, ...)
  int nodes;                       // number of nodes
  int *node;                       // list of nodes
  int index;                       // bc/mat index as defined in input file
  int selected;                    // element is selected or not
};

// zero dimensional elements
class point_t: public element_t {
 public:
  bool sharp_point;                // marker
  int edges;                       // number of parent edges
  int *edge;                       // list of parent edges
};

// one dimensional elements
class edge_t: public element_t {
 public:
  bool sharp_edge;                 // marker
  int points;                      // number of child points
  int *point;                      // list of points
  int surfaces;                    // number of parent surfaces
  int *surface;                    // list of parent surfaces
};

// two dimensional elements
class surface_t: public element_t {
 public:
  int edges;                       // number of child edges
  int *edge;                       // list of child edges
  int elements;                    // number of parent elements
  int *element;                    // list of parent elements
  double normal[3];                // unit (outward) normal
  double vertex_normals[4][3];     // unit (outward) normal on corner points
};

// mesh class
class mesh_t {
 public:
  mesh_t();
  ~mesh_t();

  bool isUndefined();
  void clear();

  int cdim;                        // model  coordinate dimension
  int dim;                         // model max element dimension
  int nodes;                       // number of nodes
  node_t *node;                    // array of nodes
  int points;                      // number of point elements
  point_t *point;                  // array of point elements
  int edges;                       // number of edge elements
  edge_t *edge;                    // array of edge elements
  int surfaces;                    // number of surface elements
  surface_t *surface;              // array of surface elements
  int elements;                    // number of volume elements
  element_t *element;              // array of volume elements
};

#endif // MESHTYPE_H
