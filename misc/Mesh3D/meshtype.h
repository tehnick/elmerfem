// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

#define GEN_UNKNOWN      1000      // unknown
#define GEN_TETLIB       1001      // tetgen
#define GEN_NGLIB        1002      // netgen
#define GEN_ELMERGRID    1003      // elmergrid

#define PDE_UNKNOWN      3000      // pde uknown
#define PDE_BOUNDARY     3001      // pde boundary
#define PDE_BULK         3002      // pde bulk

// node class
class node_t {
 public:
  double x[3];                     // 3d-coordinates
  int index;                       // optional tag
};

// base element class
class element_t {
 public:
  int nature;                      // PDE_BULK, ...
  int code;                        // element code for Elmer (504, 808, ...)
  int nodes;                       // number of nodes
  int *node;                       // list of nodes
  int index;                       // bc/mat index as defined in input file
};

// zero dimensional elements
class point_t: public element_t {
 public:
  int edges;                       // number of parent edges
  int *edge;                       // list of parent edges
};

// one dimensional elements
class edge_t: public element_t {
 public:
  bool sharp_edge;                 // marker
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
};


// mesh class
class mesh_t {
 public:
  int dim;                         // model dimension
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
