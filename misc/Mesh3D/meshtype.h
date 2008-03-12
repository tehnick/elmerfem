// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

#define GEN_TETLIB       1000      // tetgen
#define GEN_NGLIB        1001      // netgen
#define GEN_ELMERGRID    1002      // elmergrid

#define PDE_BOUNDARY     3001      // pde boundary
#define PDE_BULK         3002      // pde bulk

// node structure
class node_t {
 public:
  double x[3];                     // 3d-coordinates
  int index;                       // optional tag
};

// zero dimensional elements
class point_t {
 public:
  int nature;                      // PDE_BOUNDARY, PDE_BULK, ...
  int code;                        // element code for Elmer (101)
  int nodes;                       // number of nodes (0 or 1)
  int node;                        // node number
  int index;                       // bc/mat index as defined in input file
};

// one dimensional elements
class edge_t {
 public:
  int nature;                      // PDE_BOUNDARY, PDE_BULK, ...
  int code;                        // element code for Elmer (202, 203, ...)
  int nodes;                       // number of nodes (2, 3, ...)
  int *node;                       // list of nodes
  int surfaces;                    // number of parent surfaces
  int *surface;                    // list of parent surfaces
  int index;                       // bc/mat index as defined in input file
};

// two dimensional elements
class surface_t {
 public:
  int nature;                      // PDE_BOUNDARY, PDE_BULK, ...
  int code;                        // element code for Elmer (303, 306, ...)
  int nodes;                       // number of nodes (3, 4, ...)
  int *node;                       // list of nodes
  int edges;                       // number of edges
  int *edge;                       // list of edges
  int elements;                    // number of parent elements
  int *element;                    // list of parent elements
  double normal[3];                // unit (outward) normal
  int index;                       // bc/mat index as defined in input file
};

// three dimensional objects
class element_t {
 public:
  int nature;                      // PDE_BULK, ...
  int code;                        // element code for Elmer (504, 808, ...)
  int nodes;                       // number of nodes
  int *node;                       // list of nodes
  int index;                       // mat index as defined in input file
};

// mesh structure
class mesh_t {
 public:
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
