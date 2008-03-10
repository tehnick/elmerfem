// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

#define GEN_TETLIB       1000
#define GEN_NGLIB        1001
#define GEN_ELMERGRID    1002

#define BOUNDARY_ELEMENT 3001
#define BULK_ELEMENT     3002

// node structure
class node_t {
 public:
  double x[3];

  int index;
};

// zero dimensional objects
class point_t {
 public:
  int nature;

  int code;

  int nodes;
  int node;
};

// one dimensional objects
class edge_t {
 public:
  int nature;

  int code;

  int nodes;
  int *node;

  int boundaryelements;
  int *boundaryelement;

  int index;
};

// two dimensional objects
class boundaryelement_t {
 public:
  int nature;

  int code;

  int nodes;
  int *node;

  int edges;
  int *edge;

  int elements;
  int *element;

  double normal[3];

  int index;
};

// three dimensional objects
class element_t {
 public:
  int nature;

  int code;

  int nodes;
  int *node;

  int index;
};

// mesh structure
class mesh_t {
 public:
  int nodes;
  node_t *node;

  int points;
  point_t *point;

  int edges;
  edge_t *edge;

  int boundaryelements;
  boundaryelement_t *boundaryelement;

  int elements;
  element_t *element;
};

#endif
