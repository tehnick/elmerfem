// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

#define GEN_TETLIB    1000
#define GEN_NGLIB     1001
#define GEN_ELMERGRID 1002

class node_t {
 public:
  double x[3];

  int index;
};

class edge_t {
 public:
  int code;

  int nodes;
  int *node;

  int boundaryelements;
  int *boundaryelement;

  int index;
};

class boundaryelement_t {
 public:
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

class element_t {
 public:
  int code;

  int nodes;
  int *node;

  int index;
};

class mesh_t {
 public:
  int nodes;
  node_t *node;

  int edges;
  edge_t *edge;

  int boundaryelements;
  boundaryelement_t *boundaryelement;

  int elements;
  element_t *element;
};

#endif
