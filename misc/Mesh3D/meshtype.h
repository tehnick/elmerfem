// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

class node_t {
 public:
  double x[3];
  int index;
};

class edge_t {
 public:
  int node[2];
  int boundaryelements;
  int *boundaryelement;
  int index;
};

class boundaryelement_t {
 public:
  int node[3];
  int edge[3];
  int element[2];
  double normal[3];
  int index;
};

class element_t {
 public:
  int node[4];
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
