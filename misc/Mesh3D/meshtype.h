// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

typedef struct {
  double x[3];
  int index;
} node_t;

typedef struct {
  int node[2];
  int boundaryelements;
  int *boundaryelement;
  int index;
} edge_t;

typedef struct {
  int node[3];
  int edge[3];
  int element[2];
  int index;
  double normal[3];
} boundaryelement_t;

typedef struct {
  int node[4];
  int index;
} element_t;

typedef struct {
  int nodes;
  node_t *node;

  int edges;
  edge_t *edge;

  int boundaryelements;
  boundaryelement_t *boundaryelement;

  int elements;
  element_t *element;
} mesh_t;

#endif
