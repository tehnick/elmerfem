// Elmer mesh structure

#ifndef MESHTYPE_H
#define MESHTYPE_H

typedef struct {
  double x[3];
} node_t;

typedef struct {
  int vertex[2];
} edge_t;

typedef struct {
  int vertex[3];
  double normal[3];
  int parent[2];
  int index;
} boundaryelement_t;

typedef struct {
  int vertex[4];
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
