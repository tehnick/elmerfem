#include <iostream>
#include <math.h>
#include "meshutils.h"

using namespace std;


Meshutils::Meshutils()
{
}


Meshutils::~Meshutils()
{
}


// Bounding box...
//-----------------------------------------------------------------------------
double* Meshutils::boundingBox(mesh_t *mesh)
{
  double xmin = +9e9;
  double xmax = -9e9;

  double ymin = +9e9;
  double ymax = -9e9;

  double zmin = +9e9;
  double zmax = -9e9;

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];
    
    if(node->x[0] > xmax) 
      xmax = node->x[0];
    
    if(node->x[0] < xmin) 
      xmin = node->x[0];
    
    if(node->x[1] > ymax) 
      ymax = node->x[1];

    if(node->x[1] < ymin) 
      ymin = node->x[1];

    if(node->x[2] > zmax) 
      zmax = node->x[2];

    if(node->x[2] < zmin) 
      zmin = node->x[2];
  }
  
  double *result = new double[6];

  result[0] = xmin;
  result[1] = xmax;
  result[2] = ymin;
  result[3] = ymax;
  result[4] = zmin;
  result[5] = zmax;

  return result;
}

// Delete mesh...
//-----------------------------------------------------------------------------
void Meshutils::clearMesh(mesh_t *mesh)
{
  if(mesh != (mesh_t*)NULL) {
    if(mesh->element != (element_t*)NULL) 
      delete [] mesh->element;
    
    if(mesh->boundaryelement != (boundaryelement_t*)NULL) 
      delete [] mesh->boundaryelement;
    
    if(mesh->edge != (edge_t*)NULL) 
      delete [] mesh->edge;
    
    if(mesh->node != (node_t*)NULL)
      delete [] mesh->node;
    
    delete [] mesh;
  }
}


// Find edges for boundary elements...
//-----------------------------------------------------------------------------

void Meshutils::findBoundaryElementEdges(mesh_t *mesh)
{
  int keys = mesh->nodes;

  class hashEntry {
  public:
    int node;
    int parents;
    int parent[2];
    hashEntry *next;
  };
  
  hashEntry *hash = new hashEntry[keys];

  bool found;
  hashEntry *h;

#define RESETENTRY                 \
    h->node = -1;                  \
    h->parents = 0;                \
    h->parent[0] = -1;             \
    h->parent[1] = -1;             \
    h->next = NULL;

#define INLINESTUFF                \
    h = &hash[m];                  \
    found = false;                 \
    while(h->next) {               \
      if(h->node == n) {           \
	found = true;              \
	break;                     \
      }                            \
      h = h->next;                 \
    }                              \
    if(!found) {                   \
      h->node = n;                 \
      h->parents = 1;              \
      h->parent[0] = i;            \
      h->next = new hashEntry;     \
      h = h->next;                 \
      RESETENTRY;                  \
    } else {                       \
      if(h->parents < 2)           \
	h->parent[h->parents] = i; \
      h->parents++;                \
    }

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    RESETENTRY;
  }

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *be = &mesh->boundaryelement[i];

    int v0 = be->vertex[0];
    int v1 = be->vertex[1];
    int v2 = be->vertex[2];
    
    // edge 0-1
    int m = (v0<v1) ? v0 : v1;
    int n = (v0<v1) ? v1 : v0;

    INLINESTUFF;
        
    // edge 1-2
    m = (v1<v2) ? v1 : v2;
    n = (v1<v2) ? v2 : v1;

    INLINESTUFF;

    // edge 2-0
    m = (v2<v0) ? v2 : v0;
    n = (v2<v0) ? v0 : v2;

    INLINESTUFF;

  }

  // count:
  int edges = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h->next) {
      edges++;
      h = h->next;
    }
  }

  cout << "Found " << edges << " edges on boundary" << endl;

  mesh->edges = edges;
  // delete [] mesh->edge;
  mesh->edge = new edge_t[edges];

  edges = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h->next) {
      edge_t *e = &mesh->edge[edges++];
      e->vertex[0] = i;
      e->vertex[1] = h->node;
      e->normal[0] = 0.0;
      e->normal[1] = 0.0;
      e->normal[2] = 0.0;
      e->parent[0] = h->parent[0];
      e->parent[1] = h->parent[1];
      if((h->parents <= 0) || (h->parents > 2)) {
	e->parent[0] = -2;
	e->parent[1] = -2;
      }
      e->index = -1;
      h = h->next;
    }
  }

  delete [] hash;
}



// Find sharp edges for boundary elements...
//-----------------------------------------------------------------------------
mesh_t* Meshutils::findSharpEdges(mesh_t *mesh)
{
#define PI 3.14159
#define LIMIT 20.0
  
  double *angle = new double[mesh->edges];
  
  int count = 0;
  for(int i=0; i<mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    
    int p0 = edge->parent[0];
    int p1 = edge->parent[1];
    
    if(p0 < 0 || p1 < 0) {
      // trivially sharp:
      angle[i] = 180.0;
    } else {
      // compute sharpness:
      double *n0 = mesh->boundaryelement[p0].normal;
      double *n1 = mesh->boundaryelement[p1].normal;
      double cosofangle = n0[0]*n1[0] + n0[1]*n1[1] + n0[2]*n1[2];
      angle[i] = acos(cosofangle) / PI * 180.0;
    }    

    if(sqrt(angle[i]*angle[i]) > LIMIT)
      count++;
  }

  cout << "Found " << count << " sharp edges" << endl;

  // Allocate new mesh:
  mesh_t *newmesh = new mesh_t;

  newmesh->nodes = 0;
  newmesh->node = NULL;
  newmesh->boundaryelements = 0;
  newmesh->boundaryelement = NULL;
  newmesh->elements = 0;
  newmesh->element = NULL;
  newmesh->edges = count;
  newmesh->edge = new edge_t[count];

  count = 0;
  for(int i=0; i<mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    if(sqrt(angle[i]*angle[i]) > LIMIT) {
      edge_t *newedge = &newmesh->edge[count++];
      newedge->vertex[0] = edge->vertex[0];
      newedge->vertex[1] = edge->vertex[1];
      newedge->parent[0] = edge->parent[0];
      newedge->parent[1] = edge->parent[1];
      newedge->index = edge->index;
    }
  }

  delete [] angle;

  return newmesh;
}
