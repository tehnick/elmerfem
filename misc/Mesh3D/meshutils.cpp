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
#define UNKNOWN -1
#define MORETHANTWO -2
#define INLINESTUFF \
if(!found) { \
  h->node = n; \
  h->parent[0] = i; \
  h->parent[1] = UNKNOWN; \
  h->next = new hash_t; \
  h = (hash_t*)h->next; \
  h->node = UNKNOWN; \
  h->parent[0] = UNKNOWN; \
  h->parent[1] = UNKNOWN; \
  h->next = NULL; \
} else { \
  if(h->parent[1] == UNKNOWN ) { \
    h->parent[1] = i; \
  } else { \
    h->parent[0] = MORETHANTWO; \
    h->parent[1] = MORETHANTWO; \
  } \
} 
  
  typedef struct {
    int node;
    int parent[2];
    void *next;
  } hash_t;
  
  int keys = mesh->nodes;

  hash_t *hash = new hash_t[keys];

  for(int i=0; i<keys; i++) {
    hash[i].node = UNKNOWN;
    hash[i].parent[0] = UNKNOWN;
    hash[i].parent[1] = UNKNOWN;
    hash[i].next = NULL;
  }

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *be = &mesh->boundaryelement[i];

    int v0 = be->vertex[0];
    int v1 = be->vertex[1];
    int v2 = be->vertex[2];
    
    // edge 0-1
    int m = (v0<v1) ? v0 : v1;
    int n = (v0<v1) ? v1 : v0;
    
    hash_t *h = &hash[m];
    bool found = false;

    while(h->node > UNKNOWN) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }
    
    INLINESTUFF

    // edge 1-2
    m = (v1<v2) ? v1 : v2;
    n = (v1<v2) ? v2 : v1;

    h = &hash[m];
    found = false;

    while(h->node > UNKNOWN) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }

    INLINESTUFF

    // edge 2-0
    m = (v2<v0) ? v2 : v0;
    n = (v2<v0) ? v0 : v2;

    h = &hash[m];
    found = false;

    while(h->node > UNKNOWN) {
      if(h->node == n) {
	found = true;
	break;
      }
      h = (hash_t*)h->next;
    }

    INLINESTUFF
  }

  // count edges:
  int edges = 0;
  for(int i=0; i<keys; i++) {
    hash_t *h = &hash[i];
    while(h->node > UNKNOWN) {
      edges++;
      h = (hash_t*)h->next;
    }
  }
  
  cout << "Found " << edges << " edges on boundary" << endl;
  
  mesh->edges = edges;
  delete [] mesh->edge;
  mesh->edge = new edge_t[edges];
  
  edges = 0;
  for(int i=0; i<keys; i++) {
    hash_t *h = &hash[i];
    while(h->node > UNKNOWN) {
      mesh->edge[edges].vertex[0] = i;
      mesh->edge[edges].vertex[1] = h->node;
      mesh->edge[edges].parent[0] = h->parent[0];
      mesh->edge[edges].parent[1] = h->parent[1];
      // cout << edges << " " << i << " " << h->node << " " 
      // << h->parent[0] << " " << h->parent[1] << endl;
      edges++;
      h = (hash_t*)h->next;
    }
  }

  // is this sufficient?
  delete [] hash;
}



// Find sharp edges for boundary elements...
//-----------------------------------------------------------------------------
void Meshutils::findSharpEdges(mesh_t *mesh, mesh_t *newmesh)
{
#define UNKNOWN -1
#define MORETHANTWO -2
#define PI 3.14159
#define LIMIT 15.0
  
  cout << "Find sharp edges" << endl;
  cout << "Edges on boundary: " << mesh->edges << endl;
  
  double *angle = new double[mesh->edges];
  
  int count = 0;
  for(int i=0; i<mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];
    
    int p0 = edge->parent[0];
    int p1 = edge->parent[1];
    
    // trivially sharp:
    if(p0 < 0 || p1 < 0) {
      angle[i] = 180.0;
    } else {
      double *n0 = mesh->boundaryelement[p0].normal;
      double *n1 = mesh->boundaryelement[p1].normal;
      double cosofangle = n0[0]*n1[0] + n0[1]*n1[1] + n0[2]*n1[2];
      angle[i] = acos(cosofangle) / PI * 180.0;
    }    

    if(sqrt(angle[i]*angle[i]) > LIMIT)
      count++;

    // cout << i << " " << angle[i] << endl;
  }

  cout << "Found " << count << " sharp edges" << endl;

  newmesh->edges = count;
  // delete [] newmesh->edge;
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
}
