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
  
  double xmid = (xmin + xmax)/2.0;
  double ymid = (ymin + ymax)/2.0;
  double zmid = (zmin + zmax)/2.0;

  double xlen = (xmax - xmin)/2.0;
  double ylen = (ymax - ymin)/2.0;
  double zlen = (zmax - zmin)/2.0;

  double s = xlen;

  if(ylen > s)
    s = ylen;
  
  if(zlen > s)
    s = zlen;
  
  s *= 1.1;
  
  double *result = new double[10];

  result[0] = xmin;
  result[1] = xmax;
  result[2] = ymin;
  result[3] = ymax;
  result[4] = zmin;
  result[5] = zmax;

  result[6] = xmid;
  result[7] = ymid;
  result[8] = zmid;

  result[9] = s;

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
#define UNKNOWN -1

#define RESETENTRY              \
    h->node = UNKNOWN;          \
    h->boundaryelements = 0;    \
    h->boundaryelement = NULL;  \
    h->next = NULL;

  int keys = mesh->nodes;

  class hashEntry {
  public:
    int node;
    int boundaryelements;
    int *boundaryelement;
    hashEntry *next;
  };
  
  hashEntry *hash = new hashEntry[keys];

  bool found;
  hashEntry *h;

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    RESETENTRY;
  }

  static int edgemap[][2] = {{0,1}, {1,2}, {2,0}};
  
  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *be = &mesh->boundaryelement[i];
    
    // loop over edges
    for(int e=0; e<3; e++) {

      int n0 = be->node[edgemap[e][0]];
      int n1 = be->node[edgemap[e][1]];

      int m = (n0<n1) ? n0 : n1;
      int n = (n0<n1) ? n1 : n0;

      h = &hash[m];
      found = false;
      while(h->next) {                                       
	if(h->node == n) {
	  found = true;
	  break;
	}
	h = h->next;
      }                                                      
      
      if(!found) {
	h->node = n;
	h->boundaryelements = 1;
	h->boundaryelement = new int[1];
	h->boundaryelement[0] = i;
	h->next = new hashEntry;
	h = h->next;
	RESETENTRY;
      } else {
	int *tmp = new int[h->boundaryelements];
	for(int j=0; j<h->boundaryelements; j++)
	  tmp[j] = h->boundaryelement[j];
	delete [] h->boundaryelement;
	h->boundaryelement = new int[h->boundaryelements+1];
	for(int j=0; j<h->boundaryelements; j++)
	  h->boundaryelement[j] = tmp[j];
	h->boundaryelement[h->boundaryelements++] = i;
	delete [] tmp;
      }
    }
  }

  // count edges:
  int edges = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h = h->next) 
      edges++;
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
      e->node[0] = i;
      e->node[1] = h->node;
      e->boundaryelements = h->boundaryelements;
      e->boundaryelement = new int[e->boundaryelements];
      for(int j=0; j < e->boundaryelements; j++)
	e->boundaryelement[j] = h->boundaryelement[j];
      e->index = UNKNOWN;
      h = h->next;
    }
  }

  delete [] hash;

  // Inverse map
  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];

    for(int j=0; j < e->boundaryelements; j++) {
      int k = e->boundaryelement[j];
      boundaryelement_t *be = &mesh->boundaryelement[k];

      for(int r=0; r<3; r++) {
	if(be->edge[r] < 0) {
	  be->edge[r] = i;
	  break;
	}
      }
    }
  }  
}



// Find sharp edges for boundary elements...
//-----------------------------------------------------------------------------
void Meshutils::findSharpEdges(mesh_t *mesh, double limit)
{
#define PI 3.14159
#define UNKNOWN -1
#define SHARP 0
  
  cout << "Limit: " << limit << " degrees" << endl;
  cout.flush();

  double angle;
  int count = 0;
  
  for(int i=0; i<mesh->edges; i++) {
    edge_t *edge = &mesh->edge[i];

    edge->index = UNKNOWN;

    if( edge->boundaryelements == 2 ) {
      int b0 = edge->boundaryelement[0];
      int b1 = edge->boundaryelement[1];    
      double *n0 = mesh->boundaryelement[b0].normal;
      double *n1 = mesh->boundaryelement[b1].normal;
      double cosofangle = n0[0]*n1[0] + n0[1]*n1[1] + n0[2]*n1[2];
      angle = acos(cosofangle) / PI * 180.0;
    } else {
      angle = 180.0;
    }    
    
    if(sqrt(angle*angle) > limit) {
      edge->index = SHARP;
      count++;
    }
  }

  cout << "Found " << count << " sharp edges" << endl;
}


// Divide boundary by sharp edges...
//-----------------------------------------------------------------------------
int Meshutils::divideBoundaryBySharpEdges(mesh_t *mesh)
{
#define UNKNOWN -1
#define SHARP 0

  class Bc {
  public:
    void propagateIndex(mesh_t* mesh, int index, int i) {
      boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

      // index is ok
      if(boundaryelement->index != UNKNOWN)
	return;

      // set index
      boundaryelement->index = index;

      // propagate index
      for(int j=0; j<3; j++) {
	int k = boundaryelement->edge[j];
	edge_t *edge = &mesh->edge[k];

	// skip sharp edges
	if(edge->index != SHARP) {
	  for(int m=0; m < edge->boundaryelements; m++) {
	    int n = edge->boundaryelement[m];
	    propagateIndex(mesh, index, n);
	  }
	}
      }
    }
  };
  
  Bc *bc = new Bc;
  
  // reset bc-indices:
  for(int i=0; i < mesh->boundaryelements; i++)
    mesh->boundaryelement[i].index = UNKNOWN;

  // recursively determine boundary parts:
  int index = 0;
  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];
    if(boundaryelement->index == UNKNOWN) {
      index++;
      bc->propagateIndex(mesh, index, i);
    }
  }

  cout << "Divided boudary into " << index << " parts" << endl;

  delete bc;

  return index;
}
