/*****************************************************************************
 *                                                                           *
 *  Elmer, A Finite Element Software for Multiphysical Problems              *
 *                                                                           *
 *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland    *
 *                                                                           *
 *  This program is free software; you can redistribute it and/or            *
 *  modify it under the terms of the GNU General Public License              *
 *  as published by the Free Software Foundation; either version 2           *
 *  of the License, or (at your option) any later version.                   *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with this program (in file fem/GPL-2); if not, write to the        *
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,         *
 *  Boston, MA 02110-1301, USA.                                              *
 *                                                                           *
 *****************************************************************************/

/*****************************************************************************
 *                                                                           *
 *  ELMER/Mesh3D meshutils                                                   *
 *                                                                           *
 *****************************************************************************
 *                                                                           *
 *  Authors: Mikko Lyly, Juha Ruokolainen and Peter Råback                   *
 *  Email:   Juha.Ruokolainen@csc.fi                                         *
 *  Web:     http://www.csc.fi/elmer                                         *
 *  Address: CSC - Scientific Computing Ltd.                                 *
 *           Keilaranta 14                                                   *
 *           02101 Espoo, Finland                                            *
 *                                                                           *
 *  Original Date: 15 Mar 2008                                               *
 *                                                                           *
 *****************************************************************************/

#include <iostream>
#include <stdlib.h>
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

  bool sx = xmin==xmax;
  bool sy = ymin==ymax;
  bool sz = zmin==zmax;
  mesh->cdim = 3;
  if ( sz && sy || sz && sx || sx && sy )
    mesh->cdim=1;
  else if ( sz || sy || sx)
    mesh->cdim=2;

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

// Clear mesh...
//-----------------------------------------------------------------------------
void Meshutils::clearMesh(mesh_t *mesh)
{
  if(mesh == NULL)
    return;
    
  if(mesh->elements > 0) {
    for(int i=0; i < mesh->elements; i++)
      if(mesh->element->nodes > 0)
	delete [] mesh->element[i].node;
    delete [] mesh->element;
  }

  if(mesh->surfaces > 0) {
    for(int i=0; i < mesh->surfaces; i++) {
      if(mesh->surface->nodes > 0)
	delete [] mesh->surface[i].node;
      if(mesh->surface->edges > 0)
	delete [] mesh->surface[i].edge;
      if(mesh->surface->elements > 0)
	delete [] mesh->surface[i].element;
    }
    delete [] mesh->surface;
  }

  if(mesh->edges > 0) {
    for(int i=0; i < mesh->edges; i++) {
      if(mesh->edge->nodes > 0)
	delete [] mesh->edge[i].node;
      if(mesh->edge->surfaces > 0)
	delete [] mesh->edge[i].surface;
    }
    delete [] mesh->edge;
  }

  if(mesh->points > 0) {
    for(int i=0; i < mesh->points; i++) {
      if(mesh->point->nodes > 0)
	delete [] mesh->point[i].node;
      if(mesh->point->edges > 0)
	delete [] mesh->point[i].edge;
    }
    delete [] mesh->point;
  }
  
  if(mesh->nodes > 0)
    delete [] mesh->node;
  
  delete mesh;

  cout << "Mesh cleared" << endl;
  cout.flush();
}



// Find surface elements for 3D elements when they are not provided 
//----------------------------------------------------------------------------
void Meshutils::findSurfaceElements(mesh_t *mesh)
{
#define UNKNOWN -1

#define RESETENTRY1             \
  h->node[0] = UNKNOWN;		\
  h->node[1] = UNKNOWN;  	\
  h->element[0] = UNKNOWN;	\
  h->element[1] = UNKNOWN;	\
  h->index[0] = UNKNOWN;        \
  h->index[1] = UNKNOWN;        \
  h->next = NULL;               

  class hashEntry {
  public:
    int node[2];
    int element[2];
    int index[2];
    int face;
    hashEntry *next;
  };


  if(mesh->elements == 0) return;

  int keys = mesh->nodes;  
  hashEntry *hash = new hashEntry[keys];

  bool found;
  hashEntry *h;

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    RESETENTRY1;
  }

  // TODO: only 1st and 2nd order elements

  static int familyfaces[9] = {0, 0, 0, 0, 0, 4, 5, 5, 6};

  static int faceedges8[] = {4, 4, 4, 4, 4, 4};
  static int facemap8[][8] = {{0,1,2,3,8,9,10,11}, {4,5,6,7,16,17,18,19}, {0,1,5,4,8,13,16,12}, {1,2,6,5,9,14,17,13}, {2,3,7,6,10,15,18,14}, {3,0,4,7,11,12,19,15}};

  static int faceedges7[] = {3, 3, 4, 4, 4};
  static int facemap7[][8] = {{0,1,2,6,7,8}, {3,4,5,12,13,14}, {0,1,4,3,6,10,12,9}, {1,2,5,4,7,11,13,10}, {2,0,3,5,8,9,14,11}};

  static int faceedges6[] = {4, 3, 3, 3, 3};
  static int facemap6[][8] = {{0,1,2,3,5,6,7,8}, {0,1,4,5,10,9}, {1,2,4,6,11,10}, {2,3,4,7,12,11}, {3,0,4,8,9,12}};

  static int faceedges5[4] = {3, 3, 3, 3};
  static int facemap5[][6] = {{0,1,2,4,5,6}, {0,1,3,4,8,7}, {1,2,3,5,9,8}, {2,0,3,6,7,9}};


  for(int i=0; i < mesh->elements; i++) {
    element_t *e = &mesh->element[i];

    int facenodes = 0;
    int *facemap = NULL;
    int n[4];

    int family = e->code / 100;
    int faces = familyfaces[family];

    for(int f=0; f<faces; f++) {
      if(family == 5) {
	facenodes = faceedges5[f];
	facemap = &facemap5[f][0];
      }
      else if(family == 6) {
	facenodes = faceedges6[f];
	facemap = &facemap6[f][0];
      }
      else if(family == 7) {
	facenodes = faceedges7[f];
	facemap = &facemap7[f][0];
      }
      else if(family == 8) {
	facenodes = faceedges8[f];
	facemap = &facemap8[f][0];
      }

      for(int j=0; j < facenodes; j++) 
	n[j] = e->node[facemap[j]];

      // Order indexes in an increasing order 
      for(int k=facenodes-1;k>0;k--) {
	for(int j=0;j<k;j++) {
	  if(n[j] > n[j+1]) {
	    int tmp = n[j+1];
	    n[j+1] = n[j];
	    n[j] = tmp;
	  } 
	}
      }
      
      // three nodes define a face uniquely also for rectangles
      h = &hash[n[0]];
      found = false;
      while(h->next) {                                       
	if((h->node[0] == n[1]) && (h->node[1] == n[2])) {
	  found = true;
	  break;
	}
	h = h->next;
      }                                                      
      
      if(!found) {
	h->node[0] = n[1];
	h->node[1] = n[2];
	h->element[0] = i;
	h->index[0] = e->index;
	h->face = f;
	
	h->next = new hashEntry;
	h = h->next;
	RESETENTRY1;
      } else {
	h->index[1] = e->index;
	h->element[1] = i;
      }
    }
  }

  // count faces that have different materials at either sides:
  int allsurfaces = 0;
  int surfaces = 0;
  int maxindex1 = 0;
  int maxindex2 = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h->next){
      if(h->element[0] > UNKNOWN) allsurfaces++;
      if(h->index[0] != h->index[1]) surfaces++;
      if(h->index[0] > maxindex1) maxindex1 = h->index[0];
      if(h->index[1] > maxindex2) maxindex2 = h->index[1];
      h = h->next;
    }
  }
  cout << "Found " << surfaces << " interface faces of " << allsurfaces << endl;


  // Create a index table such that all combinations of materials 
  // get different BC index  
  int indextable[maxindex1+1][maxindex2+1];
  int index1,index2;
  for(int i=0;i<=maxindex1;i++)
    for(int j=0;j<=maxindex2;j++)
      indextable[i][j] = 0;

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h->next){
      if(h->index[0] != h->index[1]) {
	index1 = h->index[0];
	index2 = h->index[1];
	if(index2 == -1) index2 = 0;
	indextable[index1][index2] = 1;
      }
      h = h->next;
    }
  }
  index1=0;
  for(int i=0;i<=maxindex1;i++)
    for(int j=0;j<=maxindex2;j++)
      if(indextable[i][j]) indextable[i][j] = ++index1;

  cout << "Boundaries were numbered up to index " << index1 << endl;

  // Finally set the surfaces:
  mesh->surfaces = surfaces;
  mesh->surface = new surface_t[mesh->surfaces]; 

  surfaces = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];

    while(h->next) {
      if(h->index[0] != h->index[1]) {
	surface_t *s = &mesh->surface[surfaces];

	s->elements = 1;
	s->element = new int[2]; 
	s->element[0] = h->element[0];
	s->element[1] = h->element[1];
	if(s->element[1] >= 0) s->elements = 2;
	
	element_t *e = &mesh->element[h->element[0]];
	int code = e->code;
	int family = code / 100;
	int f = h->face;

	int faceedges = 0;
	int *facemap = NULL;
	int degree = 1;

	if(family == 5) {
	  faceedges = faceedges5[f];
	  if( code == 510) degree = 2;
	  facemap = &facemap5[f][0];
	}
	else if(family == 6) {
	  faceedges = faceedges6[f];
	  if( code == 613) degree = 2;
	  facemap = &facemap6[f][0];
	}
	else if(family == 7) {
	  faceedges = faceedges7[f];
	  if( code == 715) degree = 2;
	  facemap = &facemap7[f][0];
	}
	else if(family == 8) {
	  faceedges = faceedges8[f];
	  if( code == 820 ) degree = 2;
	  facemap = &facemap8[f][0];
	}
	
	int facenodes = degree * faceedges;

	s->nodes = facenodes;
	s->code = 100 * faceedges + facenodes;
	s->node = new int[s->nodes];
	for(int j=0; j < s->nodes; j++) 
	  s->node[j] = e->node[facemap[j]];

	index1 = h->index[0];
	index2 = h->index[1];
	if(index2 < 0) index2 = 0;
	
	s->index = indextable[index1][index2];

	s->edges = s->nodes;
	s->edge = new int[s->edges];
	for(int j=0; j < s->edges; j++)
	  s->edge[j] = UNKNOWN;

	s->nature = PDE_BOUNDARY;
	surfaces++;
      }
      h = h->next;
    }
  }

  delete [] hash;
}



// Find parent elements for existing surfaces...
//----------------------------------------------------------------------------
void Meshutils::findSurfaceElementParents(mesh_t *mesh)
{
#define UNKNOWN -1

#define RESETENTRY0             \
  h->node[0] = UNKNOWN;		\
  h->node[1] = UNKNOWN;  	\
  h->element[0] = UNKNOWN;	\
  h->element[1] = UNKNOWN;	\
  h->next = NULL;

  class hashEntry {
  public:
    int node[2];
    int element[2];
    hashEntry *next;
  };

  int keys = mesh->nodes;
  
  hashEntry *hash = new hashEntry[keys];

  bool found;
  hashEntry *h;

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    RESETENTRY0;
  }

  // TODO: only tetrahedron at the moment

  static int facemap[][3] = {{0,1,2}, {0,1,3}, {0,2,3}, {1,2,3}};
  
  for(int i=0; i < mesh->elements; i++) {
    element_t *e = &mesh->element[i];

    for(int f=0; f<4; f++) {
      int n0 = e->node[facemap[f][0]];
      int n1 = e->node[facemap[f][1]];
      int n2 = e->node[facemap[f][2]];

      if(n2 < n1) {
	int tmp = n2;
	n2 = n1;
	n1 = tmp;
      }
      
      if(n2 < n0) {
	int tmp = n2;
	n2 = n0;
	n0 = tmp;
      }
      
      if(n1 < n0) {
	int tmp = n1;
	n1 = n0;
	n0 = tmp;
      }
      
      h = &hash[n0];
      found = false;
      while(h->next) {                                       
	if((h->node[0] == n1) && (h->node[1] == n2)) {
	  found = true;
	  break;
	}
	h = h->next;
      }                                                      
      
      if(!found) {
	h->node[0] = n1;
	h->node[1] = n2;
	h->element[0] = i;
	h->next = new hashEntry;
	h = h->next;
	RESETENTRY0;
      } else {
	h->element[1] = i;
      }      
    }
  }

  // count faces:
  int faces = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while((h = h->next) != NULL) 
      faces++;
  }
  
  cout << "Found total of " << faces << " faces" << endl;

  // Finally find parents:
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    
    int n0 = s->node[0];
    int n1 = s->node[1];
    int n2 = s->node[2];
    
    if(n2 < n1) {
      int tmp = n2;
      n2 = n1;
      n1 = tmp;
    }
    
    if(n2 < n0) {
      int tmp = n2;
      n2 = n0;
      n0 = tmp;
    }
    
    if(n1 < n0) {
      int tmp = n1;
      n1 = n0;
      n0 = tmp;
    }
    
    h = &hash[n0];
    while(h->next) {
      if((h->node[0] == n1) && (h->node[1] == n2)) {

	// should we deallocate s->element if it exists?
	s->elements = 2;
	s->element = new int[2];

	s->element[0] = h->element[0];
	s->element[1] = h->element[1];
      }
      h = h->next;
    }
  }

  delete [] hash;
}



// Find points for edge elements...
//-----------------------------------------------------------------------------
void Meshutils::findEdgeElementPoints(mesh_t *mesh)
{
  class hashEntry {
  public:
    int edges;
    int *edge;
  };

  int keys = mesh->nodes;
  
  hashEntry *hash = new hashEntry[keys];

  for(int i = 0; i < keys; i++) {
    hashEntry *h = &hash[i];
    h->edges = 0;
    h->edge = NULL;
  }

  for(int i = 0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];

    if(e->nature == PDE_BOUNDARY) {      
      for(int k = 0; k < 2; k++) {
	int n = e->node[k];
	
	hashEntry *h = &hash[n];
	
	bool found = false;
	for(int j = 0; j < h->edges; j++) {
	  if(h->edge[j] == i) {
	    found = true;
	    break;
	  }
	}
	
	if(!found) {
	  int *tmp = new int[h->edges+1];
	  for(int j = 0; j < h->edges; j++)
	    tmp[j] = h->edge[j];
	  tmp[h->edges] = i;
	  delete [] h->edge;
	  
	  h->edges++;
	  h->edge = tmp;	
	}
      }
    }
  }
  
  // count points:
  int count = 0;
  for(int i = 0; i < keys; i++) {
    hashEntry *h = &hash[i];
    if(h->edges > 0) 
      count++;
  }

  cout << "Found " << count << " points on boundary edges" << endl;
  cout.flush();

  // delete old points, if any:
  if(mesh->points > 0) {
    cout << "Deleteing old points and creating new" << endl;
    cout.flush();
    for(int i = 0; i < mesh->points; i++) {
      delete [] mesh->point[i].node;
      delete [] mesh->point[i].edge;
    }
    delete [] mesh->point;
  }

  mesh->points = count;
  mesh->point = new point_t[mesh->points];

  count = 0;
  for(int i = 0; i < keys; i++) {
    hashEntry *h = &hash[i];
    
    if(h->edges > 0) {
      point_t *p = &mesh->point[count++];
      p->nodes = 1;
      p->node = new int[1];
      p->node[0] = i;
      p->edges = h->edges;
      p->edge = new int[p->edges];
      for(int j = 0; j < p->edges; j++) {
	p->edge[j] = h->edge[j];
      }
      p->sharp_point = false;
    }
  }

  // delete temp stuff
  for(int i = 0; i < keys; i++) {
    hashEntry *h = &hash[i];
    if(h->edges > 0)
      delete [] h->edge;
  }

  delete [] hash;

  // Inverse map
  cout << "Constructing inverse map from edges to points" << endl;
  cout.flush();

  for(int i=0; i < mesh->points; i++) {
    point_t *p = &mesh->point[i];

    for(int j=0; j < p->edges; j++) {
      int k = p->edge[j];
      if ( k<0 ) continue; // ????

      edge_t *e = &mesh->edge[k];

      // allocate space for two points, if not yet done:
      if(e->points < 2) {
	e->points = 2;
	e->point = new int[2];
	e->point[0] = -1;
	e->point[1] = -1;
      }
            
      for(int r=0; r < e->points; r++) {
	if(e->point[r] < 0) {
	  e->point[r] = i;
	  break;
	}
      }
    }
  }  
}



// Find edges for surface elements...
//-----------------------------------------------------------------------------
void Meshutils::findSurfaceElementEdges(mesh_t *mesh)
{
#define UNKNOWN -1

#define RESETENTRY              \
    h->surfaces = 0;            \
    h->surface = NULL;          \
    h->next = NULL;

  int keys = mesh->nodes;

  class hashEntry {
  public:
    int nodes;
    int *node;
    int nature;
    int index;
    int surfaces;
    int *surface;
    hashEntry *next;
  };
  
  hashEntry *hash = new hashEntry[keys];

  bool found;
  hashEntry *h;

  for(int i=0; i<keys; i++) {
    h = &hash[i];
    RESETENTRY;
  }

  if ( mesh->edge && mesh->edges>0 ) {  
    // add existing edges first:
    for( int i=0; i<mesh->edges; i++ )
    {
      edge_t *edge=&mesh->edge[i];
      int n0 = edge->node[0];
      int n1 = edge->node[1];

      int m = (n0<n1) ? n0 : n1;
      int n = (n0<n1) ? n1 : n0;

      h = &hash[m];
      found = false;
      while(h->next) {                                       
        if(h->node[0] == n) {
          found = true;
          break;
        }
        h = h->next;
      }                                                      
      
      if(!found) {
        h->nodes = edge->nodes-1;
        h->node = new int[h->nodes];
        h->node[0] = n;
        for( int j=1; j<h->nodes; j++ )
        {
          h->node[j] = edge->node[j+1];
        }
        h->surfaces = edge->surfaces;
        h->surface = new int[edge->surfaces];
        for( int j=0; j<edge->surfaces; j++ ) {
          h->surface[j] = edge->surface[j];
        }
        h->index  = edge->index;
        h->nature = edge->nature;
        h->next = new hashEntry;
        h = h->next;
        RESETENTRY;
      }
    }

    mesh->edges = 0;
    delete [] mesh->edge;
  }


  static int triedgemap[][4] = { {0,1,3,6}, {1,2,4,7}, {2,0,5,8} };
  static int quadedgemap[][4] = {{0,1,4,8}, {1,2,5,9}, {2,3,6,10}, {3,0,7,11}};

  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    
    // loop over edges
    for(int e=0; e < s->edges; e++) {
      int n0, n1,nrest[2] = { -1,-1 };
      if((int)(s->code/100) == 3) {
	n0 = s->node[triedgemap[e][0]];
	n1 = s->node[triedgemap[e][1]];
	if ( s->code>=306) nrest[0] = s->node[triedgemap[e][2]];
	if ( s->code>=309) nrest[1] = s->node[triedgemap[e][3]];
      } else if((int)(s->code/100) == 4) {
	n0 = s->node[quadedgemap[e][0]];
	n1 = s->node[quadedgemap[e][1]];
	if ( s->code>=408) nrest[0] = s->node[triedgemap[e][2]];
	if ( s->code>=412) nrest[1] = s->node[triedgemap[e][3]];
      } else {
	cout << "findBoundaryElementEdges: error: unknown element code" << endl;
	exit(0);
      }

      int m = (n0<n1) ? n0 : n1;
      int n = (n0<n1) ? n1 : n0;

      h = &hash[m];
      found = false;
      while(h->next) {                                       
	if(h->node[0] == n) {
	  found = true;
	  break;
	}
	h = h->next;
      }                                                      
      
      if(!found) {
        h->nodes = 1;
        h->nodes += nrest[0] >=0 ? 1:0;
        h->nodes += nrest[1] >=0 ? 1:0;
        h->node = new int[h->nodes];
        h->node[0] = n;
        for( int j=1; j<h->nodes; j++ )
          h->node[j] = nrest[j-1];

	h->surfaces = 1;
	h->surface = new int[1];
	h->surface[0] = i;
        h->index = UNKNOWN;
        h->nature = PDE_UNKNOWN;
	h->next = new hashEntry;
	h = h->next;
	RESETENTRY;
      } else {
        int *tmp  = new int[h->surfaces];

        found = false;
        for(int j=0; j<h->surfaces; j++)
        {
          tmp[j] = h->surface[j];
          if ( tmp[j] == i ) found=true; 
        }
        if ( found ) {
          delete [] tmp;
        } else {
          delete [] h->surface;
          h->surface = new int[h->surfaces+1];
          for(int j=0; j<h->surfaces; j++)
             h->surface[j] = tmp[j];
          h->surface[h->surfaces++] = i;
          delete [] tmp;
        }
      }
    }
  }

  // count edges:
  int edges = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while((h = h->next) != NULL) 
      edges++;
  }

  cout << "Found " << edges << " edges on boundary" << endl;

  mesh->edges = edges;
  mesh->edge = new edge_t[edges];

  // Create edges:
  edges = 0;
  for(int i=0; i<keys; i++) {
    h = &hash[i];
    while(h->next) {
      edge_t *e = &mesh->edge[edges++];
      
      e->nature = h->nature;
      e->nodes = h->nodes+1;
      e->node = new int[e->nodes];
      e->node[0] = i;
      for( int j=1; j<e->nodes; j++ )
        e->node[j] = h->node[j-1];

      e->code = 200 + e->nodes;

      e->surfaces = h->surfaces;
      e->surface = new int[max(e->surfaces,2)];
      e->surface[0] = -1;
      e->surface[1] = -1;

      for(int j=0; j < e->surfaces; j++)
	e->surface[j] = h->surface[j];

      e->sharp_edge = false;

      e->index = h->index;
      e->points = 0;
      h = h->next;
    }
  }

  delete [] hash;

  // Inverse map
  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];

    for(int j=0; j < e->surfaces; j++) {
      int k = e->surface[j];
      if ( k< 0 ) continue;

      surface_t *s = &mesh->surface[k];
      
      for(int r=0; r < s->edges; r++) {
	if(s->edge[r] < 0) {
	  s->edge[r] = i;
	  break;
	}
      }
    }
  }  

#if 0
  cout << "*********************" << endl;
  for(int i=0; i<mesh->edges; i++)
    cout << "Edge " << i << " nodes " << mesh->edge[i].node[0] << " "<< mesh->edge[i].node[0] << endl;

  for(int i=0; i<mesh->surfaces; i++)
    cout << "Surface " << i << " nodes " 
	 << mesh->surface[i].node[0] << " " 
	 << mesh->surface[i].node[1] << " "
	 << mesh->surface[i].node[2] << " "
	 << " Edges " 
	 << mesh->surface[i].edge[0] << " " 
	 << mesh->surface[i].edge[1] << " "
	 << mesh->surface[i].edge[2] << " "
	 << " Parents " 
	 << mesh->surface[i].element[0] << " " 
	 << mesh->surface[i].element[1] << " "
	 << endl;

  cout.flush();
#endif

}

// Find sharp points for edge elements...
//-----------------------------------------------------------------------------
void Meshutils::findSharpPoints(mesh_t *mesh, double limit)
{
  double t0[3], t1[3];
#define PI 3.14159
#define UNKNOWN -1
#define SHARP 0
  cout << "Limit: " << limit << " degrees" << endl;
  cout.flush();
  
  double angle;
  int count = 0;
  point_t *point = NULL;
  edge_t *edge = NULL;
  Helpers *helpers = new Helpers;
  
  for(int i=0; i<mesh->points; i++) {
    point = &mesh->point[i];

    if(point->edges == 2) {
      int n = point->node[0];

      int e0 = point->edge[0];
      int e1 = point->edge[1];

      edge = &mesh->edge[e0];
      int n0 = edge->node[0];
      if(edge->node[1] != n)
	n0 = edge->node[1];

      edge = &mesh->edge[e1];
      int n1 = edge->node[0];
      if(edge->node[1] != n)
	n1 = edge->node[1];

      // unit tangent from node to node0
      t0[0] = mesh->node[n0].x[0] - mesh->node[n].x[0];
      t0[1] = mesh->node[n0].x[1] - mesh->node[n].x[1];
      t0[2] = mesh->node[n0].x[2] - mesh->node[n].x[2];
      
      // unit tangent from node to node1
      t1[0] = mesh->node[n1].x[0] - mesh->node[n].x[0];
      t1[1] = mesh->node[n1].x[1] - mesh->node[n].x[1];
      t1[2] = mesh->node[n1].x[2] - mesh->node[n].x[2];
      
      helpers->normalize(t0);
      helpers->normalize(t1);

      double cosofangle = t0[0]*t1[0] + t0[1]*t1[1] + t0[2]*t1[2];
      angle = acos(cosofangle) / PI * 180.0;
    } else {
      angle = 0.0;
    }    
    
    point->sharp_point = false;
    if(sqrt(angle*angle) < (180.0-limit) ) {
      point->sharp_point = true;
      count++;
    }
  }

  cout << "Found " << count << " sharp points" << endl;
  delete helpers;
}



// Find sharp edges for surface elements...
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

    if(edge->surfaces == 2) {
      int s0 = edge->surface[0];
      int s1 = edge->surface[1];    
      double *n0 = mesh->surface[s0].normal;
      double *n1 = mesh->surface[s1].normal;
      double cosofangle = n0[0]*n1[0] + n0[1]*n1[1] + n0[2]*n1[2];
      angle = acos(cosofangle) / PI * 180.0;
    } else {
      angle = 180.0;
    }    
    
    edge->sharp_edge = false;
    if(sqrt(angle*angle) > limit) {
      edge->sharp_edge = true;
      count++;
    }
  }

  cout << "Found " << count << " sharp edges" << endl;
}



// Divide edge by sharp points...
//-----------------------------------------------------------------------------
int Meshutils::divideEdgeBySharpPoints(mesh_t *mesh)
{
#define UNKNOWN -1
#define SHARP 0

  class Bc {
  public:
    void propagateIndex(mesh_t* mesh, int index, int i) {
      edge_t *edge = &mesh->edge[i];

      // index is ok
      if(!edge->selected || (edge->index != UNKNOWN)
        || (edge->nature != PDE_BOUNDARY)) return;
      
      // set index
      edge->index = index;

      // propagate index
      for(int j=0; j < edge->points; j++) {
	int k = edge->point[j];
	point_t *point = &mesh->point[k];

	// skip sharp points
	if(!point->sharp_point) {
	  for(int m = 0; m < point->edges; m++) {
	    int n = point->edge[m];
	    propagateIndex(mesh, index, n);
	  }
	}

      }
    }
  };
  
  
  // reset bc-indices on edges:
  int index = 0;
  int count = 0;

  for(int i=0; i < mesh->edges; i++)
  {
    edge_t *edge=&mesh->edge[i];
    if(edge->nature == PDE_BOUNDARY && !edge->selected)
      index = max(index,edge->index);
  }
  index++;

  int edge_index[index];

  for( int i=0; i<index; i++ )
    edge_index[i] = UNKNOWN;

  index = 0;
  for(int i=0; i < mesh->edges; i++)
  {
    edge_t *edge=&mesh->edge[i];
    if (edge->nature==PDE_BOUNDARY)
    {
      if ( edge->selected ) {
        count++;
        edge->index = UNKNOWN;
      } else {
        if ( edge_index[edge->index] == UNKNOWN )
          edge_index[edge->index] = ++index;
        edge->index = edge_index[edge->index];
      }
    }
  }

  if ( count==0 ) {
    cout << "No boundary edges to divde." << endl;
    return 0;
  }

  Bc *bc = new Bc;

  // recursively determine boundary parts:
  for(int i=0; i < mesh->edges; i++)
  {
    edge_t *edge=&mesh->edge[i];
    if(edge->selected && edge->index==UNKNOWN && edge->nature==PDE_BOUNDARY)
      bc->propagateIndex(mesh, ++index, i);
  }
  index++;
  
  // Create a hopefully mesh indepedent indexing of groupings to enable
  // reapplying merge/division operations after remeshing. The indices
  // are given based on group bounding box corner distances from a given
  // point. Fails if two groups have same bbox, which should not happen 
  // often though (?)
  double xmin[index], ymin[index], zmin[index];
  double xmax[index], ymax[index], zmax[index];
  double xc,yc,zc,dist[index];
  int cc[index], order[index], sorder[index];
  double g_xmin,g_xmax,g_ymin,g_ymax,g_zmin,g_zmax;

  for( int i=0; i<index; i++ )
  {
    cc[i] = 0;
    order[i] = i;
    xmin[i] = ymin[i] = zmin[i] =  1e20;
    xmax[i] = ymax[i] = zmax[i] = -1e20;
  }

  for( int i=0; i<mesh->edges; i++ )
  {
    edge_t *edge=&mesh->edge[i];
    if (edge->nature==PDE_BOUNDARY) {
      int k = edge->index;
      for( int j=0; j<edge->nodes; j++ ) {
        int n = edge->node[j];
        cc[k]++;
        xmin[k] = min( xmin[k], mesh->node[n].x[0] );
        ymin[k] = min( ymin[k], mesh->node[n].x[1] );
        zmin[k] = min( zmin[k], mesh->node[n].x[2] );
 
        xmax[k] = max( xmax[k], mesh->node[n].x[0] );
        ymax[k] = max( ymax[k], mesh->node[n].x[1] );
        zmax[k] = max( zmax[k], mesh->node[n].x[2] );
       }
    }
  }

  g_xmin = g_ymin = g_zmin =  1e20;
  g_xmax = g_ymax = g_zmax = -1e20;
  for( int i=0; i<index; i++)
  {
    g_xmin = min(xmin[i],g_xmin);
    g_ymin = min(ymin[i],g_ymin);
    g_zmin = min(zmin[i],g_zmin);

    g_xmax = max(xmax[i],g_xmax);
    g_ymax = max(ymax[i],g_ymax);
    g_zmax = max(zmax[i],g_zmax);
  }

  double g_scale = max(max(g_xmax-g_xmin,g_ymax-g_ymin),g_zmax-g_zmin);
  double g_xp = g_xmax + 32.1345 * g_scale;
  double g_yp = g_ymin - 5.3*PI  * g_scale;
  double g_zp = g_zmax + 8.1234  * g_scale;

  for( int i=0; i<index; i++ )
  {
    dist[i] = 0;
    if ( cc[i]>0 ) {
      for( int j=0; j<8; j++ ) {
        switch(j) {
          case 0: xc=xmin[i]; yc=ymin[i]; zc=zmin[i]; break;
          case 1: xc=xmax[i]; break;
          case 2: yc=xmax[i]; break;
          case 3: xc=xmin[i]; break;
          case 4: zc=zmax[i]; break;
          case 5: yc=ymin[i]; break;
          case 6: xc=xmax[i]; break;
          case 7: yc=ymax[i]; break;
        }
        dist[i] += (xc-g_xp)*(xc-g_xp);
        dist[i] += (yc-g_yp)*(yc-g_yp);
        dist[i] += (zc-g_zp)*(zc-g_zp);
      }
    }
  }

  sort_index( index, dist, order );
  for( int i=0; i<index; i++ )
    sorder[order[i]] = i;

  for( int i=0; i<mesh->edges; i++ )
    if ( mesh->edge[i].nature == PDE_BOUNDARY )
      mesh->edge[i].index = sorder[mesh->edge[i].index];

  cout << "Edge divided into " << index << " parts" << endl;
  
  delete bc;

  return index;
}


void Meshutils::sort_index(int n, double *a, int *b)
{
   int i,j,l,ir,rb;
   double ra;

   if ( n <= 1 ) return;

   l = n / 2;
   ir = n-1;
   while( true ) {
     if ( l >= 1 ) {
       l = l - 1;
       ra = a[l];
       rb = b[l];
     } else {
       ra = a[ir];
       rb = b[ir];
       a[ir] = a[0];
       b[ir] = b[0];
       ir = ir - 1;
       if ( ir == 0 ) {
         a[0] = ra;
         b[0] = rb;
         return;
       }
     }
     i = l;
     j = l + l;
     while( j <= ir ) {
       if ( j<ir  ) {
          if ( a[j]<a[j+1] ) j = j+1;
       }
       if ( ra<a[j] ) {
         a[i] = a[j];
         b[i] = b[j];
         i = j;
         j =  j + i;
       } else {
         j = ir + 1;
       }
       a[i] = ra;
       b[i] = rb;
    }
  }
}


// Divide surface by sharp edges...
//-----------------------------------------------------------------------------
int Meshutils::divideSurfaceBySharpEdges(mesh_t *mesh)
{
#define UNKNOWN -1
#define SHARP 0

  class Bc {
  public:
    void propagateIndex(mesh_t* mesh, int index, int i) {
      surface_t *surf = &mesh->surface[i];

      // index is ok
      if(!surf->selected || (surf->index != UNKNOWN) 
        || (surf->nature != PDE_BOUNDARY) ) return;

      // set index
      surf->index = index;

      // propagate index
      for(int j=0; j<surf->edges; j++) {
	int k = surf->edge[j];
	edge_t *edge = &mesh->edge[k];

	// skip sharp edges
	if(!edge->sharp_edge) {
	  for(int m=0; m < edge->surfaces; m++) {
	    int n = edge->surface[m];
	    propagateIndex(mesh, index, n);
	  }
	}
      }
    }
  };
  
  // reset bc-indices:
  int count = 0;
  int index = 0;

  for( int i=0; i<mesh->surfaces; i++ )
  {
    surface_t *surf=&mesh->surface[i];
    if( surf->nature == PDE_BOUNDARY && !surf->selected )
      index = max(index,surf->index);
  }
  index++;

  int surf_index[index];

  for( int i=0; i<index; i++ )
    surf_index[i] = UNKNOWN;

  index = 0;
  for(int i=0; i < mesh->surfaces; i++)
  {
    surface_t *surf=&mesh->surface[i];
    if (surf->nature == PDE_BOUNDARY) {
      if ( surf->selected ) {
        count++;
        surf->index = UNKNOWN;
      } else {
        if ( surf_index[surf->index] == UNKNOWN )
          surf_index[surf->index] = ++index;
        surf->index = surf_index[surf->index];
      }
    }
  }

  if ( count==0 ) {
    cout << "No boundary surfaces to divde." << endl;
    return 0;
  }


  // recursively determine boundary parts:
  Bc *bc = new Bc;

  for(int i=0; i < mesh->surfaces; i++)
  {
    surface_t *surf = &mesh->surface[i];
    if(surf->selected && surf->index==UNKNOWN && surf->nature==PDE_BOUNDARY)
      bc->propagateIndex(mesh, ++index, i);
  }
  index++;

  // Create a hopefully mesh indepedent indexing of groupings to enable
  // reapplying merge/division operations after remeshing. The indices
  // are given based on group bounding box corner distances from a given
  // point. Fails if two groups have same bbox, which should not happen 
  // often though (?)
  double xmin[index], ymin[index], zmin[index];
  double xmax[index], ymax[index], zmax[index];
  double xc,yc,zc,dist[index];
  int cc[index], order[index], sorder[index];
  double g_xmin,g_xmax,g_ymin,g_ymax,g_zmin,g_zmax;

  for( int i=0; i<index; i++ )
  {
    cc[i] = 0;
    order[i] = i;
    xmin[i] = ymin[i] = zmin[i] =  1e20;
    xmax[i] = ymax[i] = zmax[i] = -1e20;
  }

  for( int i=0; i<mesh->surfaces; i++ )
  {
    surface_t *surf=&mesh->surface[i];
    if ( mesh->surface[i].nature == PDE_BOUNDARY ) {
      int k = surf->index;
      for( int j=0; j<surf->nodes; j++ ) {
        int n = surf->node[j];
        cc[k]++;
        xmin[k] = min( xmin[k], mesh->node[n].x[0] );
        ymin[k] = min( ymin[k], mesh->node[n].x[1] );
        zmin[k] = min( zmin[k], mesh->node[n].x[2] );
 
        xmax[k] = max( xmax[k], mesh->node[n].x[0] );
        ymax[k] = max( ymax[k], mesh->node[n].x[1] );
        zmax[k] = max( zmax[k], mesh->node[n].x[2] );
      }
    }
  }

  g_xmin = g_ymin = g_zmin =  1e20;
  g_xmax = g_ymax = g_zmax = -1e20;
  for( int i=0; i<index; i++)
  {
    g_xmin = min(xmin[i],g_xmin);
    g_ymin = min(ymin[i],g_ymin);
    g_zmin = min(zmin[i],g_zmin);

    g_xmax = max(xmax[i],g_xmax);
    g_ymax = max(ymax[i],g_ymax);
    g_zmax = max(zmax[i],g_zmax);
  }

  double g_scale = max(max(g_xmax-g_xmin,g_ymax-g_ymin),g_zmax-g_zmin);
  double g_xp = g_xmax + 32.1345 * g_scale;
  double g_yp = g_ymin - 5.3*PI  * g_scale;
  double g_zp = g_zmax + 8.1234  * g_scale;

  for( int i=0; i<index; i++ )
  {
    dist[i] = 0;
    if ( cc[i]>0 ) {
      for( int j=0; j<8; j++ ) {
        switch(j) {
          case 0: xc=xmin[i]; yc=ymin[i]; zc=zmin[i]; break;
          case 1: xc=xmax[i]; break;
          case 2: yc=xmax[i]; break;
          case 3: xc=xmin[i]; break;
          case 4: zc=zmax[i]; break;
          case 5: yc=ymin[i]; break;
          case 6: xc=xmax[i]; break;
          case 7: yc=ymax[i]; break;
        }
        dist[i] += (xc-g_xp)*(xc-g_xp);
        dist[i] += (yc-g_yp)*(yc-g_yp);
        dist[i] += (zc-g_zp)*(zc-g_zp);
      }
    }
  }

  sort_index( index, dist, order );
  for( int i=0; i<index; i++ )
    sorder[order[i]] = i;

  for( int i=0; i<mesh->surfaces; i++ )
    if ( mesh->surface[i].nature == PDE_BOUNDARY )
      mesh->surface[i].index = sorder[mesh->surface[i].index];

  cout << "Surface divided into " << index-1 << " parts" << endl;

  delete bc;

  return index;
}



// Find surface element normals...
//-----------------------------------------------------------------------------
void Meshutils::findSurfaceElementNormals(mesh_t *mesh)
{
  static double a[3], b[3], c[3];
  double center_surface[3], center_element[3], center_difference[3];
  Helpers *helpers = new Helpers;
  int u, v, w, e0, e1, i0, i1, bigger;

  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    
    u = surface->node[0];
    v = surface->node[1];

    if((int)(surface->code/100) == 3) {
      w = surface->node[2];
    } else if((int)(surface->code/100) == 4) {
      w = surface->node[3];
    } else {
      cout << "findBoundaryElementNormals: error: unknown code" << endl;
      cout.flush();
      exit(0);
    }

    // Calculate normal (modulo sign):
    a[0] = mesh->node[v].x[0] - mesh->node[u].x[0];
    a[1] = mesh->node[v].x[1] - mesh->node[u].x[1];
    a[2] = mesh->node[v].x[2] - mesh->node[u].x[2];
    
    b[0] = mesh->node[w].x[0] - mesh->node[u].x[0];
    b[1] = mesh->node[w].x[1] - mesh->node[u].x[1];
    b[2] = mesh->node[w].x[2] - mesh->node[u].x[2];
    
    helpers->crossProduct(a,b,c);
    helpers->normalize(c);
    
    surface->normal[0] = -c[0];
    surface->normal[1] = -c[1];
    surface->normal[2] = -c[2];
    
    // Determine sign:
    //----------------

    // a) which parent element has bigger index?

    e0 = surface->element[0];
    e1 = surface->element[1];
    
    if( (e0<0) && (e1<0) ) {
      // both parents unknown
      bigger = -1;
    } else if(e1<0) {
      // e0 known, e1 unknown
      bigger = e0;
    } else {
      // both parents known
      bigger = e0;
      i0 = mesh->element[e0].index;
      i1 = mesh->element[e1].index;
      if(i1 > i0)
	bigger = e1;
    }
    
    // b) normal should point to the parent with smaller index:

    if(bigger > -1) {

      // Compute center point of the surface element:
      center_surface[0] = 0.0;
      center_surface[1] = 0.0;
      center_surface[2] = 0.0;

      for(int i=0; i < surface->nodes; i++) {
	int j = surface->node[i];
	node_t *n = &mesh->node[j];
	center_surface[0] += n->x[0];
	center_surface[1] += n->x[1];
	center_surface[2] += n->x[2];
      }

      center_surface[0] /= (double)(surface->nodes);
      center_surface[1] /= (double)(surface->nodes);
      center_surface[2] /= (double)(surface->nodes);
      
      element_t *e = &mesh->element[bigger];

      // compute center point of the parent element:
      center_element[0] = 0.0;
      center_element[1] = 0.0;
      center_element[2] = 0.0;

      for(int i=0; i < e->nodes; i++) {
	int j = e->node[i];
	node_t *n = &mesh->node[j];
	center_element[0] += n->x[0];
	center_element[1] += n->x[1];
	center_element[2] += n->x[2];
      }

      center_element[0] /= (double)(e->nodes);
      center_element[1] /= (double)(e->nodes);
      center_element[2] /= (double)(e->nodes);

      // difference of the centers:
      center_difference[0] = center_element[0] - center_surface[0];
      center_difference[1] = center_element[1] - center_surface[1];
      center_difference[2] = center_element[2] - center_surface[2];
      
      // dot product must be negative
      double dp = center_difference[0]*c[0]
                + center_difference[1]*c[1] 
                + center_difference[2]*c[2];
      
      if(dp > 0.0) {
	surface->normal[0] = -surface->normal[0];
	surface->normal[1] = -surface->normal[1];
	surface->normal[2] = -surface->normal[2];

	// change orientation of the surface element:
	if(surface->code == 303) {
	  int tmp = surface->node[1];
	  surface->node[1] = surface->node[2];
	  surface->node[2] = tmp;

	} else if(surface->code == 404) {
	  int tmp = surface->node[1];
	  surface->node[1] = surface->node[3];
	  surface->node[3] = tmp;

	} else if(surface->code == 306) {
	  int tmp = surface->node[1];
	  surface->node[1] = surface->node[2];
	  surface->node[2] = tmp;
	  tmp = surface->node[3];
	  surface->node[3] = surface->node[5];
	  surface->node[5] = tmp;

	} else if(surface->code == 408) {
	  int tmp = surface->node[1];
	  surface->node[1] = surface->node[3];
	  surface->node[3] = tmp;
	  tmp = surface->node[4];
	  surface->node[4] = surface->node[7];
	  surface->node[7] = tmp;
	  tmp = surface->node[5];
	  surface->node[5] = surface->node[6];
	  surface->node[6] = tmp;

	} else {
	  cout << "findSurfaceElementNormals: error: unable to change element orientation" << endl;
	  cout.flush();
	  exit(0);
	}
      }
    }
  }

  for( int i=0; i<mesh->surfaces; i++ )
  {
    surface_t *surface = &mesh->surface[i];
    int n = surface->code / 100;
    for(int j=0; j<n; j++ )
    {
       surface->vertex_normals[j][0] = surface->normal[0];
       surface->vertex_normals[j][1] = surface->normal[1];
       surface->vertex_normals[j][2] = surface->normal[2];
    }
  }


  // List surfaces connected to nodes
  class n_s_t {
  public:
    int index;
    n_s_t *next;
  } n_s[mesh->nodes];

  for( int i=0; i<mesh->nodes; i++ )
  {
     n_s[i].index = -1;
     n_s[i].next = NULL;
  }

  for( int i=0; i<mesh->surfaces; i++ ) 
  {
     surface_t *surface = &mesh->surface[i];
     int n=surface->code/100;

     for( int j=0; j<n; j++ )
     {
        n_s_t *p = &n_s[surface->node[j]];
        if ( p->index >= 0 ) {
          n_s_t *q = new n_s_t;
          q->next = p->next;
          p->next = q;
          q->index = i;
        } else p->index = i;
     }
  }

  // avarage normals over surfaces connected to vertices if
  // normals within the limit_angle:
  double limit_angle = cos(50.*3.14159/180.);

  for( int i=0; i<mesh->surfaces; i++ )
  {
    surface_t *surf1 = &mesh->surface[i];
    int n=surf1->code/100;

    for( int j=0; j<n; j++ )
    {
      n_s_t *p = &n_s[surf1->node[j]];
      for( ; p && p->index>=0; p=p->next )
      {
        if ( p->index == i ) continue;

        surface_t *surf2 = &mesh->surface[p->index];
        double s = 0.;

        s += surf1->normal[0]*surf2->normal[0];
        s += surf1->normal[1]*surf2->normal[1];
        s += surf1->normal[2]*surf2->normal[2];
        if ( fabs(s) > limit_angle )
        {
           if ( s > 0 ) {
             surf1->vertex_normals[j][0] += surf2->normal[0];
             surf1->vertex_normals[j][1] += surf2->normal[1];
             surf1->vertex_normals[j][2] += surf2->normal[2];
           } else {
             surf1->vertex_normals[j][0] -= surf2->normal[0];
             surf1->vertex_normals[j][1] -= surf2->normal[1];
             surf1->vertex_normals[j][2] -= surf2->normal[2];
           }
        }
      }
    }
  }

  // delete lists:
  for( int i=0; i<mesh->nodes; i++ )
  {
     n_s_t *p=&n_s[i], *q;
     p = p->next;
     while( p )
     {
        q = p->next;
        delete p;
        p = q;
     }
  }

  // And finally normalize:
  for( int i=0; i<mesh->surfaces; i++ )
  {
    surface_t *surface = &mesh->surface[i];
    int n = surface->code / 100;

    for(int j=0; j<n; j++ )
    {
       double s=0;
       s += surface->vertex_normals[j][0]*surface->vertex_normals[j][0];
       s += surface->vertex_normals[j][1]*surface->vertex_normals[j][1];
       s += surface->vertex_normals[j][2]*surface->vertex_normals[j][2];
       if ( s != 0 ) {
         s = sqrt(s);
         surface->vertex_normals[j][0] /= s;
         surface->vertex_normals[j][1] /= s;
         surface->vertex_normals[j][2] /= s;
       }
    }
  }


  delete helpers;
}
