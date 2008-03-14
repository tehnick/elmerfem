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
  if(mesh == NULL)
    return;
    
  if(mesh->elements > 0) {
    for(int i=0; i < mesh->elements; i++)
      if(mesh->element[i].node)
	delete [] mesh->element[i].node;
    delete [] mesh->element;
  }

  if(mesh->surfaces > 0) {
    for(int i=0; i < mesh->surfaces; i++) {
      if(mesh->surface[i].node)
	delete [] mesh->surface[i].node;
      if(mesh->surface[i].edge)
	delete [] mesh->surface[i].edge;
      if(mesh->surface[i].element)
	delete [] mesh->surface[i].element;
    }
    delete [] mesh->surface;
  }

  if(mesh->edges > 0) {
    for(int i=0; i < mesh->edges; i++) {
      if(mesh->edge[i].node)
	delete [] mesh->edge[i].node;
      if(mesh->edge[i].surface)
	delete [] mesh->edge[i].surface;
    }
    delete [] mesh->edge;
  }

  if(mesh->points > 0) {
    for(int i=0; i < mesh->points; i++) {
      if(mesh->point[i].node)
	delete [] mesh->point[i].node;
      if(mesh->point[i].edge)
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


// Find parents for boundary elements...
//----------------------------------------------------------------------------
void Meshutils::findBoundaryElementParents(mesh_t *mesh)
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


// Find edges for boundary elements...
//-----------------------------------------------------------------------------
void Meshutils::findBoundaryElementEdges(mesh_t *mesh)
{
#define UNKNOWN -1

#define RESETENTRY              \
    h->node = UNKNOWN;          \
    h->surfaces = 0;            \
    h->surface = NULL;          \
    h->next = NULL;

  int keys = mesh->nodes;

  class hashEntry {
  public:
    int node;
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
        if(h->node == n) {
          found = true;
          break;
        }
        h = h->next;
      }                                                      
      
      if(!found) {
        h->node = n;
        h->surfaces = 1;
        h->surface = new int[1];
        h->surface[0] = i;
        h->index = edge->index;
        h->nature = edge->nature;
        h->next = new hashEntry;
        h = h->next;
        RESETENTRY;
      } else {
        int *tmp  = new int[h->surfaces];
        for(int j=0; j<h->surfaces; j++)
          tmp[j] = h->surface[j];
        delete [] h->surface;
        h->surface = new int[h->surfaces+1];
        for(int j=0; j<h->surfaces; j++)
           h->surface[j] = tmp[j];
        h->surface[h->surfaces++] = i;
        delete [] tmp;
      }
    }

    mesh->edges = 0;
    delete [] mesh->edge;
  }


  static int triedgemap[][2] = {{0,1}, {1,2}, {2,0}};
  static int quadedgemap[][2] = {{0,1}, {1,2}, {2,3}, {3,0}};
  
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *s = &mesh->surface[i];
    
    // loop over edges
    for(int e=0; e < s->edges; e++) {

      int n0, n1;
      if((int)(s->code/100) == 3) {
	n0 = s->node[triedgemap[e][0]];
	n1 = s->node[triedgemap[e][1]];
      } else if((int)(s->code/100) == 4) {
	n0 = s->node[quadedgemap[e][0]];
	n1 = s->node[quadedgemap[e][1]];
      } else {
	cout << "findBoundaryElementEdges: error: unknown element code" << endl;
	exit(0);
      }

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
        for(int j=0; j<h->surfaces; j++)
          tmp[j] = h->surface[j];
        delete [] h->surface;
        h->surface = new int[h->surfaces+1];
        for(int j=0; j<h->surfaces; j++)
           h->surface[j] = tmp[j];
        h->surface[h->surfaces++] = i;
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
      e->code = 202;
      e->nodes = 2;
      e->node = new int[2];

      e->node[0] = i;
      e->node[1] = h->node;

      e->surfaces = h->surfaces;
      e->surface = new int[e->surfaces];

      for(int j=0; j < e->surfaces; j++) {
	e->surface[j] = h->surface[j];
      }

      e->index = h->index;
      h = h->next;
    }
  }

  delete [] hash;

  // Inverse map
  for(int i=0; i < mesh->edges; i++) {
    edge_t *e = &mesh->edge[i];

    for(int j=0; j < e->surfaces; j++) {
      int k = e->surface[j];
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


// Divide boundary by sharp edges...
//-----------------------------------------------------------------------------
int Meshutils::divideBoundaryBySharpEdges(mesh_t *mesh)
{
#define UNKNOWN -1
#define SHARP 0

  class Bc {
  public:
    void propagateIndex(mesh_t* mesh, int index, int i) {
      surface_t *surface = &mesh->surface[i];

      // index is ok
      if(surface->index != UNKNOWN)
	return;

      // set index
      surface->index = index;

      // propagate index
      for(int j=0; j<surface->edges; j++) {
	int k = surface->edge[j];
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
  
  Bc *bc = new Bc;
  
  // reset bc-indices:
  for(int i=0; i < mesh->surfaces; i++)
    mesh->surface[i].index = UNKNOWN;

  // recursively determine boundary parts:
  int index = 0;
  for(int i=0; i < mesh->surfaces; i++) {
    surface_t *surface = &mesh->surface[i];
    if(surface->index == UNKNOWN) 
      bc->propagateIndex(mesh, ++index, i);
  }

  cout << "Surface divided into " << index << " parts" << endl;

  delete bc;

  return index;
}

// Find boundary element normals...
//-----------------------------------------------------------------------------
void Meshutils::findBoundaryElementNormals(mesh_t *mesh)
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

	} else {
	  cout << "findSurfaceElementNormals: error: unable to change element orientation" << endl;
	  cout.flush();
	  exit(0);
	}
      }
    }
  }

  delete helpers;
}
