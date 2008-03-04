#include <iostream>
#include "tetlib_api.h"

using namespace std;

TetlibAPI::TetlibAPI()
{
}


TetlibAPI::~TetlibAPI()
{
}


bool TetlibAPI::loadTetlib()
{
  cout << "Load tetlib...";

#ifdef WIN32
  hTetlib = LoadLibrary(TEXT("./libtet.dll"));
#else
  hTetlib = dlopen("./libtet.so", RTLD_LAZY);  
#endif
  
  if(!hTetlib) {
    cout << "unable to load library\n";
    cout << "tetlib functionality unavailable\n";
    cout.flush();
    return false;
  }

  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
#define DLSYMPROC GetProcAddress
#else
#define DLSYMPROC dlsym
#endif

  if(!(ptetgenio = (tetgenio_t) DLSYMPROC(hTetlib, "CreateObjectOfTetgenio")))
    {
      cout << "Unable to get proc address for 'tetgenio'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  in = (ptetgenio)();
  out = (ptetgenio)(); 

  if(!(delegate_tetrahedralize = (delegate_tetrahedralize_t) DLSYMPROC(hTetlib, "delegate_tetrahedralize")))
    {
      cout << "Unable to get proc address for 'delegate_tetrahedralize'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  return true;
}



// Populate elmer's mesh structure:
//-----------------------------------------------------------------------------
mesh_t* TetlibAPI::createElmerMeshStructure()
{
  Helpers helpers;
  Meshutils meshutils;

  // Create new mesh structure:
  mesh_t *mesh = new mesh_t;
  
  // Nodes:
  mesh->nodes = out->numberofpoints;
  mesh->node = new node_t[mesh->nodes];

  REAL *pointlist = out->pointlist;

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];
    
    node->x[0] = *pointlist++;
    node->x[1] = *pointlist++;
    node->x[2] = *pointlist++;

    node->index = -1; // default
  }

  // Elements:
  mesh->elements = out->numberoftetrahedra;
  mesh->element = new element_t[mesh->elements];

  int *tetrahedronlist = out->tetrahedronlist;
  REAL *attribute = out->tetrahedronattributelist;
  int na =  out->numberoftetrahedronattributes;
  
  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];
    
    element->node[0] = (*tetrahedronlist++) - out->firstnumber;
    element->node[1] = (*tetrahedronlist++) - out->firstnumber;
    element->node[2] = (*tetrahedronlist++) - out->firstnumber;
    element->node[3] = (*tetrahedronlist++) - out->firstnumber;
    
    element->index = 1; // default
    // must have "A" in control string:
    if(out->tetrahedronattributelist != (REAL*)NULL) 
      element->index = (int)attribute[na*(i+1)-1];
  }
  
  // Boundary elements:
  mesh->boundaryelements = out->numberoftrifaces;
  mesh->boundaryelement = new boundaryelement_t[mesh->boundaryelements];

  int *trifacelist = out->trifacelist;
  int *adjtetlist = out->adjtetlist;

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    boundaryelement->index = 1; // default
    if(out->trifacemarkerlist != (int*)NULL)
      boundaryelement->index = out->trifacemarkerlist[i];

    boundaryelement->edge[0] = -1;
    boundaryelement->edge[1] = -1;
    boundaryelement->edge[2] = -1;
    
    boundaryelement->element[0] = -1; // default
    boundaryelement->element[1] = -1;
    // must have "nn" in control string:
    if(out->adjtetlist != (int*)NULL) {
      boundaryelement->element[0] = (*adjtetlist++) - out->firstnumber;
      boundaryelement->element[1] = (*adjtetlist++) - out->firstnumber;
    }

    int u = (*trifacelist++) - out->firstnumber;
    int v = (*trifacelist++) - out->firstnumber;
    int w = (*trifacelist++) - out->firstnumber;

    boundaryelement->node[0] = u;
    boundaryelement->node[1] = v;
    boundaryelement->node[2] = w;

    // Normal (modulo sign):
    static double a[3], b[3], c[3];

    a[0] = mesh->node[v].x[0] - mesh->node[u].x[0];
    a[1] = mesh->node[v].x[1] - mesh->node[u].x[1];
    a[2] = mesh->node[v].x[2] - mesh->node[u].x[2];

    b[0] = mesh->node[w].x[0] - mesh->node[u].x[0];
    b[1] = mesh->node[w].x[1] - mesh->node[u].x[1];
    b[2] = mesh->node[w].x[2] - mesh->node[u].x[2];

    helpers.crossProduct(a,b,c);
    helpers.normalize(c);
    
    boundaryelement->normal[0] = c[0];
    boundaryelement->normal[1] = c[1];
    boundaryelement->normal[2] = c[2];

    // Determine sign of the normal:

    // a) which parent element has bigger index?
    int e0 = boundaryelement->element[0];
    int e1 = boundaryelement->element[1];

    int bigger;
    if( (e0<0) && (e1<0) ) {
      // both parents unknown
      bigger = -1;
    } else if(e1<0) {
      // e0 known, e1 unknown
      bigger = e0;
    } else {
      // both parents known
      bigger = e0;
      int i0 = mesh->element[e0].index;
      int i1 = mesh->element[e1].index;
      if(i1 > i0)
	bigger = e1;
    }

    // b) normal should points from bigger to smaller element index:
    if(bigger > -1) {
      int q;
      double center_boundaryelement[3], center_element[3], center_difference[3];
      
      center_boundaryelement[0] = mesh->node[u].x[0] + mesh->node[v].x[0] + mesh->node[w].x[0];
      center_boundaryelement[1] = mesh->node[u].x[1] + mesh->node[v].x[1] + mesh->node[w].x[1];
      center_boundaryelement[2] = mesh->node[u].x[2] + mesh->node[v].x[2] + mesh->node[w].x[2];
      
      center_boundaryelement[0] /= 3.0;
      center_boundaryelement[1] /= 3.0;
      center_boundaryelement[2] /= 3.0;

      element_t *e = &mesh->element[bigger];
      
      u = e->node[0];
      v = e->node[1];
      w = e->node[2];
      q = e->node[3];
      
      center_element[0] = mesh->node[u].x[0] + mesh->node[v].x[0] + mesh->node[w].x[0] + mesh->node[q].x[0];
      center_element[1] = mesh->node[u].x[1] + mesh->node[v].x[1] + mesh->node[w].x[1] + mesh->node[q].x[1];
      center_element[2] = mesh->node[u].x[2] + mesh->node[v].x[2] + mesh->node[w].x[2] + mesh->node[q].x[2];

      center_element[0] /= 4.0;
      center_element[1] /= 4.0;
      center_element[2] /= 4.0;

      center_difference[0] = center_element[0] - center_boundaryelement[0];
      center_difference[1] = center_element[1] - center_boundaryelement[1];
      center_difference[2] = center_element[2] - center_boundaryelement[2];

      // dot product must be negative
      double dp = center_difference[0]*c[0] + center_difference[1]*c[1] + center_difference[2]*c[2];

      if(dp>0.0) {
	boundaryelement->normal[0] = -c[0];
	boundaryelement->normal[1] = -c[1];
	boundaryelement->normal[2] = -c[2];
      }
    }
  }

  // Edges:
  meshutils.findBoundaryElementEdges(mesh);

  return mesh;
}
