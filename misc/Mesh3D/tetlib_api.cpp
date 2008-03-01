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
    cout << "failed\n";
    cout << "tetlib functionality unavailable\n";
    cout.flush();
    return false;
  }

  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
#define DLSYMPROC GetProcAddress
#else
#define DLSYMPROC GetProcAddress
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

    boundaryelement->parent[0] = -1; // default
    boundaryelement->parent[1] = -1;
    // must have "nn" in control string:
    if(out->adjtetlist != (int*)NULL) {
      boundaryelement->parent[0] = *adjtetlist++;
      boundaryelement->parent[1] = *adjtetlist++;
    }

    int u = (*trifacelist++) - out->firstnumber;
    int v = (*trifacelist++) - out->firstnumber;
    int w = (*trifacelist++) - out->firstnumber;

    boundaryelement->vertex[0] = u;
    boundaryelement->vertex[1] = v;
    boundaryelement->vertex[2] = w;

    // Normal:
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
  }

  // Elements:
  mesh->elements = out->numberoftetrahedra;
  mesh->element = new element_t[mesh->elements];

  int *tetrahedronlist = out->tetrahedronlist;
  REAL *attribute = out->tetrahedronattributelist;
  int na =  out->numberoftetrahedronattributes;

  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    element->vertex[0] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[1] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[2] = (*tetrahedronlist++) - out->firstnumber;
    element->vertex[3] = (*tetrahedronlist++) - out->firstnumber;

    element->index = 1; // default
    // must have "A" in control string:
    if(out->tetrahedronattributelist != (REAL*)NULL) 
      element->index = (int)attribute[na*(i+1)-1];
  }

  // Edges:
  meshutils.findBoundaryElementEdges(mesh);

  return mesh;
}
