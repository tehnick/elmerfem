#include <iostream>
#include "nglib_api.h"

using namespace std;

NglibAPI::NglibAPI()
{
}


NglibAPI::~NglibAPI()
{
}


bool NglibAPI::loadNglib()
{
  cout << "Load nglib...";

#ifdef WIN32
  hNglib = LoadLibrary(TEXT("./libng.dll"));
#else
  hNglib = dlopen("./libng.so", RTLD_LAZY);  
#endif
  
  if(!hNglib) {
    cout << "unable to load library\n";
    cout << "nglib functionality unavailable\n";
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

  if(!(pNg_Meshing_Parameters = (Ng_Meshing_Parameters_t) DLSYMPROC(hNglib, "CreateObjectOfNg_Meshing_Parameters")))
    {
      cout << "Unable to get proc address for 'Ng_Meshing_Parameters'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hNglib);
#endif
      return false;
    }
  
  mp = (pNg_Meshing_Parameters)();
  
  if(!(Ng_STL_LoadGeometry = (Ng_STL_LoadGeometry_t) DLSYMPROC(hNglib, "Ng_STL_LoadGeometry")) ||
     !(Ng_STL_InitSTLGeometry = (Ng_STL_InitSTLGeometry_t) DLSYMPROC(hNglib, "Ng_STL_InitSTLGeometry")) ||
     !(Ng_Init = (Ng_Init_t) DLSYMPROC(hNglib, "Ng_Init")) ||
     !(Ng_GetNP = (Ng_GetNP_t) DLSYMPROC(hNglib, "Ng_GetNP")) ||
     !(Ng_GetNSE = (Ng_GetNSE_t) DLSYMPROC(hNglib, "Ng_GetNSE")) ||
     !(Ng_GetNE = (Ng_GetNE_t) DLSYMPROC(hNglib, "Ng_GetNE")) ||
     !(Ng_GetPoint = (Ng_GetPoint_t) DLSYMPROC(hNglib, "Ng_GetPoint")) ||
     !(Ng_GetSurfaceElement = (Ng_GetSurfaceElement_t) DLSYMPROC(hNglib, "Ng_GetSurfaceElement")) ||
     !(Ng_GetVolumeElement = (Ng_GetVolumeElement_t) DLSYMPROC(hNglib, "Ng_GetVolumeElement")) ||
     !(Ng_NewMesh = (Ng_NewMesh_t) DLSYMPROC(hNglib, "Ng_NewMesh")) ||
     !(Ng_STL_MakeEdges = (Ng_STL_MakeEdges_t) DLSYMPROC(hNglib, "Ng_STL_MakeEdges")) ||
     !(Ng_STL_GenerateSurfaceMesh = (Ng_STL_GenerateSurfaceMesh_t) DLSYMPROC(hNglib, "Ng_STL_GenerateSurfaceMesh")) ||
     !(Ng_GenerateVolumeMesh = (Ng_GenerateVolumeMesh_t) DLSYMPROC(hNglib, "Ng_GenerateVolumeMesh")))
    {
      cout << "Unable to get (one of the) proc addresses\n";
      cout.flush();
#ifndef WIN32
      dlclose(hNglib);
#endif
      return false;      
    }
  
  return true;
}

// Populate elmer's mesh structure:
//-----------------------------------------------------------------------------
mesh_t* NglibAPI::createElmerMeshStructure()
{
  Helpers helpers;
  Meshutils meshutils;
  
  // Create new mesh structure:
  mesh_t *mesh = new mesh_t;

  // Nodes:
  mesh->nodes = Ng_GetNP(ngmesh);
  mesh->node = new node_t[mesh->nodes];

  for(int i=0; i < mesh->nodes; i++) {
    node_t *node = &mesh->node[i];

    Ng_GetPoint(ngmesh, i+1, node->x);

    node->index = -1; // default
  }

  // Boundary elements:				       
  mesh->boundaryelements = Ng_GetNSE(ngmesh);
  mesh->boundaryelement = new boundaryelement_t[mesh->boundaryelements];

  for(int i=0; i < mesh->boundaryelements; i++) {
    boundaryelement_t *boundaryelement = &mesh->boundaryelement[i];

    boundaryelement->code = 303;

    boundaryelement->nodes = 3;
    boundaryelement->node = new int[3];
    
    boundaryelement->edges = 3;
    boundaryelement->edge = new int[3];

    boundaryelement->elements = 2;
    boundaryelement->element = new int[2];

    boundaryelement->index = 1; // default

    boundaryelement->edge[0] = -1;
    boundaryelement->edge[1] = -1;
    boundaryelement->edge[2] = -1;

    boundaryelement->element[0] = -1; 
    boundaryelement->element[1] = -1;

    Ng_GetSurfaceElement(ngmesh, i+1, boundaryelement->node);
    
    boundaryelement->node[0]--;
    boundaryelement->node[1]--;
    boundaryelement->node[2]--;

    // swap orientation:
    int tmp = boundaryelement->node[1];
    boundaryelement->node[1] = boundaryelement->node[2];
    boundaryelement->node[2] = tmp;
  }

  // Elements:
  mesh->elements = Ng_GetNE(ngmesh);
  mesh->element = new element_t[mesh->elements];

  for(int i=0; i< mesh->elements; i++) {
    element_t *element = &mesh->element[i];

    Ng_GetVolumeElement(ngmesh, i+1, element->node);
    
    element->code = 504;

    element->nodes = 4;
    element->node = new int[4];

    element->node[0]--;
    element->node[1]--;
    element->node[2]--;
    element->node[3]--;

    element->index = 1; // default
  }

  // Find parents for boundary elements:
  meshutils.findBoundaryElementParents(mesh);

  // Find edges for boundary elements:
  meshutils.findBoundaryElementEdges(mesh);

  // Compute normals for boundary elements:
  meshutils.findBoundaryElementNormals(mesh);

  return mesh;
}
