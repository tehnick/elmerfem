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
    cout << "failed\n";
    cout << "nglib functionality disabled\n";
    cout.flush();
    return false;
  }
  
  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
  pNg_Meshing_Parameters = (Ng_Meshing_Parameters_t) GetProcAddress(hNglib, "CreateObjectOfNg_Meshing_Parameters");
#else
  pNg_Meshing_Parameters = (Ng_Meshing_Parameters_t) dlsym(hNglib, "CreateObjectOfNg_Meshing_Parameters");  
#endif
  
  if(!pNg_Meshing_Parameters) {
    cout << "Unable to get proc address for 'Ng_Meshing_Parameters'\n";
    cout.flush();
#ifndef WIN32
    dlclose(hNglib);
#endif
    return false;
  }
  
  mp = (pNg_Meshing_Parameters)();
  
  // TODO: check for errors here:
#ifdef WIN32
  Ng_STL_LoadGeometry = (Ng_STL_LoadGeometry_t) GetProcAddress(hNglib, "Ng_STL_LoadGeometry");
  Ng_STL_InitSTLGeometry = (Ng_STL_InitSTLGeometry_t) GetProcAddress(hNglib, "Ng_STL_InitSTLGeometry");  
  Ng_Init = (Ng_Init_t) GetProcAddress(hNglib, "Ng_Init");
  Ng_GetNP = (Ng_GetNP_t) GetProcAddress(hNglib, "Ng_GetNP");
  Ng_GetNSE = (Ng_GetNSE_t) GetProcAddress(hNglib, "Ng_GetNSE");
  Ng_GetNE = (Ng_GetNE_t) GetProcAddress(hNglib, "Ng_GetNE");
  Ng_GetPoint = (Ng_GetPoint_t) GetProcAddress(hNglib, "Ng_GetPoint");
  Ng_GetSurfaceElement = (Ng_GetSurfaceElement_t) GetProcAddress(hNglib, "Ng_GetSurfaceElement");
  Ng_GetVolumeElement = (Ng_GetVolumeElement_t) GetProcAddress(hNglib, "Ng_GetVolumeElement");
  Ng_NewMesh = (Ng_NewMesh_t) GetProcAddress(hNglib, "Ng_NewMesh");
  Ng_STL_MakeEdges = (Ng_STL_MakeEdges_t) GetProcAddress(hNglib, "Ng_STL_MakeEdges");
  Ng_STL_GenerateSurfaceMesh = (Ng_STL_GenerateSurfaceMesh_t) GetProcAddress(hNglib, "Ng_STL_GenerateSurfaceMesh");
  Ng_GenerateVolumeMesh = (Ng_GenerateVolumeMesh_t) GetProcAddress(hNglib, "Ng_GenerateVolumeMesh");
#else
  Ng_STL_LoadGeometry = (Ng_STL_LoadGeometry_t) dlsym(hNglib, "Ng_STL_LoadGeometry");
  Ng_STL_InitSTLGeometry = (Ng_STL_InitSTLGeometry_t) dlsym(hNglib, "Ng_STL_InitSTLGeometry");  
  Ng_Init = (Ng_Init_t) dlsym(hNglib, "Ng_Init");
  Ng_GetNP = (Ng_GetNP_t) dlsym(hNglib, "Ng_GetNP");
  Ng_GetNSE = (Ng_GetNSE_t) dlsym(hNglib, "Ng_GetNSE");
  Ng_GetNE = (Ng_GetNE_t) dlsym(hNglib, "Ng_GetNE");
  Ng_GetPoint = (Ng_GetPoint_t) dlsym(hNglib, "Ng_GetPoint");
  Ng_GetSurfaceElement = (Ng_GetSurfaceElement_t) dlsym(hNglib, "Ng_GetSurfaceElement");
  Ng_GetVolumeElement = (Ng_GetVolumeElement_t) dlsym(hNglib, "Ng_GetVolumeElement");
  Ng_NewMesh = (Ng_NewMesh_t) dlsym(hNglib, "Ng_NewMesh");
  Ng_STL_MakeEdges = (Ng_STL_MakeEdges_t) dlsym(hNglib, "Ng_STL_MakeEdges");
  Ng_STL_GenerateSurfaceMesh = (Ng_STL_GenerateSurfaceMesh_t) dlsym(hNglib, "Ng_STL_GenerateSurfaceMesh");
  Ng_GenerateVolumeMesh = (Ng_GenerateVolumeMesh_t) dlsym(hNglib, "Ng_GenerateVolumeMesh");
#endif

  return true;
}
