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
    cout << "nglib functionality unavailable\n";
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
