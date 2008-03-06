#ifndef NGLIB_API_H
#define NGLIB_API_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001
#define GEN_ELMERGRID 1002

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

namespace nglib {
#include "nglib.h"
}

#include "helpers.h"
#include "meshutils.h"

typedef nglib::Ng_Meshing_Parameters* (*Ng_Meshing_Parameters_t)();
typedef nglib::Ng_STL_Geometry* (*Ng_STL_LoadGeometry_t)(const char*, int);
typedef nglib::Ng_Result (*Ng_STL_InitSTLGeometry_t)(nglib::Ng_STL_Geometry*);
typedef void (*Ng_Init_t)();
typedef int (*Ng_GetNP_t)(nglib::Ng_Mesh*);
typedef int (*Ng_GetNSE_t)(nglib::Ng_Mesh*);
typedef int (*Ng_GetNE_t)(nglib::Ng_Mesh*);
typedef void (*Ng_GetPoint_t)(nglib::Ng_Mesh*, int, double*);
typedef nglib::Ng_Surface_Element_Type (*Ng_GetSurfaceElement_t)(nglib::Ng_Mesh*, int, int*);
typedef nglib::Ng_Volume_Element_Type (*Ng_GetVolumeElement_t)(nglib::Ng_Mesh*, int, int*);
typedef nglib::Ng_Mesh* (*Ng_NewMesh_t)();
typedef nglib::Ng_Result (*Ng_STL_MakeEdges_t)(nglib::Ng_STL_Geometry*, nglib::Ng_Mesh*, nglib::Ng_Meshing_Parameters*);
typedef nglib::Ng_Result (*Ng_STL_GenerateSurfaceMesh_t)(nglib::Ng_STL_Geometry*, nglib::Ng_Mesh*, nglib::Ng_Meshing_Parameters*);
typedef nglib::Ng_Result (*Ng_GenerateVolumeMesh_t)(nglib::Ng_Mesh*, nglib::Ng_Meshing_Parameters*);

class NglibAPI
{
 public:
  NglibAPI();
  ~NglibAPI();
  
  bool loadNglib();
  
#ifdef WIN32
  HINSTANCE hNglib;
#else
  void *hNglib;
#endif

  Ng_Meshing_Parameters_t pNg_Meshing_Parameters;
  Ng_STL_LoadGeometry_t Ng_STL_LoadGeometry;
  Ng_STL_InitSTLGeometry_t Ng_STL_InitSTLGeometry;
  Ng_Init_t Ng_Init;
  Ng_GetNP_t Ng_GetNP;
  Ng_GetNSE_t Ng_GetNSE;
  Ng_GetNE_t Ng_GetNE;
  Ng_GetPoint_t Ng_GetPoint;
  Ng_GetSurfaceElement_t Ng_GetSurfaceElement;
  Ng_GetVolumeElement_t Ng_GetVolumeElement;
  Ng_NewMesh_t Ng_NewMesh;
  Ng_STL_MakeEdges_t Ng_STL_MakeEdges;
  Ng_STL_GenerateSurfaceMesh_t Ng_STL_GenerateSurfaceMesh;
  Ng_GenerateVolumeMesh_t Ng_GenerateVolumeMesh;

  nglib::Ng_Mesh *ngmesh;
  nglib::Ng_STL_Geometry *nggeom;
  nglib::Ng_Meshing_Parameters *mp;

  mesh_t* createElmerMeshStructure();
};

#endif // #ifndef NGLIB_API_H
