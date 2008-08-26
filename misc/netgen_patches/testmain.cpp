#ifdef WIN32
#include <windows.h>
#include <iostream.h>
#include <fstream.h>

namespace nglib {
#include "../libsrc/interface/nglib.h"
}

using namespace nglib;

typedef Ng_Meshing_Parameters* (*Ng_Meshing_Parameters_t)();
typedef void (*Ng_Init_t)();
typedef Ng_Mesh* (*Ng_NewMesh_t)();
typedef void (*Ng_AddPoint_t)(Ng_Mesh *mesh, double *x);
typedef void (*Ng_AddSurfaceElement_t)(Ng_Mesh *mesh, Ng_Surface_Element_Type et, int * pi);
typedef Ng_Result (*Ng_GenerateVolumeMesh_t)(Ng_Mesh *mesh, Ng_Meshing_Parameters *mp);
typedef void (*Ng_SaveMesh_t)(Ng_Mesh *mesh, const char *filename);

int main (int argc, char ** argv)
{

  // Load lib
  cout << "Load nglib...";
  HINSTANCE h = LoadLibrary(TEXT("./libng.dll"));
  if(!h) {
    cerr << "Unable to load plugin" << endl;
    return 1;
  }
  cout << "done" << endl;

  cout << "Get proc address for Ng_Meshing_Parameters...";
  Ng_Meshing_Parameters_t pNg_Meshing_Parameters = (Ng_Meshing_Parameters_t) GetProcAddress(h, "CreateObjectOfNg_Meshing_Parameters");
  if(!pNg_Meshing_Parameters) {
    cerr << "Unable to get proc address for Ng_Meshing_Parameters" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_Meshing_Parameters *mp = (pNg_Meshing_Parameters)();
  
  
  cout << "Get proc address for Ng_Init...";
  Ng_Init_t pNg_Init = (Ng_Init_t) GetProcAddress(h, "Ng_Init");
  if(!pNg_Init) {
    cerr << "Unable to get proc address for Ng_Init" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_Init_t Ng_Init = pNg_Init;


  cout << "Get proc address for Ng_NewMesh...";
  Ng_NewMesh_t pNg_NewMesh = (Ng_NewMesh_t) GetProcAddress(h, "Ng_NewMesh");
  if(!pNg_NewMesh) {
    cerr << "Unable to get proc address for Ng_NewMesh" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_NewMesh_t Ng_NewMesh = pNg_NewMesh;


  cout << "Get proc address for Ng_AddPoint...";
  Ng_AddPoint_t pNg_AddPoint = (Ng_AddPoint_t) GetProcAddress(h, "Ng_AddPoint");
  if(!pNg_AddPoint) {
    cerr << "Unable to get proc address for Ng_AddPoint" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_AddPoint_t Ng_AddPoint = pNg_AddPoint;

  cout << "Get proc address for Ng_AddSurfaceElement...";
  Ng_AddSurfaceElement_t pNg_AddSurfaceElement = (Ng_AddSurfaceElement_t) GetProcAddress(h, "Ng_AddSurfaceElement");
  if(!pNg_AddSurfaceElement) {
    cerr << "Unable to get proc address for Ng_AddSurfaceElement" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_AddSurfaceElement_t Ng_AddSurfaceElement = pNg_AddSurfaceElement;

  cout << "Get proc address for Ng_GenerateVolumeMesh...";
  Ng_GenerateVolumeMesh_t pNg_GenerateVolumeMesh = (Ng_GenerateVolumeMesh_t) GetProcAddress(h, "Ng_GenerateVolumeMesh");
  if(!pNg_GenerateVolumeMesh) {
    cerr << "Unable to get proc address for Ng_GenerateVolumeMesh" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_GenerateVolumeMesh_t Ng_GenerateVolumeMesh = pNg_GenerateVolumeMesh;

  cout << "Get proc address for Ng_SaveMesh...";
  Ng_SaveMesh_t pNg_SaveMesh = (Ng_SaveMesh_t) GetProcAddress(h, "Ng_SaveMesh");
  if(!pNg_SaveMesh) {
    cerr << "Unable to get proc address for Ng_SaveMesh" << endl;
    return 1;
  }
  cout << "done" << endl;
  Ng_SaveMesh_t Ng_SaveMesh = pNg_SaveMesh;



  cout << "Netgen Testing" << endl;

  if (argc < 2)
    {
      cerr << "use: ng_vol filename" << endl;
      return 1;
    }

  Ng_Mesh * mesh;
  
  Ng_Init();
  
  // creates mesh structure
  mesh = Ng_NewMesh ();
  
  int i, np, nse, ne;
  double point[3];
  int trig[3], tet[4];
  
  // reads surface mesh from file
  ifstream in(argv[1]);

  in >> np;
  cout << "Reading " << np  << " points..."; cout.flush();
  for (i = 1; i <= np; i++)
    {
      in >> point[0] >> point[1] >> point[2];
      Ng_AddPoint (mesh, point);
    }
  cout << "done" << endl;

  in >> nse;
  cout << "Reading " << nse  << " faces..."; cout.flush();
  for (i = 1; i <= nse; i++)
    {
      in >> trig[0] >> trig[1] >> trig[2];
      Ng_AddSurfaceElement (mesh, NG_TRIG, trig);
    }
  cout << "done" << endl;


  // generate volume mesh
  mp->maxh = 1e6;
  mp->fineness = 1;
  mp->secondorder = 0;

  cout << "start meshing" << endl;
  Ng_GenerateVolumeMesh (mesh, mp);
  cout << "meshing done" << endl;

  Ng_SaveMesh(mesh, "test.vol");

  return 0;
}
#endif
