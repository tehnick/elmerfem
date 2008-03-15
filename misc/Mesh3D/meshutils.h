#ifndef MESHUTILS_H
#define MESHUTILS_H

#include <math.h>
#include "meshtype.h"
#include "helpers.h"

class Meshutils
{
 public:
  Meshutils();
  ~Meshutils();

  void clearMesh(mesh_t*);
  void findSurfaceElementEdges(mesh_t*);
  void findSurfaceElementParents(mesh_t*);
  void findSurfaceElementNormals(mesh_t*);
  void findSharpEdges(mesh_t*, double);
  int divideSurfaceBySharpEdges(mesh_t*);
  double* boundingBox(mesh_t*);
};
#endif // #ifndef MESHUTILS_H
