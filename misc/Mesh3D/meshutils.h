#ifndef MESHUTILS_H
#define MESHUTILS_H

#include <math.h>
#include "meshtype.h"

class Meshutils
{
 public:
  Meshutils();
  ~Meshutils();

  void clearMesh(mesh_t *mesh);
  void findBoundaryElementEdges(mesh_t*);
  mesh_t* findSharpEdges(mesh_t*);
  double* boundingBox(mesh_t*);
};
#endif // #ifndef MESHUTILS_H
