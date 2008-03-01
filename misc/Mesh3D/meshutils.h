#ifndef MESHUTILS_H
#define MESHUTILS_H

#include "meshtype.h"

class Meshutils
{
 public:
  Meshutils();
  ~Meshutils();

  void clearMesh(mesh_t *mesh);
  void findBoundaryElementEdges(mesh_t*);
  void findSharpEdges(mesh_t*, mesh_t*);  
};
#endif // #ifndef MESHUTILS_H
