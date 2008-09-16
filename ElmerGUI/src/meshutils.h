#ifndef MESHUTILS_H
#define MESHUTILS_H

#include <math.h>
#include "meshtype.h"
#include "helpers.h"

#define SHARPEDGE  0
#define UNKNOWN   -1
#define MYPI 3.14159

class Meshutils
{
 public:
  Meshutils();
  ~Meshutils();

  double* boundingBox(mesh_t*);
  void clearMesh(mesh_t*);
  void findEdgeElementPoints(mesh_t*);
  void findSurfaceElements(mesh_t*);
  void findSurfaceElementEdges(mesh_t*);
  void findSurfaceElementParents(mesh_t*);
  void findSurfaceElementNormals(mesh_t*);
  void findSharpEdges(mesh_t*, double);
  void findSharpPoints(mesh_t*, double);
  int divideEdgeBySharpPoints(mesh_t*);
  int divideSurfaceBySharpEdges(mesh_t*);
  void sort_index(int n, double *a, int *b);
  void increaseElementOrder(mesh_t*);
  void decreaseElementOrder(mesh_t*);
  int cleanHangingSharpEdges(mesh_t*);
};
#endif // #ifndef MESHUTILS_H
