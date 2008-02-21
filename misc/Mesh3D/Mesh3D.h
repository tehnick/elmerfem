#ifndef MESH3D_H
#define MESH3D_H

#include "meshtype.h"

class Mesh3D
{
 public:
  Mesh3D();
  ~Mesh3D();

  void invertMatrix(const double *a, double *b);

  void crossProduct(double *a, double *b, double *c);
  double vlen(double *a);
  void normalize(double *a);

 private:

};

#endif
