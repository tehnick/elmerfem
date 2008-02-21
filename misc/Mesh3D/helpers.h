#ifndef HELPERS_H
#define HELPERS_H

#include "meshtype.h"

class Helpers
{
 public:
  Helpers();
  ~Helpers();

  void invertMatrix(const double *a, double *b);

  void crossProduct(double *a, double *b, double *c);
  double vlen(double *a);
  void normalize(double *a);

 private:

};

#endif
