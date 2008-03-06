#ifndef ELMERGRID_API_H
#define ELMERGRID_API_H

#define GEN_TETLIB    1000
#define GEN_NGLIB     1001
#define GEN_ELMERGRID 1002

#include "meshtype.h"

class ElmergridAPI
{
 public:
  ElmergridAPI();
  ~ElmergridAPI();
  
  mesh_t* createElmerMeshStructure();

};

#endif // #ifndef ELMERGRID_API_H
