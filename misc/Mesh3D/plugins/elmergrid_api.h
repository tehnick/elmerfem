#ifndef ELMERGRID_API_H
#define ELMERGRID_API_H

#define GEN_TETLIB    1000
#define GEN_NGLIB     1001
#define GEN_ELMERGRID 1002

#include "meshtype.h"
#include "meshutils.h"

class ElmergridAPI
{
 public:
  ElmergridAPI();
  ~ElmergridAPI();
  
  void createElmerMeshStructure(mesh_t*);

};

#endif // #ifndef ELMERGRID_API_H
