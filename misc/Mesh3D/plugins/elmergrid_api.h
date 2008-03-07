#ifndef ELMERGRID_API_H
#define ELMERGRID_API_H

#include "meshtype.h"

class ElmergridAPI
{
 public:
  ElmergridAPI();
  ~ElmergridAPI();
  
  void createElmerMeshStructure(mesh_t*);

};

#endif // #ifndef ELMERGRID_API_H
 
