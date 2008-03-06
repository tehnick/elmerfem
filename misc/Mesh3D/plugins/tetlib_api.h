#ifndef TETLIB_API_H
#define TETLIB_API_H

#define GEN_TETLIB 1000
#define GEN_NGLIB  1001
#define GEN_ELMERGRID 1002

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "tetgen.h"

#include "helpers.h"
#include "meshutils.h"

typedef void (*delegate_tetrahedralize_t)(int, char*, char*, tetgenio*, tetgenio*, tetgenio*, tetgenio*);

class TetlibAPI
{
 public:
  TetlibAPI();
  ~TetlibAPI();
  
  bool loadTetlib();

#ifdef WIN32
  HINSTANCE hTetlib;
#else
  void *hTetlib;
#endif
  
  typedef tetgenio* (*tetgenio_t)();
  tetgenio_t ptetgenio;
  tetgenio *in;
  tetgenio *out;

  delegate_tetrahedralize_t delegate_tetrahedralize;

  mesh_t* createElmerMeshStructure();

};

#endif // #ifndef TETLIB_API_H
