#include <iostream>
#include "tetlib_api.h"

using namespace std;

TetlibAPI::TetlibAPI()
{
}


TetlibAPI::~TetlibAPI()
{
}


bool TetlibAPI::loadTetlib()
{
  cout << "Load tetlib...";

#ifdef WIN32
  hTetlib = LoadLibrary(TEXT("./libtet.dll"));
#else
  hTetlib = dlopen("./libtet.so", RTLD_LAZY);  
#endif
  
  if(!hTetlib) {
    cout << "failed\n";
    cout << "tetlib functionality unavailable\n";
    cout.flush();
    return false;
  }

  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
#define DLSYMPROC GetProcAddress
#else
#define DLSYMPROC GetProcAddress
#endif

  if(!(ptetgenio = (tetgenio_t) DLSYMPROC(hTetlib, "CreateObjectOfTetgenio")))
    {
      cout << "Unable to get proc address for 'tetgenio'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  in = (ptetgenio)();
  out = (ptetgenio)(); 

  if(!(delegate_tetrahedralize = (delegate_tetrahedralize_t) DLSYMPROC(hTetlib, "delegate_tetrahedralize")))
    {
      cout << "Unable to get proc address for 'delegate_tetrahedralize'\n";
      cout.flush();
#ifndef WIN32
      dlclose(hTetlib);
#endif
      return false;
    }
  
  return true;
}
