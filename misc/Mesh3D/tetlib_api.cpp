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
    cout << "tetlib functionality disabled\n";
    cout.flush();
    return false;
  }

  cout << "done\n";
  cout.flush();
  
#ifdef WIN32
  ptetgenio = (tetgenio_t) GetProcAddress(hTetlib, "CreateObjectOfTetgenio");
#else
  ptetgenio = (tetgenio_t) dlsym(hTetlib, "CreateObjectOfTetgenio");  
#endif
  
  if(!ptetgenio) {
    cout << "Unable to get proc address for 'tetgenio'\n";
    cout.flush();
#ifndef WIN32
    dlclose(hTetlib);
#endif
    return false;
  }
  
  in = (ptetgenio)();
  out = (ptetgenio)(); 

#ifdef WIN32
  delegate_tetrahedralize = (delegate_tetrahedralize_t) GetProcAddress(hTetlib, "delegate_tetrahedralize");
#else
  delegate_tetrahedralize = (delegate_tetrahedralize_t) dlsym(hTetlib, "delegate_tetrahedralize");
#endif

  if(!delegate_tetrahedralize) {
    cout << "Unable to get proc address for 'delegate_tetrahedralize'\n";
    cout.flush();
#ifndef WIN32
    dlclose(hTetlib);
#endif
    return false;
  }
  
  return true;
}
