#include <iostream>
#include "tetlib_api.h"


TetlibAPI::TetlibAPI()
{
}


TetlibAPI::~TetlibAPI()
{
}


bool TetlibAPI::loadTetlib()
{
  std::cout << "Load tetlib...";

#ifdef WIN32
  hTetlib = LoadLibrary(TEXT("./libtet.dll"));
#else
  hTetlib = dlopen("./libtet.so", RTLD_LAZY);  
#endif
  
  if(!hTetlib) {
    std::cout << "failed\n";
    std::cout << "tetlib functionality disabled\n";
    std::cout.flush();
    return false;
  }

  std::cout << "done\n";
  std::cout.flush();
  
#ifdef WIN32
  ptetgenio = (tetgenio_t) GetProcAddress(hTetlib, "CreateObjectOfTetgenio");
#else
  ptetgenio = (tetgenio_t) dlsym(hTetlib, "CreateObjectOfTetgenio");  
#endif
  
  if(!ptetgenio) {
    std::cout << "Unable to get proc address for 'tetgenio'\n";
    std::cout.flush();
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
    std::cout << "Unable to get proc address for 'delegate_tetrahedralize'\n";
    std::cout.flush();
#ifndef WIN32
    dlclose(hTetlib);
#endif
    return false;
  }
  
  return true;
}
