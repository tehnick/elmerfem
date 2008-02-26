// Linux: g++ testmain.cpp -o testmain -ldl
// MinGW: g++ testmain.cpp -o testmain.exe

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif
#include <iostream>
#include "tetgen.h"

typedef tetgenio* (*tetgenio_t)();
typedef void (*delegate_t)(int, char *tetgenbehavior, char*, tetgenio*,
			   tetgenio*, tetgenio*, tetgenio*);

using namespace std;

int main(int argc, char **argv) {
  
#ifdef WIN32
  HINSTANCE h = LoadLibrary(TEXT("./libtet.dll"));
#else
  void *h = dlopen("./libtet.so", RTLD_LAZY);
#endif
  
  if(!h) {
    cout << "Unable to load library\n";
    return 1;
  }
  
#ifdef WIN32
  tetgenio_t ptetgenio = (tetgenio_t)GetProcAddress(h, "CreateObjectOfTetgenio");
#else
  tetgenio_t ptetgenio = (tetgenio_t) dlsym(h, "CreateObjectOfTetgenio");
#endif
  
  if(!ptetgenio) {
    cerr << "Unable to load symbol\n";
#ifndef WIN32
    dlclose(h);
#endif
    return 1;
  }
  
#ifdef WIN32
  delegate_t pdelegate= (delegate_t)GetProcAddress(h, "delegate_tetrahedralize");
#else
  delegate_t pdelegate= (delegate_t) dlsym(h, "delegate_tetrahedralize");
#endif
  
  if(!pdelegate) {
    cerr << "Unable to load symbol\n";
#ifndef WIN32
    dlclose(h);
#endif
    return 1;
  }
  
  
  tetgenio *in = (ptetgenio)();
  tetgenio *out = (ptetgenio)();
  delegate_t delegate_tetrahedralize = pdelegate;
  
  in->initialize();
  in->load_poly("example");
  delegate_tetrahedralize(1, NULL, "JApq1.414", in, out, NULL, NULL);
  
  return 0;
}
