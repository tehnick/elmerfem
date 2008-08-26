#ifdef WIN32
#include <windows.h>
#endif
#include <iostream.h>
#include <fstream.h>

namespace nglib {
#include "../libsrc/interface/nglib.h"
}

using namespace nglib;

extern "C"
Ng_Meshing_Parameters* CreateObjectOfNg_Meshing_Parameters()
{
  return new Ng_Meshing_Parameters();
}
