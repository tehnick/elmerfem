#include <iostream>
#include "tetgen.h"
using namespace std;
//
// C-delegator for overloaded 'tetrahedralize':
//
void delegate_tetrahedralize(int bs, tetgenbehavior *b, char *switches, 
			     tetgenio *in, tetgenio *out, tetgenio *addin,
			     tetgenio *bgmin)
{
  if(bs==0)
    tetrahedralize(b, in, out, addin, bgmin);
      
  if(bs==1)
    tetrahedralize(switches, in, out, addin, bgmin);

  return;
}
//
// Create object of class 'tetgenio'
//
extern "C" 
#ifdef WIN32
__declspec(dllexport)
#endif
tetgenio* CreateObjectOfTetgenio()
{
  return new tetgenio();
}
