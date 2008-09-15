#ifndef CHECKMPI_H
#define CHECKMPI_H

#ifdef WIN32
#include <windows.h>
#include <psapi.h>
#endif

class CheckMpi
{
public:
  CheckMpi();
  ~CheckMpi();

  int findSmpd();

private:
 
};

#endif // CHECKMPI_H
