/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , Center for Scientific Computing,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: Center for Scientific Computing
*                         Tietotie 6, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front
Module:     ecif_process.cpp
Language:   C++
Date:       20.01.99
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Implementation.

************************************************************************/

#if defined(WIN32)
  // Nothing special
#else
  #include <unistd.h>
#endif

#include "ecif_process.h"
#include "ecif_func.h"

Process::Process()
{
  init();
}


Process::Process(char* process_cmd, char* process_args)
{
  init();
  update_dyna_string(command, process_cmd);
  update_dyna_string(arguments, process_args);
}


Process::Process(char* process_cmd, char* process_args,
                 int process_nbr, char* process_name,
                 enum priorityLevel process_priority,
                 bool show_console, char* logfile_name)
{
  init();
  update_dyna_string(command, process_cmd);
  update_dyna_string(arguments, process_args);
  id = process_nbr;
  update_dyna_string(name, process_name);
  priority = process_priority;
  showConsole = show_console;
  update_dyna_string(logfileName, logfile_name);
}


Process::~Process()
{
  delete[] command;
  delete[] arguments;
  delete[] name;
  delete[] logfileName;
}

void
Process::init()
{
  command = NULL;
  arguments = NULL;
  name = NULL;
  logfileName = NULL;

  priority = ECIF_NO_PRIORITY;
  id = 0;
  processId = 0;
  processHandle = 0;
  showConsole = false;
  logfileHandle = 0;

  threadHandle = NULL;
  threadId = NULL;

  started = 0;
  stopped = 0;
}


void
Process::setLogfile(char* logfile)
{
  if (logfile != NULL ) {
    update_dyna_string(logfileName, logfile);
  }
}


// Platform specific parts

#if defined(WIN32)
  #include "ecif_process_WIN32.hpp"

#else
  #include "ecif_process_UNIX.hpp"
#endif

