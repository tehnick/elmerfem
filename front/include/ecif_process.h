/***********************************************************************
*
*       ELMER, A Computational Fluid Dynamics Program.
*
*       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
*                                    Finland.
*
*       All rights reserved. No part of this program may be used,
*       reproduced or transmitted in any form or by any means
*       without the written permission of CSC.
*
*                Address: CSC - IT Center for Science Ltd.
*                         Keilaranta 14, P.O. BOX 405
*                         02101 Espoo, Finland
*                         Tel.     +358 0 457 2001
*                         Telefax: +358 0 457 2302
*                         EMail:   Jari.Jarvinen@csc.fi
************************************************************************/

/***********************************************************************
Program:    ELMER Front
Module:     ecif_process.h
Language:   C++
Date:       20.01.99
Version:    1.00
Author(s):  Martti Verho
Revisions:

Abstract:   Process class for handling external processes.

************************************************************************/

#ifndef _PROCESS_
#define _PROCESS_


#include "ecif_def.h"


class Process
{
public:
  Process();
  Process(char* process_cmd, char* process_args);
  Process(char* process_cmd, char* process_args,
          int process_nbr, char* process_name,
          enum priorityLevel process_priority,
          bool show_console, char* logfile_name);
  ~Process();
  bool exists();
  const char* getName() { return name;}
  Hfile getOutputHandle() { return outputHandle;}
  priorityLevel getPriorityLevel() { return priority; }
  Hprocess getProcessHandle() { return processHandle;}
  ProcessId getProcessId() { return processId;}
  Hprocess getThreadHandle() { return threadHandle;}
  ProcessId getThreadId() { return threadId;}
  int ID() { return id;}
  bool resume();
  void setLogfile(char* logfile);
  void setShowConsole(bool value) { showConsole = value; }
  void setPriorityLevel(priorityLevel level);
  bool start();
  bool stop();
  bool suspend();

private:
  char* arguments;
  char* command;
  int id;
  Hfile logfileHandle;
  char* logfileName;
  char* name;
  Hfile outputHandle;
  enum priorityLevel priority;
  Hprocess processHandle;
  ProcessId processId;
  bool showConsole;
  bool started;
  bool stopped;
  Hprocess threadHandle;
  ProcessId threadId;

  void init();
  Hfile setLogfile();
};


#endif
