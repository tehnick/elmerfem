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
Module:     ecif_process_UNIX.hpp
Language:   C++
Date:       20.01.99
Version:    1.00
Author(s):  Martti Verho
Revisions:  
 
Abstract:   Implementation, UNIX specific

************************************************************************/

// Helper routines
// ===============
void
display_system_msg(char* msg)
{
  cerr << msg << endl;
}


// System specific, called in main
// ===============================
void initConsole()
{
}


// =====================
// Process class methods
// =====================

bool
Process::exists()
{
  return true;
}


bool
Process::resume()
{
  return false;
}


Hfile
Process::setLogfile()
{
  if (logfileName == NULL ||
      logfileName[0] == '\0'
     )
    return 0;

  // STDERR
  return 2;
}


bool
Process::start()
{
  strstream strm;
  strm << command << ' ' << arguments << ends;

  system(strm.str());

  return true;
}


bool
Process::stop()
{
  return false;
}


bool
Process::suspend()
{
  return false;
}


void
Process::setPriorityLevel(priorityLevel level)
{ 
  priority = level;
}

