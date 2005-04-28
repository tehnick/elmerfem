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
Module:     ecif_timer.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   Simple timer class for elapsed time.

************************************************************************/

#ifndef _TIMER_
#define _TIMER_

#include "ecif_def.h"

class Timer
{
public:
  Timer();   
  void start();
  void stop();
  double getLapTime(enum timeType time_type = PROCESS_TIME);
  double getEndTime(enum timeType time_type = PROCESS_TIME);
  static void formTimeString(double seconds, char* buffer);

private:
  double getProcessTime();
  double getWallClockTime();

  short started;
  short stopped;
  double start_process_time;
  double end_process_time;
  double start_wall_time;
  double end_wall_time;  
};


#endif
