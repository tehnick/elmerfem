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
Module:     ecif_stdafx.h
Language:   C++
Date:       01.10.98
Version:    1.00
Author(s):  Martti Verho
Revisions:  

Abstract:   This header file imports all system level h-files. 
            In MS VC++ it used for precompilation of non-changing h-files.

************************************************************************/

#ifndef _ECIF_STDAFX_
#define _ECIF_STDAFX_

#ifdef WIN32
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
  #undef WIN32_LEAN_AND_MEAN
  //This is needed for VC4.2!!!
  //#include <yvals.h>
#else
  #include <Xlib.h>
#endif


#endif
