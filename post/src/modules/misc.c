/******************************************************************************
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
 ******************************************************************************/

/*******************************************************************************
 *
 *     Misc graphics utilities.
 *
 *******************************************************************************
 *
 *                     Author:       Juha Ruokolainen
 *
 *                    Address: Center for Scientific Computing
 *                                Tietotie 6, P.O. BOX 405
 *                                  02101 Espoo, Finland
 *                                  Tel. +358 0 457 2723
 *                                Telefax: +358 0 457 2302
 *                              EMail: Juha.Ruokolainen@csc.fi
 *
 *                       Date: 6 Jun 1996
 *
 *                Modified by:
 *
 *       Date of modification:
 *
 ******************************************************************************/

/*
 * $Id: misc.c,v 1.3 1999/06/04 15:13:20 jim Exp $ 
 *
 * $Log: misc.c,v $
 * Revision 1.3  1999/06/04 15:13:20  jim
 * *** empty log message ***
 *
 * Revision 1.2  1998/08/01 12:35:00  jpr
 *
 * Added Id, started Log.
 * 
 *
 */

#include "../elmerpost.h"

#include <tcl.h>
#include <tk.h>

static int LineSmooth(ClientData cl,Tcl_Interp *interp,int argc,char **argv)
{
   if ( argc>1 )
   {
     if ( strcmp( argv[1], "on" ) == 0 )
     {
        glEnable( GL_LINE_SMOOTH );
        glEnable( GL_BLEND );
     } else {
        glDisable( GL_LINE_SMOOTH );
        glDisable( GL_BLEND );
     }
   } else {
     glEnable( GL_LINE_SMOOTH );
     glEnable( GL_BLEND );
   }

   return TCL_OK;
}

static int GraphicsClear( ClientData cl,Tcl_Interp *interp,int argc,char **argv )
{
   extern double br,bg,bb;

   glClearColor( br,bg,bb,1.0 );
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT  );

   return TCL_OK;
}

int Misc_Init( Tcl_Interp *interp )
{
   Tcl_CreateCommand( interp,"linesmooth",LineSmooth,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateCommand( interp,"gclear",GraphicsClear,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   return TCL_OK;
}
