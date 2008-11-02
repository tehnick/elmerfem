#*******************************************************************************
#*
#*       ELMER, A Computational Fluid Dynamics Program.
#*
#*       Copyright 1st April 1995 - , Center for Scientific Computing,
#*                                    Finland.
#*
#*       All rights reserved. No part of this program may be used,
#*       reproduced or transmitted in any form or by any means
#*       without the written permission of CSC.
#*
#*******************************************************************************

#*******************************************************************************
#*
#*  Makefile for objects subdirectory of ElmerPost
#*
#*******************************************************************************
#*
#*                     Author:       Juha Ruokolainen
#*
#*                    Address: Center for Scientific Computing
#*                                Tietotie 6, P.O. BOX 405
#*                                  02101 Espoo, Finland
#*                                  Tel. +358 0 457 2723
#*                                Telefax: +358 0 457 2302
#*                              EMail: Juha.Ruokolainen@csc.fi
#*
#*                       Date: 1 Oct 1995
#*
#*                Modified by:
#*
#*       Date of modification:
#*
#*******************************************************************************

objdir = .\src

OBJS = $(objdir)\eig.obj $(objdir)\funcs.obj $(objdir)\lu.obj $(objdir)\oper.obj $(objdir)\str.obj $(objdir)\eval.obj $(objdir)\jacobi.obj $(objdir)\matc.obj $(objdir)\optim.obj $(objdir)\urand.obj $(objdir)\files.obj $(objdir)\lists.obj $(objdir)\matrix.obj $(objdir)\parser.obj $(objdir)\variable.obj $(objdir)\gra.obj $(objdir)\gra_com.obj $(objdir)\clip.obj $(objdir)\c3d.obj $(objdir)\dri_ps.obj $(objdir)\error.obj

.c.obj:
	cl -DWIN32 -c -nologo -Zm200 -O2 -MD -GR -EHsc -W3 $?
	copy *.obj $(objdir)
	del *.obj

matc.lib : $(OBJS)
	LIB -out:matc.lib $(OBJS)
