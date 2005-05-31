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
#* Initialize some global TCL variables, and include other TCL/TK commands
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
#*                       Date: 26 Sep 1995
#*
#*                Modified by:
#*
#*       Date of modification:
#*
#*******************************************************************************

set tcl_interactive 1

if { [info exists env(ELMER_POST_HOME)] } {

   set ELMER_POST_HOME $env(ELMER_POST_HOME)

} else {

   set ELMER_POST_HOME "/mnt/mds/csc/jpr/SRC/ELMER/PostProcessor/"

}

frame .top -relief raised -bd 1
pack  .top -side top -fill both

frame .bot -relief raised -bd 1
pack  .bot -side top -fill both

message .top.msg -width 3i -text "Send Interrupt" -font -Adobe-Times-Medium-R-Normal-*-140-*
pack .top.msg -expand 1 -fill both -padx 5m -pady 5m

button .bot.button -bg gray -text "Send Interrupt" -command "SendInterrupt"
pack .bot.button -expand 1 -fill both -padx 5m -pady 5m
