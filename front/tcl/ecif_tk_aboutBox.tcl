#***********************************************************************
#
#       ELMER, A Computational Fluid Dynamics Program.
#
#       Copyright 1st April 1995 - , Center for Scientific Computing,
#                                    Finland.
#
#       All rights reserved. No part of this program may be used,
#       reproduced or transmitted in any form or by any means
#       without the written permission of CSC.
#
#                Address: Center for Scientific Computing
#                         Tietotie 6, P.O. BOX 405
#                         02101 Espoo, Finland
#                         Tel.     +358 0 457 2001
#                         Telefax: +358 0 457 2302
#                         EMail:   Jari.Jarvinen@csc.fi
#***********************************************************************

#***********************************************************************
#Program:   ELMER Front 
#Module:    ecif_tk_aboutBox.tcl
#Language:  Tcl
#Date:      05.10.98
#Version:   1.00
#Author(s): Martti Verho
#Revisions: 
#
#Abstract:  Help menu About-box
#
#************************************************************************


#------About box proc------
#
proc About::openPanel {} {
  # This procedure displays program's "About" box
  global Info About

  set wgeom +500+100

  set w .aboutbox
  set id [winfo atom $w]
  set globHelp(aboutWinId) $id

  if { 1 == [Util::checkPanelWindow About $id "About" $wgeom] } {
    return
  }  

  set About(dataChanged) 0
  set About(dataModified) 0

  toplevel $w
  focus $w 

  wm geometry $w $wgeom
  wm title $w "About $Info(PROJECT) $Info(MODULE)"

  set fpx $Info(framePadX2)
  set fpy $Info(framePadY2)
  #--Widgets
  frame $w.fl -bd 2 -relief groove
  frame $w.fb
  label $w.fl.l1 -text \
     "$Info(PROJECT) $Info(VERSION)\n\n \
      $Info(FRONT_NAME) $Info(FRONT_VERSION_NBR)\n\n \
      This is a front end program for setting\n \
      boundary conditions and other parameters\n \
      for CFD problems.\n\n"

  set ok_btn [button $w.fb.b1 -text OK -command "About::panelOk $w"]
  focus $ok_btn

  #--Packing
  pack $w.fb $w.fl -expand 1 -side bottom -fill both -padx $fpx -pady $fpy 
  pack $ok_btn $w.fl.l1 -expand 1 -side top  
}


proc About::panelOk {w} {
  Panel::cancel $w
}

#-end About box proc


