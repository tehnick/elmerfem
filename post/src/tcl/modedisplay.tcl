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
#*     Model file read utility routines
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

set OUTPS      0
set PSFileName ""

set DVector "Displacement"
set DScale  "1"
set DCycles "1"
set DMode   "1"
set DFrame  "20"
set tloop   "Animate"

proc mode_animate {} {
  global DCycles DVector DMode DScale DFrame tloop

  if { $tloop == "Animate" } {
    set tloop "Stop"
    math function f(d,t) import nodes {  _f=d(0:2,time(t-1)) };
    math n=nodes;

    do i 0 [@ $DCycles*$DFrame] {
      if { $tloop == "Animate" } {
        math nodes=n;
        return
      }
      math t=$i/$DFrame*2*pi
      math nodes=n+sin(t)*f($DVector,$DMode)*$DScale
      UpdateObject; display
    }
  }
  math nodes=n;
  set tloop "Animate"
}

proc ModeDisplay {} {
    set w .modedisplay

    if { [winfo exists $w] } {
      wm iconify $w
      wm deiconify $w
      return
    } else {
      toplevel $w
      place_window $w
      wm title $w "Mode Display"
    }

    label $w.title -text "Mode Display"
    pack $w.title

    set DVector "Displacement"
    frame $w.arrow
    label $w.arrow.label -text "Displacement Variable: "
    button $w.arrow.but -textvariable DVector \
               -command { set DVector [make_vector_list]; }

    pack $w.arrow -side top
    pack $w.arrow.label -side left
    pack $w.arrow.but -side left -fill x


    frame $w.file -relief sunken
    label $w.file.mlab -text "Select Mode: "
    entry $w.file.mode -width 10 -textvariable DMode
    label $w.file.dlab -text "Disp scale: "
    entry $w.file.disp -width 10 -textvariable DScale
    label $w.file.flab -text "Frames/Cycle: "
    entry $w.file.frms -width 10 -textvariable DFrame
    label $w.file.clab -text "Cycles: "
    entry $w.file.cycl -width 10 -textvariable DCycles
    pack $w.file.mlab $w.file.mode $w.file.dlab $w.file.disp
    pack $w.file.flab $w.file.frms $w.file.clab $w.file.cycl
# -side left

    set tloop "Animate"
    button $w.file.but -textvariable tloop -command { mode_animate; }

    pack $w.file.but -side left
    pack $w.file -side top

    label $w.sp4 -text \n
    pack $w.sp4 -side top

    frame $w.but
    button  $w.but.exit -text "Close" -command "destroy $w"
    pack $w.but $w.but.exit -side right
}
