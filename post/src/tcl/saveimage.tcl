
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

proc saveimage { File outps }   {
  global GlobalOptions

  if { $outps } {
     set GlobalOptions(OutputPS) 1
     display
     set GlobalOptions(OutputPS) 0
     display
  } else {
     screensave $File
  }
}


proc SaveImage {} {
    global PSFileName OutputPS OUTPS

    set w .screensave

    if { [winfo exists $w] } {
      wm iconify $w
      wm deiconify $w
      return
    } else {
      toplevel $w
      place_window $w
      wm title $w "Save Screen"
    }

#
# File name
#
    label $w.sp1 -text "Save as:"
    radiobutton $w.rb2 -value 0 -variable "OUTPS" -text "PPM Image"
    checkbutton $w.rb3  -variable "GlobalOptions(FitToPagePS)" -text "Fit PS to page"
    radiobutton $w.rb1 -value 1 -variable "OUTPS" -text "Postscript"
    pack $w.sp1 $w.rb1 $w.rb3 $w.rb2

#    pack $w.rb3 -side top

    label $w.sp3 -text "\nSelect file:\n" -font "Helvetica-Bold 12"
    pack $w.sp3 -side top

    frame $w.file -relief sunken
    label $w.file.lab -text "File Name: "
    entry $w.file.name -width 40 -textvariable PSFileName

    button $w.file.but -text "Browse..." -command \
       { set PSFileName [tk_getSaveFile -parent .screensave -title "Save Image To File"]; }

    pack $w.file.lab $w.file.name $w.file.but -side left
    pack $w.file -side top

    label $w.sp4 -text \n
    pack $w.sp4 -side top

    frame $w.but
    button  $w.but.exit -text "Close" -command "destroy $w"
    button  $w.but.save -text "Save" -command { saveimage $PSFileName $OUTPS }

    pack $w.but $w.but.exit $w.but.save -side right
}
