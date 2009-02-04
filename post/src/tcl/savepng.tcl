#*******************************************************************************
#*
#*       ELMER, A Computational Fluid Dynamics Program.
#*
#*       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
#*                                    Finland.
#*
#*       All rights reserved. No part of this program may be used,
#*       reproduced or transmitted in any form or by any means
#*       without the written permission of CSC.
#*
#*******************************************************************************

#*******************************************************************************
#*
#*    PNG save utility routines
#*
#*******************************************************************************
#*
#*                     Author:       Mikko Lyly
#*
#*                    Address: CSC - IT Center for Science Ltd.
#*                                Keilaranta 14, P.O. BOX 405
#*                                  02101 Espoo, Finland
#*                                  Tel. +358 0 457 2723
#*                                Telefax: +358 0 457 2302
#*                              EMail: Juha.Ruokolainen@csc.fi
#*
#*                       Date: 04 Oct 2007
#*
#*                Modified by:
#*
#*       Date of modification:
#*
#******************************************************************************
set PNGFileName "elmerpost.png"

proc savepng.Control { } {
    global savepng_control PNGFileName 

    set savepng_control .savepng_control
    
    if { [winfo exists $savepng_control] } {
	destroy $savepng_control.title
	destroy $savepng_control.file
	destroy $savepng_control.save_button
	destroy $savepng_control.buttons
    } else {
	toplevel $savepng_control
	place_window $savepng_control
    }

    wm title $savepng_control "Savepng control"
    #
    # File name:
    #
    frame $savepng_control.file
    
    label $savepng_control.file.label -width 8 -text "File name:"
    entry $savepng_control.file.name -width 30 -textvariable PNGFileName
    button $savepng_control.file.button -text "Browse.." \
	-command { set PNGFileName [tk_getSaveFile -parent .savepng_control \
					-title "Save Picture To File"]; }
    pack $savepng_control.file.label $savepng_control.file.name \
	$savepng_control.file.button -side left -expand 1

    pack $savepng_control.file -expand 1 -fill both -side top
    #
    #   Buttons:
    #
    frame $savepng_control.buttons

    button $savepng_control.buttons.close -text "Close" \
	-command { destroy $savepng_control }

    frame $savepng_control.save_button
    button $savepng_control.save_button.save \
	-text "Save" -command { savepng $PNGFileName  }
    pack $savepng_control.save_button.save \
	-side left -expand 1 -fill x

    pack $savepng_control.buttons.close -side right

    pack $savepng_control.save_button -side top -expand 1 -fill x
    pack $savepng_control.buttons -side top -fill x -expand 1
}
