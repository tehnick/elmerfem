
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
#*    JPG save utility routines
#*
#*******************************************************************************
#*
#*                     Author:       Mikko Lyly
#*
#*                    Address: Center for Scientific Computing
#*                                Tietotie 6, P.O. BOX 405
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
set savejpg_quality 80
set JPGFileName "elmerpost.jpg"

proc savejpg.Control { } {
    global savejpg_control savejpg_quality JPGFileName

    set savejpg_control .savejpg_control
    
    if { [winfo exists $savejpg_control] } {
	destroy $savejpg_control.title
	destroy $savejpg_control.quality
	destroy $savejpg_control.file
	destroy $savejpg_control.save_button
	destroy $savejpg_control.buttons
    } else {
	toplevel $savejpg_control
	place_window $savejpg_control
    }

    wm title $savejpg_control "Savejpg control"
    #
    #   Quality control:
    #
    frame $savejpg_control.quality

    label $savejpg_control.quality.label1 -width 8 -text "Quality:   "
    entry $savejpg_control.quality.value -width 5 \
	-textvariable savejpg_quality
    label $savejpg_control.quality.label2 -width 20 -text "(1=low ... 100=best)"
    pack $savejpg_control.quality.label1 $savejpg_control.quality.value \
	$savejpg_control.quality.label2 -side left 
    
    pack $savejpg_control.quality -expand 1 -fill both -side top
    #
    # File name:
    #
    frame $savejpg_control.file
    
    label $savejpg_control.file.label -width 8 -text "File name:"
    entry $savejpg_control.file.name -width 30 -textvariable JPGFileName
    button $savejpg_control.file.button -text "Browse.." \
	-command { set JPGFileName [tk_getSaveFile -parent .savejpg_control \
					-title "Save Picture To File"]; }
    pack $savejpg_control.file.label $savejpg_control.file.name \
	$savejpg_control.file.button -side left -expand 1

    pack $savejpg_control.file -expand 1 -fill both -side top
    #
    #   Buttons:
    #
    frame $savejpg_control.buttons

    button $savejpg_control.buttons.close -text "Close" \
	-command { destroy $savejpg_control }

    frame $savejpg_control.save_button
    button $savejpg_control.save_button.save \
	-text "Save" -command { savejpg $JPGFileName $savejpg_quality }
    pack $savejpg_control.save_button.save \
	-side left -expand 1 -fill x

    pack $savejpg_control.buttons.close -side right

    pack $savejpg_control.save_button -side top -expand 1 -fill x
    pack $savejpg_control.buttons -side top -fill x -expand 1
}
