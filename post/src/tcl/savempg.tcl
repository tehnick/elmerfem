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
#*    MPG save utility routines
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
#*                       Date: 04 Oct 2007
#*
#*                Modified by:
#*
#*       Date of modification:
#*
#*******************************************************************************
set savempg_start_stop_state "Start"
set savempg_bitrate 1000000
set MPGFileName "elmerpost.mpg"
#
# Helper procs:
#
proc savempg_start_stop {  } {
    global savempg_start_stop_state savempg_bitrate MPGFileName

    if { $savempg_start_stop_state == "Start" } {
	savempg bitrate $savempg_bitrate
	savempg start $MPGFileName
	set savempg_start_stop_state "Stop"
    } else {
	savempg stop
	set savempg_start_stop_state "Start"
    }
}

proc savempg_append { } {
    global savempg_start_stop_state

    if { $savempg_start_stop_state == "Stop" } {
	savempg append
    }
}

proc savempg_close { } {
    global savempg_start_stop_state

    if { $savempg_start_stop_state == "Stop" } {
	savempg stop
	set savempg_start_stop_state "Start"
    }
}
#
# Main control:
#
proc savempg.Control { } {
    global savempg_control savempg_start_stop_state savempg_bitrate MPGFileName

    set savempg_control .savempg_control
    
    if { [winfo exists $savempg_control] } {
	destroy $savempg_control.title
	destroy $savempg_control.bitrate 
	destroy $savempg_control.file
	destroy $savempg_control.start_stop_button
	destroy $savempg_control.append_button
	destroy $savempg_control.buttons
    } else {
	toplevel $savempg_control
	place_window $savempg_control
    }

    wm title $savempg_control "Savempg control"
    #
    #   Bitrate control:
    #
    frame $savempg_control.bitrate

    label $savempg_control.bitrate.label1 -width 8 -text "Bitrate:   "
    entry $savempg_control.bitrate.value -width 10 \
	-textvariable savempg_bitrate
    label $savempg_control.bitrate.label2 -width 3 -text "bps"
    pack $savempg_control.bitrate.label1 $savempg_control.bitrate.value \
	$savempg_control.bitrate.label2 -side left 
    
    pack $savempg_control.bitrate -expand 1 -fill both -side top
    #
    # File name:
    #
    frame $savempg_control.file
    
    label $savempg_control.file.label -width 8 -text "File name:"
    entry $savempg_control.file.name -width 30 -textvariable MPGFileName
    button $savempg_control.file.button -text "Browse.." \
	-command { set MPGFileName [tk_getSaveFile -parent .savempg_control \
					-title "Save Video Clip To File"]; }
    pack $savempg_control.file.label $savempg_control.file.name \
	$savempg_control.file.button -side left -expand 1

    pack $savempg_control.file -expand 1 -fill both -side top
    #
    #   Buttons:
    #
    frame $savempg_control.buttons

    button $savempg_control.buttons.close -text "Close" \
	-command { savempg_close; destroy $savempg_control }

    frame $savempg_control.start_stop_button
    button $savempg_control.start_stop_button.start_stop \
	-textvariable savempg_start_stop_state  \
	-command { savempg_start_stop }
    pack $savempg_control.start_stop_button.start_stop \
	-side left -expand 1 -fill x

    frame $savempg_control.append_button
    button $savempg_control.append_button.append -text "Append" \
	-command savempg_append
    pack $savempg_control.append_button.append -side left -expand 1 -fill x

    pack $savempg_control.buttons.close -side right

    pack $savempg_control.start_stop_button -side top -expand 1 -fill x
    pack $savempg_control.append_button -side top -expand 1 -fill x
    pack $savempg_control.buttons -side top -fill x -expand 1
}
