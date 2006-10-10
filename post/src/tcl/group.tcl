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
#* Element grouping parameter settings
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
#
# 
#

#
# 22 Apr 1996
#

proc group_edit { } {
    global Groups NumberOfGroups GroupStatus

    if { [winfo exists .groups] } {
        wm iconify .groups
        wm deiconify .groups
        return
    }

    toplevel .groups
    place_window ".groups"

    frame .groups.lframe
    text .groups.lframe.list -yscroll ".groups.lframe.scroll set"
    scrollbar .groups.lframe.scroll -command ".groups.lframe.list yview"
    pack .groups.lframe.scroll -side left -expand 1 -fill both
    pack .groups.lframe.list -side left -fill y
    pack .groups.lframe -side top -expand 1 -fill both


    do i 0 [@ $NumberOfGroups-1] {
       checkbutton .groups.lframe.g$i -variable GroupStatus($i) -text $Groups($i)\n
       .groups.lframe.list window create end -window .groups.lframe.g$i
#       pack .groups.lframe.g$i -side top -expand 1 -fill x
    }
    pack .groups.lframe
#
# buttons
#
    frame .groups.buttons

    button .groups.buttons.apply -text "Apply" -command { group; display }

    button .groups.buttons.ok -text "Ok" -command { group; display; destroy .groups; }

    button .groups.buttons.close -text "Cancel" -command { destroy .groups }

    pack .groups.buttons.apply -side left
    pack .groups.buttons.ok    -side left
    pack .groups.buttons.close -side left -fill x
    pack .groups.buttons       -side top
}
