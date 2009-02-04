#***********************************************************************
#
#       ELMER, A Computational Fluid Dynamics Program.
#
#       Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.,
#                                    Finland.
#
#       All rights reserved. No part of this program may be used,
#       reproduced or transmitted in any form or by any means
#       without the written permission of CSC.
#
#                Address: CSC - IT Center for Science Ltd.
#                         Keilaranta 14, P.O. BOX 405
#                         02101 Espoo, Finland
#                         Tel.     +358 0 457 2001
#                         Telefax: +358 0 457 2302
#                         EMail:   Jari.Jarvinen@csc.fi
#***********************************************************************
 
#***********************************************************************
#Program:   ELMER Front 
#Module:    ecif_tk_procsLis.tcl
#Language:  Tcl
#Date:      31.01.00
#Version:   1.00
#Author(s): Martti Verho
#Revisions: 
#
#Abstract:  List handling procedures
#
#************************************************************************


#######################
#### List routines ####
#######################

# Remove an item from list of items
#
proc List::removeFromList { list_items remove_items } {

	foreach item $remove_items {

		set index [lsearch $list_items $item]

		if { $index != -1 } {
			set list_items [lreplace $list_items $index $index]
		}
  }
	
	return $list_items
}


# Add id to the ids-list if it not yet there
#
proc List::addToIdList {ids id_to_add { sort 1} } {

  foreach id $ids {

    # New-id already in the list, do nothing!
    if { $id == $id_to_add } {
      return $ids
    }
  }

  # Add new id, sort result
  lappend ids $id_to_add

  if { $sort } {
    return [lsort $ids]

  } else {
    return $ids
  }
}


# Add id to the ids-list if it not yet there
# Return updates indices and ids as a double list
#
proc List::addToIndexedIdList {indices ids id_index id_to_add { sort 1 } } {

  set pos 0
  foreach index $indices {

    # If id's index already in the list, replace id!
    if { $index == $id_index } {
      
      set ids [lreplace $ids $pos $pos $id_to_add]

      lappend result $indices
      lappend result $ids

      return $result
    }

    incr pos
  }

  # Append new index and id
  set new_indices $indices
  lappend new_indices $id_index

  set new_ids $ids
  lappend new_ids $id_to_add

  lappend result $new_indices
  lappend result $new_ids

  return $result
}


# Remove id from the ids-list if there
#
proc List::removeFromIdList {ids id_to_remove} {

  set pos 0
  foreach id $ids {

    # Remove id
    if { $id == $id_to_remove } {
      return [lreplace $ids $pos $pos]
    }

    incr pos
  }

  # Id not in the list, return original ids!
  return $ids
}


# Remove id from from given index position
# Return updated indices and ids as a double list
#
proc List::removeFromIndexedIdList {indices ids id_index id_to_remove} {

  set pos 0
  foreach index $indices {

    # Remove id
    if { $index == $id_index } {
      set new_indices [lreplace $indices $pos $pos]
      set new_ids [lreplace $ids $pos $pos]

      lappend result $new_indices
      lappend result $new_ids

      return $result
    }

    incr pos
  }

  # Index not in the list, return orginal lists
  lappend result $indices
  lappend result $ids

  return $result
}


proc List::getListRowsByIndex {source indices } {

  set result ""
  foreach i $indices {
    lappend result [lindex $source $i]
  }
  return $result
}


proc List::getListRowsByRange {source ranges } {
  
  set result ""
  foreach i $ranges {
    set range_list [lrange $source [lindex $i 0] [lindex $i 1]]
    foreach index $range_list {
      lappend result $index
    }
  }
  return $result
}


proc List::makeIndicesFromRange {ranges} {
  foreach i $ranges {
    set start [lindex $i 0]
    set end [lindex $i 1]
    for {set index $start} {$index <= $end} {incr index} {
      lappend result $index
    }
  }
  return $result
}


proc List::getUniqueIdList {id_list} {
  global Info

  set result ""

  set prev_id $Info(NO_INDEX)
  set id_list [lsort -integer $id_list]

  foreach id $id_list {

    if { $id != $prev_id } {
      lappend result $id
      set prev_id $id
    }
  }

  return $result
}


# Check if 'list' contains 'string'
# -Case sensitive search
# -Trailing blanks ignored
#
# Returns: list-index (0...) or -1 if not found
#
proc List::caseSearch {list str} {

  set idx 0
  
  set str [string trim $str]

  foreach item $list {

    set item [string trim $item]
    
    if { [string equal $item $str] } {
      return $idx
    }

    incr idx
  }

  return -1
}


# Check if 'list' contains 'string'
# -No case sensitive search
# -Trailing blanks ignored
#
# Returns: list-index (0...) or -1 if not found
#
proc List::nocaseSearch {list str} {

  set idx 0
  
  set str [string trim $str]

  foreach item $list {

    set item [string trim $item]
    
    if { [string equal -nocase $item $str] } {
      return $idx
    }

    incr idx
  }

  return -1
}


# Check if a 'file_list' contains filename 'fname'
# -Case or Nocase sensitive search, depending on system
# -Trailing blanks ignored
#
# Returns: list-index (0...) or -1 if not found
#
proc List::fileListSearch {file_list fname} {
  global Info

  if { $Info(FNCS) } {
    set nocase ""
  } else {
    set nocase "-nocase"
  }

  set idx 0
  
  set fname [string trim $fname]

  foreach fn $file_list {

    set fn [string trim $fn]
    
    if { [string equal $nocase $fn $fname] } {
      return $idx
    }

    incr idx
  }

  return -1
}



# end ecif_tk_procsList.tcl
# ********************
