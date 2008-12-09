#==============================================================================
#
#        ElmerGUI: qmake project file for Unix, Win32, and MacX
#
#     For more details, see the project files in the subdirectories
#
#==============================================================================

include(ElmerGUI.pri)

#------------------------------------------------------------------------------
# Test the configuration for some headers:
#------------------------------------------------------------------------------
!exists($${QWT_INCLUDEPATH}/qwt.h) {
   message("QWT has been defined, but qwt.h was not found")
   message("Check QWT_INCLUDEPATH or undefine QWT in ElmerGUI.pri")
   error("Detected inconsistent configuration. Unable to continue.")
}

!exists($${VTK_INCLUDEPATH}/QVTKWidget.h) {
   message("VTKPOST has been defined, but QVTKWidget.h was not found")
   message("Check VTK_INCLUDEPATH or undefine VTKPOST in ElmerGUI.pri")
   error("Detected inconsistent configuration. Unable to continue.")
}

!exists($${OCC_INCLUDEPATH}/BRepTools.hxx) {
   message("OCC_63 has been defined, but BRepTools.hxx was not found")
   message("Check OCC_INCLUDEPATH or undefine OCC_63 in ElmerGUI.pri")
   error("Detected inconsistent configuration. Unable to continue.")
}

!exists($${PY_INCLUDEPATH}/Python.h) {
   message("PYTHONQT has been defined, but Python.h was not found")
   message("Check PY_INCLUDEPATH or undefine PYTHONQT in ElmerGUI.pri")
   error("Detected inconsistent configuration. Unable to continue.")
}

message(ELMER_HOME = $${ELMER_HOME})

#------------------------------------------------------------------------------
# Build in all subdirectories:
#------------------------------------------------------------------------------
TEMPLATE = subdirs
SUBDIRS = matc netgen
contains(DEFINES, PYTHONQT): SUBDIRS += PythonQt
SUBDIRS += Application
