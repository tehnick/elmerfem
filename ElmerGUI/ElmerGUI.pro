#==============================================================================
#
#      ElmerGUI: qmake project file for Unix, Win32, and MacX
#
#==============================================================================

include(ElmerGUI.pri)

#------------------------------------------------------------------------------
# Optional components (undefine or comment out to exclude from compilation):
#------------------------------------------------------------------------------
DEFINES += QWT       # Use QWT for convergence monitor?
DEFINES += VTKPOST   # Use VTK for postprocessing?
DEFINES += MATC      # Use MATC for internal operations in postprocessing?
DEFINES += OCC_63    # Use OpenCASCADE 6.3 for importing CAD files? Needs VTK.
DEFINES += PYTHONQT  # Use PythonQt for scripting in post processor?

#------------------------------------------------------------------------------
# 64 bit system?
#------------------------------------------------------------------------------
BITS = 32

#------------------------------------------------------------------------------
# Target:
#------------------------------------------------------------------------------
TARGET = ElmerGUI
TEMPLATE = app
CONFIG += release

#------------------------------------------------------------------------------
# Installation directory:
#------------------------------------------------------------------------------
ELMERGUI_HOME = $${ELMER_HOME}/bin
target.path = $${ELMERGUI_HOME}
INSTALLS += target
edf.path = $${ELMERGUI_HOME}/edf
edf.files += edf/*
INSTALLS += edf

#------------------------------------------------------------------------------
# Compiler flags:
#------------------------------------------------------------------------------
CONFIG += warn_off

#------------------------------------------------------------------------------
# Directories:
#------------------------------------------------------------------------------
DEPENDPATH += . src forms plugins vtkpost cad
INCLUDEPATH += .
MOC_DIR = ./tmp
OBJECTS_DIR = ./tmp
RCC_DIR = ./tmp
UI_DIR = ./tmp

#------------------------------------------------------------------------------
# QT:
#------------------------------------------------------------------------------
QT += opengl xml script
CONFIG += uitools

#------------------------------------------------------------------------------
# NETGEN (see ./netgen/README for more details):
#------------------------------------------------------------------------------
INCLUDEPATH += netgen/libsrc/interface
LIBPATH += netgen/ngcore
LIBS += -lng

unix: QMAKE_PRE_LINK = cd netgen ; qmake ; make ; cd .. 
win32: QMAKE_PRE_LINK = cd netgen & qmake & nmake & cd ..
macx: QMAKE_PRE_LINK = cd netgen ; qmake ; make ; cd ..

#------------------------------------------------------------------------------
# PYTHONQT (see ./PythonQt/README for more details):
#------------------------------------------------------------------------------
contains(DEFINES, PYTHONQT) {
   INCLUDEPATH += $${PY_INCLUDEPATH} PythonQt/src
   LIBPATH += $${PY_LIBPATH} PythonQt/lib
   LIBS += $${PY_LIBS} -lPythonQt

   unix: QMAKE_PRE_LINK += ; cd PythonQt; qmake; make; cd .. 
   win32: QMAKE_PRE_LINK += & cd PythonQt & qmake & nmake & cd .. 
   macx: DEFINES -= PYTHONQT  # not supported at the moment
}

#------------------------------------------------------------------------------
# QWT:
#------------------------------------------------------------------------------
contains(DEFINES, QWT) {
   INCLUDEPATH += $${QWT_INCLUDEPATH}
   LIBPATH += $${QWT_LIBPATH}
   LIBS += $${QWT_LIBS}
}

#------------------------------------------------------------------------------
# VTK:
#------------------------------------------------------------------------------
contains(DEFINES, VTKPOST) {
   INCLUDEPATH += $${VTK_INCLUDEPATH}
   LIBPATH += $${VTK_LIBPATH}
   LIBS += $${VTK_LIBS}
}

#------------------------------------------------------------------------------
# MATC:
#------------------------------------------------------------------------------
contains(DEFINES, MATC) {
   LIBPATH += matc/lib
   LIBS += -lmatc

   unix: QMAKE_PRE_LINK += ; cd matc; qmake; make; cd .. 
   win32: QMAKE_PRE_LINK += & cd matc & qmake & nmake & cd ..
   macx: QMAKE_PRE_LINK += ; cd matc; qmake; make; cd .. 
}

#------------------------------------------------------------------------------
# OpenCASCADE:
#------------------------------------------------------------------------------
contains(DEFINES, OCC_63) {
   contains(BITS, 64):  DEFINES += _OCC64

   unix: DEFINES += HAVE_CONFIG_H HAVE_IOSTREAM HAVE_FSTREAM HAVE_LIMITS_H
   win32: DEFINES += WNT CSFDB
   macx: DEFINED -= OCC_63         # not supported at the moment

   INCLUDEPATH += $${OCC_INCLUDEPATH}
   LIBPATH += $${OCC_LIBPATH}
   LIBS += $${OCC_LIBS}
}

#------------------------------------------------------------------------------
# Process info query on win32:
#------------------------------------------------------------------------------
win32: LIBS += -lpsapi

#------------------------------------------------------------------------------
# Input files:
#------------------------------------------------------------------------------
HEADERS += src/bodypropertyeditor.h \
           src/boundarydivision.h \
           src/boundarypropertyeditor.h \
           src/checkmpi.h \
           src/dynamiceditor.h \
           src/edfeditor.h \
           src/egini.h \
           src/generalsetup.h \
           src/glcontrol.h \
           src/glwidget.h \
           src/helpers.h \
           src/mainwindow.h \
           src/materiallibrary.h \
           src/maxlimits.h \
           src/meshcontrol.h \
           src/meshingthread.h \
           src/meshtype.h \
           src/meshutils.h \
           src/operation.h \
           src/parallel.h \
           src/projectio.h \
           src/sifgenerator.h \
           src/sifwindow.h \
           src/solverparameters.h \
           src/summaryeditor.h \
           plugins/egconvert.h \
           plugins/egdef.h \
           plugins/egmain.h \
           plugins/egmesh.h \
           plugins/egnative.h \
           plugins/egtypes.h \
           plugins/egutils.h \
           plugins/elmergrid_api.h \
           plugins/nglib_api.h \
           plugins/tetgen.h \
           plugins/tetlib_api.h

FORMS += forms/bodypropertyeditor.ui \
         forms/boundarydivision.ui \
         forms/boundarypropertyeditor.ui \
         forms/generalsetup.ui \
         forms/glcontrol.ui \
         forms/materiallibrary.ui \
         forms/meshcontrol.ui \
         forms/parallel.ui \
         forms/solverparameters.ui \
         forms/summaryeditor.ui

SOURCES += src/bodypropertyeditor.cpp \
           src/boundarydivision.cpp \
           src/boundarypropertyeditor.cpp \
           src/checkmpi.cpp \
           src/dynamiceditor.cpp \
           src/edfeditor.cpp \
           src/egini.cpp \
           src/generalsetup.cpp \
           src/glcontrol.cpp \
           src/glwidget.cpp \
           src/helpers.cpp \
           src/main.cpp \
           src/mainwindow.cpp \
           src/materiallibrary.cpp \
           src/maxlimits.cpp \
           src/meshcontrol.cpp \
           src/meshingthread.cpp \
           src/meshtype.cpp \
           src/meshutils.cpp \
           src/operation.cpp \
           src/parallel.cpp \
           src/projectio.cpp \
           src/sifgenerator.cpp \
           src/sifwindow.cpp \
           src/solverparameters.cpp \
           src/summaryeditor.cpp \
           plugins/egconvert.cpp \
           plugins/egmain.cpp \
           plugins/egmesh.cpp \
           plugins/egnative.cpp \
           plugins/egutils.cpp \
           plugins/elmergrid_api.cpp \
           plugins/nglib_api.cpp \
           plugins/tetlib_api.cpp

#------------------------------------------------------------------------------
# Optional input files:
#------------------------------------------------------------------------------
contains(DEFINES, QWT) {
   HEADERS += src/convergenceview.h
   SOURCES += src/convergenceview.cpp
}

contains(DEFINES, VTKPOST) {
   HEADERS += vtkpost/axes.h \
              vtkpost/featureedge.h \
              vtkpost/vtkpost.h \
              vtkpost/isosurface.h \
              vtkpost/isocontour.h \
              vtkpost/epmesh.h \
              vtkpost/colorbar.h \
              vtkpost/meshpoint.h \
              vtkpost/meshedge.h \
              vtkpost/surface.h \
              vtkpost/preferences.h \
              vtkpost/vector.h \
              vtkpost/readepfile.h \
              vtkpost/streamline.h \
              vtkpost/timestep.h

   FORMS += vtkpost/axes.ui \
            vtkpost/featureedge.ui \
            vtkpost/isosurface.ui \
            vtkpost/isocontour.ui \
            vtkpost/colorbar.ui \
            vtkpost/surface.ui \
            vtkpost/meshpoint.ui \
            vtkpost/meshedge.ui \
            vtkpost/preferences.ui \
            vtkpost/vector.ui \
            vtkpost/readepfile.ui \
            vtkpost/streamline.ui \
            vtkpost/timestep.ui

   SOURCES += vtkpost/axes.cpp \
              vtkpost/featureedge.cpp \
              vtkpost/vtkpost.cpp \
              vtkpost/isosurface.cpp \
              vtkpost/isocontour.cpp \
              vtkpost/epmesh.cpp \
              vtkpost/colorbar.cpp \
              vtkpost/meshpoint.cpp \
              vtkpost/meshedge.cpp \
              vtkpost/surface.cpp \
              vtkpost/preferences.cpp \
              vtkpost/vector.cpp \
              vtkpost/readepfile.cpp \
              vtkpost/streamline.cpp \
              vtkpost/timestep.cpp

   contains(DEFINES, MATC) {
      HEADERS += vtkpost/matc.h \
                 vtkpost/mc.h

      FORMS += vtkpost/matc.ui

      SOURCES += vtkpost/matc.cpp
   }
}

contains(DEFINES, OCC_63) {
   HEADERS += cad/cadview.h \
              cad/cadpreferences.h

   FORMS += cad/cadpreferences.ui

   SOURCES += cad/cadview.cpp \
              cad/cadpreferences.cpp
}

#------------------------------------------------------------------------------
# Resource files:
#------------------------------------------------------------------------------
RESOURCES += ElmerGUI.qrc
win32: RC_FILE += ElmerGUI.rc
macx: RC_FILE = M3Dicon.icns

#------------------------------------------------------------------------------
# END OF FILE
#------------------------------------------------------------------------------
