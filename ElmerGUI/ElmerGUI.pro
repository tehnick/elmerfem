#==============================================================================
#
#                      ElmerGUI.pro for Linux and MinGW
#
#==============================================================================

#------------------------------------------------------------------------------
# Optional components:
#------------------------------------------------------------------------------
DEFINES += QWT       # Use QWT for convergence monitor?
DEFINES += VTKPOST   # Use VTK for postprocessing?
DEFINES += MATC      # Use MATC for internal operations in postprocessing?

#------------------------------------------------------------------------------
# Application:
#------------------------------------------------------------------------------
TEMPLATE = app
TARGET = ElmerGUI

#------------------------------------------------------------------------------
# Directories:
#------------------------------------------------------------------------------
DEPENDPATH += . src forms plugins vtkpost
INCLUDEPATH += .
MOC_DIR = ./tmp
OBJECTS_DIR = ./tmp
RCC_DIR = ./tmp
UI_DIR = ./tmp

#------------------------------------------------------------------------------
# Compiler flags:
#------------------------------------------------------------------------------
QMAKE_CXXFLAGS = -O2 -Wno-deprecated
QMAKE_CXXFLAGS_WARN_ON = 

#------------------------------------------------------------------------------
# QT:
#------------------------------------------------------------------------------
QT += opengl xml script
CONFIG += uitools

#------------------------------------------------------------------------------
# QWT (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, QWT) {
   unix {
      INCLUDEPATH += /usr/include/qwt-qt4
      LIBS += -lqwt-qt4
   }

   win32 {
      INCLUDEPATH += /c/QWT/include
      LIBPATH += /c/QWT/lib
      LIBS += -lqwt5
   }
}

#------------------------------------------------------------------------------
# VTK (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, VTKPOST) {
   unix {
      INCLUDEPATH += /usr/include/vtk-5.0
      LIBS += -lvtkHybrid -lQVTK
   }

   win32 {
      INCLUDEPATH += /c/VTK/include/vtk-5.2
      LIBPATH += /c/VTK/lib/vtk-5.2
      LIBS += -lvtkCommon -lvtkRendering -lvtkFiltering -lvtkGraphics -lvtkIO  -lvtkHybrid -lQVTK
   }
}

#------------------------------------------------------------------------------
# MATC (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, MATC) {
   unix {
      LIBPATH += /usr/local/lib
      LIBS += -lmatc
   }

   win 32 {
      LIBPATH += /c/Elmer5.4/lib
      LIBS += -lmatc
   }
}

#------------------------------------------------------------------------------
# Process info query on win32:
#------------------------------------------------------------------------------
win32 {
   LIBS += -lpsapi
}

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
           plugins/nglib.h \
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

#------------------------------------------------------------------------------
# Resource files:
#------------------------------------------------------------------------------
RESOURCES += ElmerGUI.qrc
RC_FILE += ElmerGUI.rc   

#------------------------------------------------------------------------------
# END OF FILE
#------------------------------------------------------------------------------
