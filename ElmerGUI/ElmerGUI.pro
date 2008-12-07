#==============================================================================
#
#      ElmerGUI: qmake project file for Unix, Win32, and MacX
#
#==============================================================================

#------------------------------------------------------------------------------
# Optional components:
#------------------------------------------------------------------------------
DEFINES += QWT       # Use QWT for convergence monitor?
DEFINES += VTKPOST   # Use VTK for postprocessing?
DEFINES += MATC      # Use MATC for internal operations in postprocessing?
DEFINES += OCC_63    # Use OpenCASCADE 6.3 for importing CAD files? Needs VTK.
# DEFINES += PYTHONQT  # Use PythonQt for scripting? Works only with *nix atm.

#------------------------------------------------------------------------------
# Target:
#------------------------------------------------------------------------------
TEMPLATE = app
TARGET = ElmerGUI
CONFIG += release

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
# Compiler flags:
#------------------------------------------------------------------------------
unix {
   QMAKE_CXXFLAGS = -Wno-deprecated
   QMAKE_CXXFLAGS_WARN_ON = 
}

win32 {
   QMAKE_CXXFLAGS = /wd4005 /wd4100 /wd4996 /wd4305 \
                    /wd4805 /wd4189 /wd4390 /wd4554
}

#------------------------------------------------------------------------------
# QT:
#------------------------------------------------------------------------------
QT += opengl xml script
CONFIG += uitools

#------------------------------------------------------------------------------
# NETGEN (see ./netgen/README for more details):
#------------------------------------------------------------------------------
unix:  QMAKE_PRE_LINK = cd netgen ; qmake ; make ; cd .. ; 
win32: QMAKE_PRE_LINK = cd netgen & qmake & nmake & cd ..
macx:  QMAKE_PRE_LINK = cd netgen ; qmake ; make ; cd .. ; 

INCLUDEPATH += ./netgen/libsrc/interface
LIBPATH += ./netgen/ngcore
LIBS += -lng

#------------------------------------------------------------------------------
# PYTHONQT (see ./PythonQt/README for more details):
#------------------------------------------------------------------------------
contains(DEFINES, PYTHONQT) {
   unix {
      QMAKE_PRE_LINK += cd PythonQt; qmake; make; cd .. ; 
      INCLUDEPATH += /usr/include/python2.5
      INCLUDEPATH += ./PythonQt/src
      LIBPATH += ./PythonQt/lib
      LIBS += -lPythonQt
   }

   win32 {
      QMAKE_PRE_LINK += & cd PythonQt & qmake & make & cd .. 
      INCLUDEPATH += c:\PYTHON\Python-2.6.1\Include
      INCLUDEPATH += c:\PYTHON\Python-2.6.1\PC             # pyconfig.h
      INCLUDEPATH += ./PythonQt/src
      LIBPATH += ./PythonQt/lib
      LIBPATH += c:\PYTHON\Python-2.6.1\PCbuild
      LIBS += -lpython26 -lPythonQt
   }

   macx {
      DEFINES -= PYTHONQT
   }
}

#------------------------------------------------------------------------------
# QWT (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, QWT) {
   unix {
      INCLUDEPATH += /usr/include/qwt-qt4
      LIBS += -lqwt-qt4
   }

   win32 {
      INCLUDEPATH += c:\qwt\5.1.1\vc\include
      LIBPATH += c:\qwt\5.1.1\vc\lib
      LIBS += -lqwt5
   }

   macx {
      INCLUDEPATH += /usr/local/qwt-5.0.2/include
      LIBPATH += /usr/local/qwt-5.0.2/lib
      LIBS += -lqwt5
   }
}

#------------------------------------------------------------------------------
# VTK (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, VTKPOST) {
   unix {
      INCLUDEPATH += /usr/include/vtk-5.0
      LIBS += -lvtkHybrid -lvtkWidgets -lQVTK
   }

   win32 {
      INCLUDEPATH += c:\VTK\VC\include\vtk-5.2
      LIBPATH += c:\VTK\VC\lib\vtk-5.2
      LIBS += -lQVTK \
              -lvtkCommon \
              -lvtkDICOMParser \
              -lvtkFiltering \
              -lvtkGenericFiltering \
              -lvtkGraphics \
              -lvtkHybrid \
              -lvtkIO \
              -lvtkImaging \
              -lvtkInfovis \
              -lvtkNetCDF \
              -lvtkRendering \
              -lvtkViews \
              -lvtkVolumeRendering \
              -lvtkWidgets \
              -lvtkexoIIc \
              -lvtkexpat \
              -lvtkfreetype \
              -lvtkftgl \
              -lvtkjpeg \
              -lvtklibxml2 \
              -lvtkmetaio \
              -lvtkpng \
              -lvtksys \
              -lvtktiff \
              -lvtkverdict \
              -lvtkzlib \
              -ladvapi32
      }

   macx {
      INCLUDEPATH += /usr/local/include/vtk-5.0
      LIBS += -lvtkHybrid -lvtkWidgets -lQVTK
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

   win32 {
      QMAKE_PRE_LINK += & cd matc & nmake & cd ..
      LIBPATH += .\matc
      LIBS += -lmatc
   }

   macx {
      LIBPATH += /usr/local/lib
      LIBS += -lmatc
   }
}

#------------------------------------------------------------------------------
# OpenCASCADE (you may need to edit this):
#------------------------------------------------------------------------------
contains(DEFINES, OCC_63) {
   unix {
      DEFINES -= _OCC64     # Add when compiling on 64-bit platforms
      DEFINES +=  HAVE_CONFIG_H HAVE_IOSTREAM HAVE_FSTREAM HAVE_LIMITS_H
      INCLUDEPATH += /usr/local/OpenCASCADE/inc
      LIBPATH += /usr/local/OpenCASCADE/lib
      LIBS += -lTKBRep -lTKSTL -lTKSTEP -lTKIGES
   }
              
   win32 {
      # CONFIG += embed_manifest_dll windows
      DEFINES += OCC_63 WNT CSFDB
      INCLUDEPATH += $(CASROOT)/inc
      LIBPATH += $(CASROOT)/win32/lib
      LIBS += $(CASROOT)/win32/lib/TKBRep.lib \
              $(CASROOT)/win32/lib/TKernel.lib \
              $(CASROOT)/win32/lib/TKG2d.lib \
              $(CASROOT)/win32/lib/TKG3d.lib \
              $(CASROOT)/win32/lib/TKGeomAlgo.lib \
              $(CASROOT)/win32/lib/TKGeomBase.lib \
              $(CASROOT)/win32/lib/TKMath.lib \
              $(CASROOT)/win32/lib/TKMesh.lib \
              $(CASROOT)/win32/lib/TKShHealing.lib \
              $(CASROOT)/win32/lib/TKSTEP.lib \
              $(CASROOT)/win32/lib/TKSTEP209.lib \
              $(CASROOT)/win32/lib/TKSTEPAttr.lib \
              $(CASROOT)/win32/lib/TKSTEPBase.lib \
              $(CASROOT)/win32/lib/TKIGES.lib \
              $(CASROOT)/win32/lib/TKTopAlgo.lib \
              $(CASROOT)/win32/lib/TKXSBase.lib
   }

   macx {
      # Unsupported at the moment:
      DEFINES -= OCC_63
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

macx {
   RC_FILE = M3Dicon.icns
}

RC_FILE += ElmerGUI.rc   

#------------------------------------------------------------------------------
# END OF FILE
#------------------------------------------------------------------------------
