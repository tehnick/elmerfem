# ElmerGUI.pro for 64bit Ubuntu with OCC62
# If you do not have OCC62 installed, use ElmerGUI_Linux.pro instead

TEMPLATE = app
TARGET = ElmerGUI
DEPENDPATH += . src cad forms plugins
INCLUDEPATH += .
MOC_DIR = ./tmp
OBJECTS_DIR = ./tmp
RCC_DIR = ./tmp
UI_DIR = ./tmp

QMAKE_CXXFLAGS = -O2
QMAKE_CXXFLAGS_WARN_ON = 

# QT
#----
QT += opengl xml script
CONFIG += uitools
CONFIG += release 
CONFIG -= debug

# QWT
#-----
INCLUDEPATH += /usr/include/qwt-qt4
LIBS += -lqwt-qt4

# OPEN CASCADE (remove this section for not compiling against OCC62)
#--------------------------------------------------------------------
#
DEFINES += OCC62
#
DEFINES +=  _OCC64 HAVE_CONFIG_H HAVE_IOSTREAM HAVE_FSTREAM HAVE_LIMITS_H
INCLUDEPATH += /usr/include/opencascade
LIBS += -lBinLPlugin \
	-lBinPlugin \
	-lBinXCAFPlugin \
	-lFWOSPlugin \
	-lmscmd \
	-lPTKernel \
	-lStdLPlugin \
	-lStdPlugin \
	-lTKAdvTools \
	-lTKBin \
	-lTKBinL \
	-lTKBinXCAF \
	-lTKBO \
	-lTKBool \
	-lTKBRep \
	-lTKCAF \
	-lTKCDF \
	-lTKCDLFront \
	-lTKCPPClient \
	-lTKCPPExt \
	-lTKCPPIntExt \
	-lTKCPPJini \
	-lTKCSFDBSchema \
	-lTKDraw \
	-lTKernel \
	-lTKFeat \
	-lTKFillet \
	-lTKG2d \
	-lTKG3d \
	-lTKGeomAlgo \
	-lTKGeomBase \
	-lTKHLR \
	-lTKIDLFront \
	-lTKIGES \
	-lTKLCAF \
	-lTKMath \
	-lTKMesh \
	-lTKMeshVS \
	-lTKOffset \
	-lTKOpenGl \
	-lTKPCAF \
	-lTKPLCAF \
	-lTKPrim \
	-lTKPShape \
	-lTKService \
	-lTKShapeSchema \
	-lTKShHealing \
	-lTKStdLSchema \
	-lTKStdSchema \
	-lTKSTEP \
	-lTKSTEPAttr \
	-lTKSTEPBase \
	-lTKSTL \
	-lTKTCPPExt \
	-lTKTopAlgo \
	-lTKV2d \
	-lTKV3d \
	-lTKVRML \
	-lTKXCAF \
	-lTKXCAFSchema \
	-lTKXDEIGES \
	-lTKXDESTEP \
	-lTKXml \
	-lTKXmlL \
	-lTKXmlXCAF \
	-lTKXSBase \
	-lXCAFPlugin \
	-lXmlLPlugin \
	-lXmlPlugin \
	-lXmlXCAFPlugin \
	-lXmu

# Input
HEADERS += src/bodypropertyeditor.h \
           src/boundarydivision.h \
           src/boundarypropertyeditor.h \
           src/checkmpi.h \
           src/convergenceview.h \
           src/dynamiceditor.h \
           src/edfeditor.h \
           src/egini.h \
           src/generalsetup.h \
           src/glcontrol.h \
           src/glwidget.h \
           src/helpers.h \
           src/mainwindow.h \
           src/maxlimits.h \
           src/meshcontrol.h \
           src/meshingthread.h \
           src/meshtype.h \
           src/meshutils.h \
           src/operation.h \
           src/parallel.h \
           src/sifgenerator.h \
           src/sifwindow.h \
           src/solverparameters.h \
           src/summaryeditor.h \
           cad/cadview.h \
           cad/qocc.h \
           cad/qoccinternal.h \
           cad/qoccviewwidget.h \
           cad/qoccviewercontext.h \
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
         forms/meshcontrol.ui \
         forms/parallel.ui \
         forms/solverparameters.ui \
         forms/summaryeditor.ui
SOURCES += src/bodypropertyeditor.cpp \
           src/boundarydivision.cpp \
           src/boundarypropertyeditor.cpp \
           src/checkmpi.cpp \
           src/convergenceview.cpp \
           src/dynamiceditor.cpp \
           src/edfeditor.cpp \
           src/egini.cpp \
           src/generalsetup.cpp \
           src/glcontrol.cpp \
           src/glwidget.cpp \
           src/helpers.cpp \
           src/main.cpp \
           src/mainwindow.cpp \
           src/meshcontrol.cpp \
           src/meshingthread.cpp \
           src/meshutils.cpp \
           src/operation.cpp \
           src/parallel.cpp \
           src/sifgenerator.cpp \
           src/sifwindow.cpp \
           src/solverparameters.cpp \
           src/summaryeditor.cpp \
           cad/cadview.cpp \
           cad/qoccviewWidget.cpp \
           cad/qoccviewercontext.cpp \
           plugins/egconvert.cpp \
           plugins/egmain.cpp \
           plugins/egmesh.cpp \
           plugins/egnative.cpp \
           plugins/egutils.cpp \
           plugins/elmergrid_api.cpp \
           plugins/nglib_api.cpp \
           plugins/tetlib_api.cpp
RESOURCES += ElmerGUI.qrc
RC_FILE += ElmerGUI.rc   
