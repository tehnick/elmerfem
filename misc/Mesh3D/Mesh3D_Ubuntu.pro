# Mesh3D.pro for 64bit Ubuntu

TEMPLATE = app
TARGET = Mesh3D
DEPENDPATH += . cad forms plugins tmp\rcc\debug_shared tmp\rcc\release_shared
INCLUDEPATH += .

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
HEADERS += bodypropertyeditor.h \
           boundarydivision.h \
           boundarypropertyeditor.h \
           checkmpi.h \
           convergenceview.h \
           dynamiceditor.h \
           edfeditor.h \
           generalsetup.h \
           glcontrol.h \
           glwidget.h \
           helpers.h \
           mainwindow.h \
           maxlimits.h \
           meshcontrol.h \
           meshingthread.h \
           meshtype.h \
           meshutils.h \
           parallel.h \
           sifgenerator.h \
           sifwindow.h \
           solverparameters.h \
           summaryeditor.h \
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
SOURCES += bodypropertyeditor.cpp \
           boundarydivision.cpp \
           boundarypropertyeditor.cpp \
           checkmpi.cpp \
           convergenceview.cpp \
           dynamiceditor.cpp \
           edfeditor.cpp \
           generalsetup.cpp \
           glcontrol.cpp \
           glwidget.cpp \
           helpers.cpp \
           main.cpp \
           mainwindow.cpp \
           meshcontrol.cpp \
           meshingthread.cpp \
           meshutils.cpp \
           parallel.cpp \
           sifgenerator.cpp \
           sifwindow.cpp \
           solverparameters.cpp \
           summaryeditor.cpp \
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
RESOURCES += Mesh3D.qrc
RC_FILE += Mesh3D.rc   
