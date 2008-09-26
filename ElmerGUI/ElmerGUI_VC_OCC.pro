# ElmerGUI.pro for VC 2008 (with OCC)

TEMPLATE = app
TARGET = ElmerGUI
DEPENDPATH += . src cad forms plugins
INCLUDEPATH += .
MOC_DIR = ./tmp
OBJECTS_DIR = ./tmp
RCC_DIR = ./tmp
UI_DIR = ./tmp

# QT
QT += opengl xml script
CONFIG += uitools

# QWT
INCLUDEPATH += c:/qwt-5.1.1/include
LIBPATH += c:/qwt-5.1.1/lib
LIBS += -lqwt5

#OCC
INCLUDEPATH += $(CASROOT)/inc
LIBPATH += $(CASROOT)/win32/lib
LIBS += $(CASROOT)/win32/lib/BinLPlugin.lib \
		$(CASROOT)/win32/lib/BinPlugin.lib \
		$(CASROOT)/win32/lib/BinXCAFPlugin.lib \
		$(CASROOT)/win32/lib/FWOSPlugin.lib \
		$(CASROOT)/win32/lib/mscmd.lib \
		$(CASROOT)/win32/lib/PTKernel.lib \
		$(CASROOT)/win32/lib/StdLPlugin.lib \
		$(CASROOT)/win32/lib/StdPlugin.lib \
		$(CASROOT)/win32/lib/TKAdvTools.lib \
		$(CASROOT)/win32/lib/TKBin.lib \
		$(CASROOT)/win32/lib/TKBinL.lib \
		$(CASROOT)/win32/lib/TKBinXCAF.lib \
		$(CASROOT)/win32/lib/TKBO.lib \
		$(CASROOT)/win32/lib/TKBool.lib \
		$(CASROOT)/win32/lib/TKBRep.lib \
		$(CASROOT)/win32/lib/TKCAF.lib \
		$(CASROOT)/win32/lib/TKCDF.lib \
		$(CASROOT)/win32/lib/TKCDLFront.lib \
		$(CASROOT)/win32/lib/TKCPPClient.lib \
		$(CASROOT)/win32/lib/TKCPPExt.lib \
		$(CASROOT)/win32/lib/TKCPPIntExt.lib \
		$(CASROOT)/win32/lib/TKCPPJini.lib \
		$(CASROOT)/win32/lib/TKCSFDBSchema.lib \
		$(CASROOT)/win32/lib/TKDCAF.lib \
		$(CASROOT)/win32/lib/TKDraw.lib \
		$(CASROOT)/win32/lib/TKernel.lib \
		$(CASROOT)/win32/lib/TKFeat.lib \
		$(CASROOT)/win32/lib/TKFillet.lib \
		$(CASROOT)/win32/lib/TKG2d.lib \
		$(CASROOT)/win32/lib/TKG3d.lib \
		$(CASROOT)/win32/lib/TKGeomAlgo.lib \
		$(CASROOT)/win32/lib/TKGeomBase.lib \
		$(CASROOT)/win32/lib/TKHLR.lib \
		$(CASROOT)/win32/lib/TKIDLFront.lib \
		$(CASROOT)/win32/lib/TKIGES.lib \
		$(CASROOT)/win32/lib/TKjcas.lib \
		$(CASROOT)/win32/lib/TKLCAF.lib \
		$(CASROOT)/win32/lib/TKMath.lib \
		$(CASROOT)/win32/lib/TKMesh.lib \
		$(CASROOT)/win32/lib/TKMeshVS.lib \
		$(CASROOT)/win32/lib/TKOffset.lib \
		$(CASROOT)/win32/lib/TKOpenGl.lib \
		$(CASROOT)/win32/lib/TKPCAF.lib \
		$(CASROOT)/win32/lib/TKPLCAF.lib \
		$(CASROOT)/win32/lib/TKPrim.lib \
		$(CASROOT)/win32/lib/TKPShape.lib \
		$(CASROOT)/win32/lib/TKService.lib \
		$(CASROOT)/win32/lib/TKShapeSchema.lib \
		$(CASROOT)/win32/lib/TKShHealing.lib \
		$(CASROOT)/win32/lib/TKStdLSchema.lib \
		$(CASROOT)/win32/lib/TKStdSchema.lib \
		$(CASROOT)/win32/lib/TKSTEP.lib \
		$(CASROOT)/win32/lib/TKSTEP209.lib \
		$(CASROOT)/win32/lib/TKSTEPAttr.lib \
		$(CASROOT)/win32/lib/TKSTEPBase.lib \
		$(CASROOT)/win32/lib/TKSTL.lib \
		$(CASROOT)/win32/lib/TKTCPPExt.lib \
		$(CASROOT)/win32/lib/TKTopAlgo.lib \
		$(CASROOT)/win32/lib/TKTopTest.lib \
		$(CASROOT)/win32/lib/TKV2d.lib \
		$(CASROOT)/win32/lib/TKV3d.lib \
		$(CASROOT)/win32/lib/TKViewerTest.lib \
		$(CASROOT)/win32/lib/TKVRML.lib \
		$(CASROOT)/win32/lib/TKXCAF.lib \
		$(CASROOT)/win32/lib/TKXCAFSchema.lib \
		$(CASROOT)/win32/lib/TKXDEDRAW.lib \
		$(CASROOT)/win32/lib/TKXDEIGES.lib \
		$(CASROOT)/win32/lib/TKXDESTEP.lib \
		$(CASROOT)/win32/lib/TKXml.lib \
		$(CASROOT)/win32/lib/TKXmlL.lib \
		$(CASROOT)/win32/lib/TKXmlXCAF.lib \
		$(CASROOT)/win32/lib/TKXSBase.lib \
		$(CASROOT)/win32/lib/TKXSDRAW.lib \
		$(CASROOT)/win32/lib/XCAFPlugin.lib \
		$(CASROOT)/win32/lib/XmlLPlugin.lib \
		$(CASROOT)/win32/lib/XmlPlugin.lib \
		$(CASROOT)/win32/lib/XmlXCAFPlugin.lib

DEFINES += WNT OCC62 CSFDB QOCC_STATIC

CONFIG += qt opengl embed_manifest_dll windows

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
           cad/cadview.h  \
           cad/qoccinternal.h \
           cad/qoccviewwidget.h \
           cad/qocc.h \
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
         forms/materiallibrary.ui \
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
           src/materiallibrary.cpp \
           src/maxlimits.cpp \
           src/meshcontrol.cpp \
           src/meshtype.cpp \
           src/meshingthread.cpp \
           src/meshutils.cpp \
           src/operation.cpp \
           src/parallel.cpp \
           src/projectio.cpp \
           src/sifgenerator.cpp \
           src/sifwindow.cpp \
           src/solverparameters.cpp \
           src/summaryeditor.cpp \
           cad/cadview.cpp \
           cad/qoccviewWidget.cpp  \
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
