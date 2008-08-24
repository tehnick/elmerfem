# Mesh3D.pro for VC 2008 (with OCC)

TEMPLATE = app
TARGET = Mesh3D
DEPENDPATH += . cad forms plugins
INCLUDEPATH += .

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
HEADERS += bodypropertyeditor.h \
           boundarydivision.h \
           boundarypropertyeditor.h \
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
           sifgenerator.h \
           sifwindow.h \
           solverparameters.h \
           summaryeditor.h \
           ui_bcpropertyeditor.h \
           ui_matpropertyeditor.h \
           ui_pdepropertyeditor.h \
           ui_propertyeditor.h \
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
         forms/meshcontrol.ui \
         forms/solverparameters.ui \
         forms/summaryeditor.ui
SOURCES += bodypropertyeditor.cpp \
           boundarydivision.cpp \
           boundarypropertyeditor.cpp \
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
           sifgenerator.cpp \
           sifwindow.cpp \
           solverparameters.cpp \
           summaryeditor.cpp \
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
RESOURCES += Mesh3D.qrc
RC_FILE += Mesh3D.rc   
