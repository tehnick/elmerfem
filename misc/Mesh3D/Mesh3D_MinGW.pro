# Mesh3D.pro for MinGW

TEMPLATE = app
TARGET = Mesh3D
DEPENDPATH += . forms plugins
INCLUDEPATH += .

LIBS += -lpsapi

# QT
QT += opengl xml script
CONFIG += uitools

# QWT
INCLUDEPATH += /c/Qwt-5.0.2/include
LIBPATH += /c/Qwt-5.0.2/lib
LIBS += -lqwt5

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
           ui_bcpropertyeditor.h \
           ui_matpropertyeditor.h \
           ui_pdepropertyeditor.h \
           ui_propertyeditor.h \
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
