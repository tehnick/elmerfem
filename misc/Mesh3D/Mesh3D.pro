######################################################################
# Automatically generated by qmake (2.01a) su 23. maalis 20:00:03 2008
######################################################################

TEMPLATE = app
TARGET = 
DEPENDPATH += . forms plugins tmp\rcc\release_shared
INCLUDEPATH += .
QT += opengl

# Input
HEADERS += bcpropertyeditor.h \
           boundarydivision.h \
           glwidget.h \
           helpers.h \
           mainwindow.h \
           matpropertyeditor.h \
           meshcontrol.h \
           meshingthread.h \
           meshtype.h \
           meshutils.h \
           pdepropertyeditor.h \
           sifwindow.h \
           solverparameters.h \
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
FORMS += forms/bcpropertyeditor.ui \
         forms/boundarydivision.ui \
         forms/matpropertyeditor.ui \
         forms/meshcontrol.ui \
         forms/pdepropertyeditor.ui \
         forms/solverparameters.ui
SOURCES += bcpropertyeditor.cpp \
           boundarydivision.cpp \
           glwidget.cpp \
           helpers.cpp \
           main.cpp \
           mainwindow.cpp \
           matpropertyeditor.cpp \
           meshcontrol.cpp \
           meshingthread.cpp \
           meshutils.cpp \
           pdepropertyeditor.cpp \
           sifedit_backup.cpp \
           sifwindow.cpp \
           solverparameters.cpp \
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
