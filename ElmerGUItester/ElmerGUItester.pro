TEMPLATE = app
TARGET = ElmerGUItester
DEPENDPATH += . forms src
INCLUDEPATH += .

OBJECTS_DIR = tmp
UI_DIR = tmp
MOC_DIR = tmp

CONFIG += release

HEADERS += src/tester.h
FORMS += forms/mainform.ui
SOURCES += src/main.cpp src/tester.cpp
