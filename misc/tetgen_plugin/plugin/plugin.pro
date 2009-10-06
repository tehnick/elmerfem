ELMER_HOME=$$(ELMER_HOME)

isEmpty(ELMER_HOME) {
   error("ELMER_HOME is undefined")
}

TEMPLATE = lib
TARGET = libtet
DEPENDPATH += .
INCLUDEPATH += .
CONFIG += release dll warn_off
QMAKE_CXXFLAGS += -DTETLIBRARY
HEADERS += tetgen.h
SOURCES += predicates.cxx tetgen.cxx ElmerAPI.cpp
target.path = $${ELMER_HOME}/lib
INSTALLS += target
