TEMPLATE = app
TARGET = ElmerClips
DEPENDPATH += . src
INCLUDEPATH += . src

win32 {
   INCLUDEPATH += C:/Stuff/Elmerfem/include src/win32
   QMAKE_LIBDIR += C:/Stuff/Elmerfem/bin
   LIBS += -lavcodec -lavutil -lswscale
   DESTDIR = ElmerClips
}

unix {
  LIBS += -lavcodec -lavcore -lavutil -lswscale
}

CONFIG += release

HEADERS += src/preview.h src/encoder.h
SOURCES += src/main.cpp src/preview.cpp src/encoder.cpp
RESOURCES += ElmerClips.qrc
RC_FILE += ElmerClips.rc
