TARGET = PythonQt
TEMPLATE = lib
win32:CONFIG += release

unix {
   INCLUDEPATH += /usr/include/python2.5
   LIBPATH += /usr/lib
   LIBS += -lpython2.5
}

win32 {
   DEFINES += ELMERGUI_MODIFICATIONS
   INCLUDEPATH += c:\PYTHON\Python-2.6.1\Include
   INCLUDEPATH += c:\PYTHON\Python-2.6.1\PC          # for pyconfog.h
   LIBPATH += c:\PYTHON\Python-2.6.1\PCbuild
   LIBS += -lpython26
}

INCLUDEPATH += src
CONFIG += qt
DEFINES += PYTHONQT_EXPORTS
DESTDIR = ./lib
DLLDESTDIR = ./lib
MOC_DIR = ./tmp
OBJECTS_DIR = ./tmp
RCC_DIR = ./tmp
UI_DIR = ./tmp
unix:QMAKE_CXXFLAGS = -w

HEADERS += \
  src/PythonQt.h \
  src/PythonQtStdDecorators.h \
  src/PythonQtClassInfo.h \
  src/PythonQtImporter.h \
  src/PythonQtObjectPtr.h \
  src/PythonQtSlot.h \
  src/PythonQtStdOut.h \
  src/PythonQtMisc.h \
  src/PythonQtMethodInfo.h \
  src/PythonQtImportFileInterface.h \
  src/PythonQtConversion.h \
  src/PythonQtSignalReceiver.h \
  src/PythonQtWrapper.h \
  src/PythonQtMetaObjectWrapper.h \
  src/PythonQtCppWrapperFactory.h \
  src/PythonQtVariants.h \
  src/PythonQtVariantWrapper.h \
  src/wrapper/PythonQtWrappedVariants.h \
  src/gui/PythonQtScriptingConsole.h    \
  src/PythonQtSystem.h
  
SOURCES += \
  src/PythonQtStdDecorators.cpp \
  src/PythonQt.cpp \
  src/PythonQtClassInfo.cpp \
  src/PythonQtImporter.cpp \
  src/PythonQtObjectPtr.cpp \
  src/PythonQtStdOut.cpp \
  src/PythonQtSlot.cpp \
  src/PythonQtMisc.cpp \
  src/PythonQtMethodInfo.cpp \
  src/PythonQtConversion.cpp \
  src/PythonQtSignalReceiver.cpp \
  src/PythonQtVariants.cpp \
  src/PythonQtVariantWrapper.cpp \
  src/PythonQtWrapper.cpp \
  src/PythonQtMetaObjectWrapper.cpp \
  src/gui/PythonQtScriptingConsole.cpp
