TARGET = matc
TEMPLATE = lib
CONFIG -= qt debug
CONFIG += staticlib release
QMAKE_CFLAGS_WARN_ON = -w
DESTDIR = lib
OBJECTS_DIR = obj
win32: DEFINES = WIN32 _CRT_SECURE_NO_WARNINGS 
INCLUDEPATH = . src
SOURCES = src/eig.c \
          src/funcs.c \
          src/lu.c \
          src/oper.c \
          src/str.c \
          src/eval.c \
          src/jacobi.c \
          src/matc.c \
          src/optim.c \
          src/urand.c \
          src/files.c \
          src/lists.c \
          src/matrix.c \
          src/parser.c \
          src/variable.c \
          src/gra.c \
          src/gra_com.c \
          src/clip.c \
          src/c3d.c \
          src/dri_ps.c \
          src/error.c
