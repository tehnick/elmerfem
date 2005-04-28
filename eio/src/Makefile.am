## Makefile.am for hutiter/src

INCLUDES = \
        -I$(top_builddir)/include       \
        -I$(top_srcdir)/include         

COMMON_SRCS =  \
	eio-config.h \
	EIOConstraintAgent.cpp \
	EIODualMeshAgent.cpp \
	EIOGeometryAgent.cpp \
	EIOMeshAgent.cpp \
	EIOModelDataAgent.cpp \
	EIOModelManager.cpp \
	EIOPartWriter.cpp \
	EIOSolverAgent.cpp 

lib_LIBRARIES = libeioc.a libeiof.a
libeioc_a_SOURCES = $(COMMON_SRCS) eio_api_c.cpp
libeiof_a_SOURCES = $(COMMON_SRCS) eio_api_f.cpp

