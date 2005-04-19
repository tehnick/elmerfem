## Makefile.am for hutiter/src

INCLUDES = \
        -I$(top_builddir)/include       \
        -I$(top_srcdir)/include         

COMMON_SRCS =  \
	eio_api_f.cpp \
	EIOConstraintAgent.cpp \
	EIODualMeshAgent.cpp \
	EIOGeometryAgent.cpp \
	EIOMeshAgent.cpp \
	EIOModelDataAgent.cpp \
	EIOModelManager.cpp \
	EIOPartWriter.cpp \
	EIOSolverAgent.cpp 

lib_LIBRARIES = libeio.a
libeio_a_SOURCES = $(COMMON_SRCS)
