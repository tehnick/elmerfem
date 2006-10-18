SUBDIRS=.

if R
SUBDIRS+=R
endif

if MATLAB
SUBDIRS+=matlab
endif

bin_PROGRAMS = ElmerParam
ElmerParam_SOURCES = ElmerParam.f90
if MATC
ElmerParam_LDADD = libelmerparamf.a libelmerparam.a -L@prefix@/lib -lmatc
else
ElmerParam_LDADD = libelmerparamf.a libelmerparam.a
endif

lib_LIBRARIES = libelmerparam.a libelmerparamf.a

libelmerparam_a_SOURCES = elmerparam.c \
	elmerparam.h \
	global.h \
	modelline.c \
	modelline.h \
	dynarray.c \
	dynarray.h
libelmerparamf_a_SOURCES = elmerparamf.f90

include_HEADERS = elmerparam.h

if UPPERCASEMODS
nodist_include_HEADERS = ELMERPARAM.mod
else
nodist_include_HEADERS = elmerparam.mod
endif
MOSTLYCLEANFILES = $(nodist_include_HEADERS)
