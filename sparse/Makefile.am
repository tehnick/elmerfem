include_HEADERS = \
	spconfig.h  \
	spdefs.h  \
	spmatrix.h


COMMON_SRCS =  \
        spallocate.c \
        spbuild.c \
        spconfig.h \
        spdefs.h \
        spfactor.c \
        spfortran.c \
        spmatrix.h \
        spoutput.c \
        spsolve.c \
        sputils.c 

lib_LIBRARIES = libsparse.a
libsparse_a_SOURCES = $(COMMON_SRCS)

bin_PROGRAMS = sptest

sptest_SOURCES = $(COMMON_SRCS) sptest.c




