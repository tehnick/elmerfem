SUBDIRS=metis

bin_PROGRAMS = ElmerGrid

COMMON_SRCS =  \
	common.h \
	femdef.h \
	femelmer.h \
	femfact.h \
	femfilein.h \
	femfileout.h \
	feminfo.h \
	femknot.h \
	femmesh.h \
	femsolve.h \
	femtools.h \
	femtypes.h \
	nrutil.h \
	common.c \
	femelmer.c \
	femfact.c \
	femfilein.c \
	femfileout.c \
	feminfo.c \
	femknot.c \
	femmesh.c \
	fempre.c \
	femsolve.c \
	femtools.c \
	nrutil.c 

ElmerGrid_SOURCES = $(COMMON_SRCS) 

ElmerGrid_LDADD = \
	metis/libmetis.a
