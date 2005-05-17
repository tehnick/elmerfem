SUBDIRS=metis

bin_PROGRAMS = ElmerGrid

COMMON_SRCS =  \
	common.h \
	easymesh.h \
	femdef.h \
	femelmer.h \
	femfact.h \
	femfile.h \
	feminfo.h \
	femknot.h \
	femmesh.h \
	femsolve.h \
	femtools.h \
	femtypes.h \
	nrutil.h \
	common.c \
	easymesh.c \
	femelmer.c \
	femfact.c \
	femfile.c \
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
