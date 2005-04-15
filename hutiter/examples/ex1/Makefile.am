INCLUDES = \
	-I$(top_builddir)/include       \
	-I$(top_srcdir)/include         

check_PROGRAMS = hutiex

hutiex_SOURCES = \
	globals_module.f90 \
	dummymain.c \
	huti-ex.F90 

hutiex_LDADD = \
	../../src/libhuti.a




TESTS = testmat.script
