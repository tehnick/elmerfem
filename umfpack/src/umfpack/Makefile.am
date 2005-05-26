SUBDIRS=include

#-------------------------------------------------------------------------------
# source files
#-------------------------------------------------------------------------------
include make.dep

EXTRA_DIST = $(UMFPACK_ORIG_SOURCES) umf4_f77wrapper.c umf4_f77zwrapper.c
UMFPACK_SOURCES = umfpack_timer.c umfpack_tictoc.c

INCLUDES = \
	-I./include/       \
	-I$(top_srcdir)/src/amd/include     \
	-I$(top_srcdir)/src/amd \
	-I.         

lib_LIBRARIES = libumfpack.a

libumfpack_a_SOURCES = $(UMFPACK_SOURCES)
nodist_libumfpack_a_SOURCES = $(UMFPACK_CPP_SOURCES) umf4_f_f77wrapper.c umf4_f_f77zwrapper.c

clean-local:
	-rm -f $(UMFPACK_CPP_SOURCES)

#
# umfpack fortran wrappers
#
if USE_64BIT_ARCH

umf4_f_f77wrapper.c: umf4_f77wrapper.c
	$(CPP) $(INCLUDES) umf4_f77wrapper.c > umf4_f_f77wrapper.c

umf4_f_f77zwrapper.c: umf4_f77zwrapper.c
	$(CPP) $(INCLUDES) umf4_f77zwrapper.c > umf4_f_f77zwrapper.c

else

umf4_f_f77wrapper.c: umf4_f77wrapper.c
	$(CPP) $(INCLUDES) -DLONG umf4_f77wrapper.c > umf4_f_f77wrapper.c

umf4_f_f77zwrapper.c: umf4_f77zwrapper.c
	$(CPP) $(INCLUDES) -DLONG umf4_f77zwrapper.c > umf4_f_f77zwrapper.c

endif

