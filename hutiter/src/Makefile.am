## Makefile.am for hutiter/src

INCLUDES = \
	-I$(top_builddir)/include       \
	-I$(top_srcdir)/include         

COMMON_SRCS =  \
	huti_aux_C.F90 \
	huti_aux_D.F90 \
	huti_aux_S.F90 \
	huti_aux_Z.F90 \
	huti_bicgstab_2_C.F90 \
	huti_bicgstab_2_D.F90 \
	huti_bicgstab_2_S.F90 \
	huti_bicgstab_2_Z.F90 \
	huti_bicgstab_C.F90 \
	huti_bicgstab_D.F90 \
	huti_bicgstab_S.F90 \
	huti_bicgstab_Z.F90 \
	huti_cg_C.F90 \
	huti_cg_D.F90 \
	huti_cgs_C.F90 \
	huti_cgs_D.F90 \
	huti_cg_S.F90 \
	huti_cgs_S.F90 \
	huti_cgs_Z.F90 \
	huti_cg_Z.F90 \
	huti_fdefs.h \
	huti_gmres_C.F90 \
	huti_gmres_D.F90 \
	huti_gmres_S.F90 \
	huti_gmres_Z.F90 \
	huti_intdefs.h \
	huti_qmr_C.F90 \
	huti_qmr_D.F90 \
	huti_qmr_S.F90 \
	huti_qmr_Z.F90 \
	huti_tfqmr_C.F90 \
	huti_tfqmr_D.F90 \
	huti_tfqmr_S.F90 \
	huti_defs.h \
	huti_main.c \
	huti_sfe_c_C.\
	huti_sfe_c_D.c \
	huti_sfe_c_S.c \
	huti_sfe_c_Z.c \
	huti_tfqmr_Z.F90

lib_LIBRARIES = libhuti.a
libhuti_a_SOURCES = $(COMMON_SRCS)

#################################################################################
# If sources are missing generate them with the preprocessor.
# this is the section to watch out for, when compiling on more exotic (non-linux) 
# platforms.
#
$(COMMON_SRCS): preprocess

HUTI_CPP="cpp"
PRECS	=	S D C Z
precdf90=huti_cg huti_tfqmr huti_cgs huti_qmr huti_bicgstab \
	 huti_gmres huti_bicgstab_2 huti_aux
precdc=huti_sfe_c
HUTI_PPFLAGS = -traditional -P

preprocess:
	@echo
	@echo 'Generating source files from *.src files, WARNING, only known to work on Linux :)'
	@echo
#
# First make the precision dependent Fortran90 files
#
	@if [ "$(precdf90)X" != "X" ]; then \
	  for p in $(PRECS); do \
	    for pd in $(precdf90); do \
		$(RM) $${pd}_$${p}.F90; \
		echo $(HUTI_CPP) $(HUTI_PPFLAGS) -D$${p}_PRE $${pd}.src $${pd}_$${p}.F90; \
		$(HUTI_CPP) $(HUTI_PPFLAGS) -D$${p}_PRE $${pd}.src $${pd}_$${p}.F90; \
	    done; \
	  done; \
	fi 
#
# Third make the precision dependent C files
#
	@if [ "$(precdc)X" != "X" ]; then \
	  for p in $(PRECS); do \
	    for pd in $(precdc); do \
		$(RM) $${pd}_$${p}.c; \
		$(HUTI_CPP)  $(HUTI_PPFLAGS) -D$${p}_PRE $${pd}.src $${pd}_$${p}.c; \
	    done; \
	  done; \
	fi 
