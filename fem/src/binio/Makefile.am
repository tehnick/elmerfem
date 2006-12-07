SUBDIRS = test

#AM_CFLAGS=-Wall -ansi -pedantic -O3
AM_FCFLAGS=@modput_opt@..

noinst_LIBRARIES = libbinio.a
libbinio_a_SOURCES = binio.c biniomod.f90

if UPPERCASEMODS
install-exec-local:
	$(INSTALL) ../BINIO.mod $(includedir)
else
install-exec-local:
	$(INSTALL) ../binio.mod $(includedir)
endif
