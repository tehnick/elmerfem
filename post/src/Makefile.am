SUBDIRS=tk glaux camera graphics modules sico2elmer objects visuals tcl elements 

bin_PROGRAMS=elmerpost QueryGLXExt

elmerpost_SOURCES=\
	ElmerPost.c \
	elmerpost.h \
	geometry.c \
	geometry.h \
	LUDecomp.c \
	second.c \
	cplusplus.cpp \
	teksti.c 


elmerpost_LDADD=\
	./camera/libcamera.a \
	./elements/libelements.a \
	./modules/libmodule.a \
	./objects/libobjects.a \
	./visuals/libvisuals.a \
	./graphics/libgraphics.a \
	./glaux/libglaux.a \
	./tk/libtk.a

elmerpost_LDFLAGS=$(SH_EXPALL_FLAG)

QueryGLXExt_SOURCES=QueryGLXExt.c 

all-local: screensave.so window.so
clean-local:
	rm *.so

screensave.so: screensave.$(OBJEXT)
	$(CC) $(CFLAGS) @SH_LDFLAGS@ -o $@ $< $(LIBS)

window.so: window.$(OBJEXT)
	$(CC) $(CFLAGS) @SH_LDFLAGS@ -o $@ $< glaux/libglaux.a tk/libtk.a $(LIBS) 

ELMER_POST_DATADIR=$(prefix)/share/elmerpost
install-data-local:
	@echo "Instaling modules to ${ELMER_POST_DATADIR}/modules"

	mkdir -p $(ELMER_POST_DATADIR)/modules
	cp screensave.so $(ELMER_POST_DATADIR)/modules 
	cp window.so $(ELMER_POST_DATADIR)/modules

	@echo "Instaling help to ${ELMER_POST_DATADIR}/help"
	mkdir -p $(ELMER_POST_DATADIR)/help
	cp -r help/* $(ELMER_POST_DATADIR)/help

	@echo "Instaling tcl scripts to ${ELMER_POST_DATADIR}/tcl"
	mkdir -p $(ELMER_POST_DATADIR)/tcl
	cp -r tcl/* $(ELMER_POST_DATADIR)/tcl

	@echo "Instaling initialization scripts to ${ELMER_POST_DATADIR}/lib"
	mkdir -p $(ELMER_POST_DATADIR)/lib
	cp -r lib/* $(ELMER_POST_DATADIR)/lib








