SUBDIRS=tk glaux camera graphics modules sico2elmer objects visuals tcl elements 

if NOTWINDOWS

bin_PROGRAMS=ElmerPost QueryGLXExt

ElmerPost_SOURCES=\
	ElmerPost.c \
	elmerpost.h \
	geometry.c \
	geometry.h \
	LUDecomp.c \
	second.c \
	cplusplus.cpp \
	teksti.c 

ElmerPost_LDADD=\
	./camera/libcamera.a \
	./elements/libelements.a \
	./modules/libmodule.a \
	./objects/libobjects.a \
	./visuals/libvisuals.a \
	./graphics/libgraphics.a \
	./glaux/libglaux.a \
	./tk/libtk.a

QueryGLXExt_SOURCES=QueryGLXExt.c 

all-local: screensave.so window.so
clean-local:
	rm *.so

EXTRA_DIST=screensave.c window.c

screensave.so: screensave.$(OBJEXT)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SH_LDFLAGS) -o $@ $< $(LIBS)

window.so: window.$(OBJEXT)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SH_LDFLAGS) -o $@ $< glaux/libglaux.a tk/libtk.a $(LIBS) 

else

# WINDOWS

bin_PROGRAMS=ElmerPost 

ElmerPost_SOURCES=\
	ElmerPost.c \
	elmerpost.h \
	geometry.c \
	geometry.h \
	LUDecomp.c \
	second.c \
	cplusplus.cpp 

ElmerPost_LDADD=\
	./camera/libcamera.a \
	./elements/libelements.a \
	./modules/libmodule.a \
	./objects/libobjects.a \
	./visuals/libvisuals.a \
	./graphics/libgraphics.a \
	-lglaux  \
	-lgdi32 \
	-luser32 \
	-ladvapi32
endif

ElmerPost_LDFLAGS=$(SH_EXPALL_FLAG)

ELMER_POST_DATADIR=$(prefix)/share/elmerpost

install-data-local:
	@echo "Instaling modules to ${ELMER_POST_DATADIR}/modules"

	mkdir -p $(ELMER_POST_DATADIR)/modules
	-cp screensave.so $(ELMER_POST_DATADIR)/modules 
	-cp window.so $(ELMER_POST_DATADIR)/modules

	@echo "Instaling help to ${ELMER_POST_DATADIR}/help"
	mkdir -p $(ELMER_POST_DATADIR)/help
	cp -r help/* $(ELMER_POST_DATADIR)/help
	rm -rf `find $(ELMER_POST_DATADIR)/help -name .svn`

	@echo "Instaling tcl scripts to ${ELMER_POST_DATADIR}/tcl"
	mkdir -p $(ELMER_POST_DATADIR)/tcl
	cp -r tcl/* $(ELMER_POST_DATADIR)/tcl
	rm -rf `find $(ELMER_POST_DATADIR)/tcl -name .svn`

	@echo "Instaling initialization scripts to ${ELMER_POST_DATADIR}/lib"
	mkdir -p $(ELMER_POST_DATADIR)/lib
	cp -r lib/* $(ELMER_POST_DATADIR)/lib
	rm -rf `find $(ELMER_POST_DATADIR)/lib -name .svn`

#
# Copy the help and tcl
#
dist-hook:
	cp -r tcl $(distdir)
	cp -r help $(distdir)
	cp -r lib $(distdir)
	rm -rf `find $(distdir)/ -name .svn`
	rm -rf `find $(distdir)/ -name CVS`
	rm -rf `find $(distdir)/ -name \#*`
	rm -rf `find $(distdir)/ -name *~`






