SUBDIRS=tk glaux camera graphics modules sico2elmer objects visuals tcl elements 

bin_PROGRAMS=elmerpost QueryGLXExt

elmerpost_SOURCES=\
	ElmerPost.c \
	elmerpost.h \
	geometry.c \
	geometry.h \
	LUDecomp.c \
	screensave.c \
	second.c \
	teksti.c \
	window.c 

elmerpost_LDADD=\
	./camera/libcamera.a \
	./elements/libelements.a \
	./modules/libmodule.a \
	./objects/libobjects.a \
	./visuals/libvisuals.a \
	./graphics/libgraphics.a \
	./glaux/libglaux.a \
	./tk/libtk.a

QueryGLXExt_SOURCES=QueryGLXExt.c 

