## Makefile.am for meshgen2d/src
SUBDIRS=include

INCLUDES = \
        -I./include       

bin_PROGRAMS = ElmerMesh2D Mesh2D

COMMON_SRCS =  \
	BGGridMesh.cpp \
	BGTriangleMesh.cpp \
	BGVertex.cpp \
	Body.cpp \
	BoundaryElement.cpp \
	BoundaryLayer.cpp \
	Connect.cpp \
	Element.cpp \
	GeometryEdge.cpp \
	main.cpp \
	Mesh.cpp \
	MeshNode.cpp \
	MeshParser.cpp \
	Node.cpp \
	PQ.cpp \
	QuadElement.cpp \
	QuadLayer.cpp \
	SSMFVoronoiSegment.cpp \
	SSSFVoronoiSegment.cpp \
	TriangleElement.cpp \
	TriangleNELayer.cpp \
	Vertex.cpp \
	VoronoiSegment.cpp \
	VoronoiVertex.cpp \
	VSVertex.cpp 

if HAVE_WINDRES

.rc.o:
	windres $^ -o $@
%.o : %.rc
	windres $^ -o $@

ElmerMesh2D_SOURCES=$(COMMON_SRCS) ElmerMesh2D-res.rc
Mesh2D_SOURCES=$(COMMON_SRCS) Mesh2D-res.rc

else

ElmerMesh2D_SOURCES=$(COMMON_SRCS)
Mesh2D_SOURCES=$(COMMON_SRCS)

endif

ElmerMesh2D_LDADD = \
	$(MESHGEN2D_LIBS)
Mesh2D_LDADD = \
	$(MESHGEN2D_LIBS)
