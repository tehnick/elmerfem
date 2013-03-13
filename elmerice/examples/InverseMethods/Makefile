SrcDir=./src
ExeDir=./Executables


_SOURCES=USF_Init.f90 
SOURCES = $(patsubst %,$(SrcDir)/%,$(_SOURCES))

_SOLVERS=$(_SOURCES:.f90=.so)
SOLVERS=$(patsubst %,$(ExeDir)/%,$(_SOLVERS)) 

all:$(SOLVERS)

Mesh: 
	ElmerGrid  1 2 mesh2D -metis $(np) 4

$(ExeDir)/%.so: $(SrcDir)/%.f90
	elmerf90 $< -o $@


clean:
	rm -f *~
	rm -f *.dat* 
	rm -f *.out
	rm -f Executables/*
	rm -f -r mesh2D
	rm -f ELMERSOLVER_STARTINFO
