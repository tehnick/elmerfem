# poisson equation by discontinuous galerkin
#
run:
#	$(ELMER_HOME)/bin/ElmerGrid 1 2 square
	$(F90) -c PoissonDG.f90
	$(LD) -o PoissonDG$(SHL_EXT) PoissonDG.o
	$(ELMER_HOME)/bin/ElmerSolver

clean:
	/bin/rm test.log temp.log mon.out dg.ep PoissonDG$(SHL_EXT) PoissonDG.o
#	/bin/rm -r square
