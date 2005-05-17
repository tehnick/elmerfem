# A model for heat and moisture behaviour of bentonite clay
#
run:
	$(F90) -c BentoniteSolver.f90
	$(LD) -o BentoniteSolver$(SHL_EXT) BentoniteSolver.o $(LIBS)
	$(ELMER_HOME)/bin/Solver


clean:
	/bin/rm test.log temp.log mon.out BentoniteSolver$(SHL_EXT) BentoniteSolver.o so_locations
