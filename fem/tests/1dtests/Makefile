# A poisson equation solver and several 1d test cases
#
run:
	$(F90) -c Poisson.f90 Source.f90
	$(LD) -o Poisson$(SHL_EXT) Poisson.obj $(LIBS)
	$(LD) -o Source$(SHL_EXT) Source.obj $(LIBS)
	$(ELMER_GRID) 1 2 1d -decimals 20
	$(ELMER_SOLVER)


clean:
	/bin/rm test.log temp.log mon.out Poisson Poisson.o Source Source.o so_locations
	/bin/rm -r 1d
