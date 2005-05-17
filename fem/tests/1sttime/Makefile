# test 1st order time integration
#
run:
	$(F90) -c TimeIntegrate.f90 Source.f90
	$(LD) -o TimeIntegrate$(SHL_EXT) TimeIntegrate.o $(LIBS)
	$(LD) -o Source$(SHL_EXT) Source.o $(LIBS)
	$(ELMER_HOME)/bin/Solver


clean:
	/bin/rm test.log temp.log mon.out TimeIntegrate.so TimeIntegrate.o
	/bin/rm Source.so Source.o so_locations 
