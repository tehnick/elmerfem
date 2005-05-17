# Flow-structure interaction (FSI) test in 2D
#
run:
	$(F90) -c FsiStuff.f90
	$(LD) -o FsiStuff$(SHL_EXT) FsiStuff.o $(LIBS)
	$(ELMER_HOME)/bin/ElmerGrid 1 2 fsi
	$(ELMER_HOME)/bin/Solver

clean:
	/bin/rm FsiStuff.o FsiStuff$(SHL_EXT) test.log temp.log mon.out so_locations
	/bin/rm -r fsi
