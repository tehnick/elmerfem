# Solves vector Helmholtz equation using face elements
#
run:
	$(F90) -c Helmholtz.f90
	$(LD) -o Helmholtz$(SHL_EXT) Helmholtz.o $(LIBS)
	$(ELMER_HOME)/bin/ElmerGrid 1 2 square.grd -triangles
	$(ELMER_HOME)/bin/Solver


clean:
	/bin/rm test.log temp.log mon.out Helmholtz$(SHL_EXT) Helmholtz.o so_locations  ratkaisu.ep
	/bin/rm -r square
