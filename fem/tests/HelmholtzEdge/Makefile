# Solves vector Helmholtz equation using face elements
#
run:
	$(F90) -c Hrot.f90
	$(LD) -o Hrot$(SHL_EXT) Hrot.o $(LIBS)
	$(ELMER_GRID) 1 2 square.grd -triangles
	$(ELMER_SOLVER)

clean:
	/bin/rm test.log temp.log mon.out Hrot$(SHL_EXT) Hrot.o so_locations ratkaisu.ep
	/bin/rm -r square
