# magnetohydrodynamics with two bodies
#
run:
	$(F90) -c  exmag.f90
	$(LD) -o exmag$(SHL_EXT) exmag.o $(LIBS)
	$(ELMER_GRID) 1 2 cz.grd -out Czmesh
	$(ELMER_SOLVER)


clean:
	/bin/rm test.log temp.log mon.out exmag.o exmag 
	/bin/rm -r Czmesh
