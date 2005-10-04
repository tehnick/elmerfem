# advection-reaction equation by discontinuous galerkin
#
run:
	$(ELMER_GRID) 1 2 square
	$(F90) -c AdvReactDG.f90
	$(LD) -o AdvReactDG$(SHL_EXT) AdvReactDG$(OBJ_EXT) $(LIBS)
	$(ELMER_SOLVER)

clean:
	/bin/rm test.log temp.log mon.out dg.ep AdvReactDG$(SHL_EXT) AdvReactDG$(OBJ_EXT)

