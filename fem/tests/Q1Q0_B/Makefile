# A (navier-)stokes solver with  Q1/Q0 elements
#
#
run:
	$(F90) -c Q1Q0_B.f90
	$(ELMER_GRID) 1 2 Step
	$(LD) -o Q1Q0_B$(SHL_EXT) Q1Q0_B$(OBJ_EXT) $(LIBS)
	$(ELMER_SOLVER)

clean:
	/bin/rm -r Step
	/bin/rm test.log temp.log  Q1Q0_B$(SHL_EXT) Q1Q0_B$(OBJ_EXT)
