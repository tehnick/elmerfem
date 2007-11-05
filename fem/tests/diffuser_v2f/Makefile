# Asymmetric diffuser / V2-F model test
#
run:
	$(F90) -c Epsilon-BC.f90
	$(LD) -o Epsilon-BC$(SHL_EXT) Epsilon-BC$(OBJ_EXT) $(LIBS)
	$(ELMER_GRID) 1 2 diffuser -scale 0.127 0.127 0.127
	$(ELMER_SOLVER)

clean:
	/bin/rm test.log temp.log Epsilon-BC$(SHL_EXT) Epsilon-BC$(OBJ_EXT)
