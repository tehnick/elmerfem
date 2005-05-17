# Navier-Velocity solver with pressure projection
#
run:
	$(F90) -c Velocity.f90 Pressure.f90
	$(LD) -o Velocity$(SHL_EXT) Velocity.o $(LIBS)
	$(LD) -o Pressure$(SHL_EXT) Pressure.o $(LIBS)
	$(ELMER_SOLVER)


clean:
	/bin/rm test.log temp.log mon.out Velocity$(SHL_EXT) Velocity.o Pressure$(SHL_EXT) Pressure.o so_locations
