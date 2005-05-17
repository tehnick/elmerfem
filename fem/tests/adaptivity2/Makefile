# Adaptive solver with internal spliting
#

run:
	$(ELMER_GRID) 1 2 Ldomain
	$(ELMER_SOLVER)

clean:
	/bin/rm test.log temp.log fort.12 mon.out
	/bin/rm -rf Ldomain
	/bin/rm -rf RefinedMesh*
