# Adaptive solver test with remeshing
#

run:
	/bin/mkdir -p Ldomain
	$(ELMER_HOME)/bin/Mesh2D Ldomain.mif .
	/bin/mv mesh.* Ldomain
	/bin/cp Ldomain.mif Ldomain
	$(ELMER_HOME)/bin/Solver

clean:
	/bin/rm test.log temp.log bgmesh fort.12 mon.out
	/bin/rm -rf Ldomain
	/bin/rm -rf RefinedMesh*
