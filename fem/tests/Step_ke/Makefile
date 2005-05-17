# Backward facing step / k-epsilon model test
#
run:
	$(ELMER_HOME)/bin/ElmerGrid 1 2 Step_ke;
	$(ELMER_HOME)/bin/ElmerSolver

clean:
	/bin/rm test.log temp.log mon.out
	/bin/rm -r Step_ke
