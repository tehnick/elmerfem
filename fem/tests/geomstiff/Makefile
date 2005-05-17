# Modal analysis with geometric stiffness in 2D
#
run:
	$(F90) check.f90 -o check
	$(ELMER_HOME)/bin/ElmerGrid 1 2 geomstiff
	$(ELMER_HOME)/bin/Solver
	grep -i '(' test.log > out2.txt
	grep -i 'eigen' out2.txt > out3.txt
	./check
	chmod 775 *

clean:
	/bin/rm -r geomstiff out*.txt
	/bin/rm test.log temp.log mon.out
