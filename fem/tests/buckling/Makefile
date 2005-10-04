# Linear elasticity solver test in 2D (buckling)
#
run:
	$(F90) check.f90 -o check$(EXE_EXT)
	$(ELMER_GRID) 1 2 buckling
	$(ELMER_SOLVER)
	grep -i '(' test.log > out2.txt
	grep -i 'eigen' out2.txt > out3.txt
	./check
	chmod 775 *

clean:
	/bin/rm -r buckling out*.txt
	/bin/rm test.log temp.log mon.out
