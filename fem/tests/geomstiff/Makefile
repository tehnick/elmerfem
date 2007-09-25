# Modal analysis with geometric stiffness in 2D
#
run:
	$(F90) check.f90 -o check$(EXE_EXT)
	$(ELMER_GRID) 1 2 geomstiff
	$(ELMER_SOLVER)
	grep -i '(' test.log > out2.txt
	grep -i 'eigen' out2.txt > out3.txt
	./check$(EXE_EXT)
	mv tmp.log test.log
	chmod 775 *

clean:
	/bin/rm -r geomstiff out*.txt check$(EXE_EXT)
	/bin/rm test.log temp.log mon.out
