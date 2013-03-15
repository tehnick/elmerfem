#### Makefile to compile the solver Scattered2DDataInterpolator
##### This solver require external libraries:
#####	csa: http://code.google.com/p/csa-c/
#####	nn: http://code.google.com/p/nn-c/
####### Thies libraries must be compiled by the user and this Makefile edited/updated


# change to "yes" is you have compiled nn and csa libraries and want to compile the solver Scattered2DDataInterpolator
HAVENNCS=No


####### EDIT the floowing flags according to your installation ###########

# PATH to the HOME directory of NN and CSA header files and libraries
NN_HOME=/Users/fgillet/Programmes/INTERPOLATION/nn
CS_HOME=/Users/fgillet/Programmes/INTERPOLATION/csa

###### C compilert and FLAG
CC=mpicc
CCFLAG=-fPIC

##########################################################################


##############
ifeq ($(HAVENNCS),yes)

NNLIB=$(NN_HOME)/libnn.a
CSLIB=$(CS_HOME)/libcsa.a

solverobjects := $(solverobjects) Solvers/Scattered2DDataInterpolator.o Solvers/csa_interpolate_points.o $(NNLIB) $(CSLIB)

Scattered2DModules= nearestneighbour.mod point_type.mod
MODULES := $(MODULES) $(Scattered2DModules)

endif



Solvers/Scattered2DDataInterpolator.o: Solvers/Scattered2DDataInterpolator.f90 $(Scattered2DModules) 
	elmerf90-nosh  -c Solvers/Scattered2DDataInterpolator.f90 -o Solvers/Scattered2DDataInterpolator.o
Solvers/csa_interpolate_points.o : Solvers/csa_interpolate_points.c
	$(CC) $(CCFLAG) -I$(CS_HOME) -c $^ -o $@
$(Scattered2DModules): Solvers/Scattered2D_FInterface.f90
	elmerf90-nosh -c $< -o Solvers/Scattered2D_FInterface.o

