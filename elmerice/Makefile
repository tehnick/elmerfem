##############################################
# Makefile for Elmer/Ice
# (c) 2012 CSC - IT Center for Science Ltd.
# Author: T. Zwinger
# Original Date: 2012-12-03
#############################################

ELMERICE_HOME ?= $(ELMER_HOME)/share/elmersolver
EXECUTABLES := ElmerIceSolvers.so ElmerIceUSF.so
INSTALLED := $(ELMERICE_HOME)/lib/ElmerIceSolvers.so \
 $(ELMERICE_HOME)/lib/ElmerIceUSF.so \
 $(ELMERICE_HOME)/include/defgrid.mod
MODULES := defgrid.mod
SOLVERSOURCE := \
Solvers/AIFlowSolve_nlD2.f90 \
Solvers/AIFlowSolve_nlS2.f90 \
Solvers/CaffeFlow.f90 \
Solvers/CaffeSolver.f90 \
Solvers/ComputeDevStressNS.f90 \
Solvers/ComputeEigenValues.f90 \
Solvers/ComputeNormal.f90 \
Solvers/ComputeStrainRate.f90 \
Solvers/DeformationalHeat.f90 \
Solvers/fAandfB_in.f90 \
Solvers/FabricSolve.f90 \
Solvers/Flowdepth.f90 \
Solvers/ForceToStress.f90 \
Solvers/GetHydrostaticLoads.f90 \
Solvers/GolfLaw.f90 \
Solvers/GroundedSolver.f90 \
Solvers/IntegratedVelocity.f90 \
Solvers/PorousSolve.f90 \
Solvers/SIASolver.f90 \
Solvers/SSASolver.f90 \
Solvers/TemperateIce.f90
USFSOURCE := \
UserFunctions/Buoyancy.f90 \
UserFunctions/USF_Contact.f90 \
UserFunctions/USF_LateralFriction.f90 \
UserFunctions/USF_ShapeFactor.f90 \
UserFunctions/USF_Sliding.f90 \
UserFunctions/USF_Zs.f90

solverobjects := $(patsubst %.f90,%.o,$(SOLVERSOURCE))
usfobjects := $(patsubst %.f90,%.o,$(USFSOURCE))

all: clean compile

compile:  $(EXECUTABLES)

install: $(EXECUTABLES)
	cp $(EXECUTABLES) $(ELMERICE_HOME)/lib
	cp $(MODULES) $(ELMERICE_HOME)/include

ElmerIceSolvers.so: $(solverobjects)
	mv *.o Solvers
	elmerf90 $(solverobjects) -o ElmerIceSolvers.so

ElmerIceUSF.so: $(usfobjects)
	mv *.o UserFunctions
	elmerf90 $(usfobjects) -o ElmerIceUSF.so

clean:
	rm -rf  Solvers/*.o UserFunctions/*.o $(EXECUTABLES) $(MODULES)

purge: clean
	rm -rf $(INSTALLED)

%.o: %.f90 
	elmerf90-nosh  $< -c




