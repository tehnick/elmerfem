#!/bin/bash  

# Define the number of partitions (has to be a parallel run, i.e. np>1 !!)
export np=4 

# Test to run 

file=Adjoint_Beta.sif  # Control inverse method; optimisation of the slip coef.
file=Adjoint_Mu.sif    # Control inverse method; optimisation of the Viscosity
#file=Robin_Beta.sif   # Robin inverse method; optimisation of the slip coef.

## make compilation and Mesh
make clean
make 
make Mesh

# Run the test
echo $file > ELMERSOLVER_STARTINFO
mpirun -n $np ElmerSolver_mpi
