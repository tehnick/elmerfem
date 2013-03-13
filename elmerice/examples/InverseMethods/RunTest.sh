#!/bin/bash  

# Define the number of partitions (has to be a parallel run, i.e. np>1 !!)
export np=8 

# Test to run 

file=Adjoint_Beta.sif  # Control inverse method
#file=Robin_Beta.sif   # Robin inverse method

## make compilation and Mesh
make clean
make 
make Mesh

# Run the test
echo $file > ELMERSOLVER_STARTINFO
mpirun -n $np ElmerSolver_mpi
