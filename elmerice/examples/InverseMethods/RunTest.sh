#!/bin/bash  

# Define the number of partitions (has to be a parallel run, i.e. np>1 !!)
export np=8 

## make compilation and Mesh
make clean
make 
make Mesh

# Run the test
echo Adjoint_Beta.sif > ELMERSOLVER_STARTINFO
mpirun -n $np ElmerSolver_mpi
