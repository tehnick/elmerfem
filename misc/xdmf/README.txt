This folder contains an experimental Xdmf/HDF5 result writer module for Elmer

Limitations: Only scalar fields and linear elements are supported at the moment

Sample usage:

     ./compile.sh

     ElmerGrid 1 2 beam3d
     ElmerGrid 2 2 beam3d -metis 8

     ElmerGrid 1 2 angle3d
     ElmerGrid 2 2 angle3d -metis 8

     mpirun -np 8 ElmerSolver_mpi

The results are stored in the files

     results.xmf
     results.h5

The file "results.xmf" can be opened with in Paraview.
