# 
# Experimental CMake configuration script for Elmer
# 
# Authors:
#  Sami Ilvonen, CSC - IT Center for Science, Ltd.
#  Phil Weir, NUMA Engineering Services, Ltd.
#
# First public beta release 11th of October, 2013
#
# Note that this version is highly experimental and includes
# only a very limited subset of features that are supported
# by the original autotools scripts.
#

PROJECT(Elmer Fortran C CXX)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/Modules")

ENABLE_TESTING()

SET(ELMER_FEM_MAJOR_VERSION 7)
SET(ELMER_FEM_MINOR_VERSION 0)
SET(ELMER_FEM_VERSION
  ${ELMER_FEM_MAJOR_VERSION}.${ELMER_FEM_MINOR_VERSION})

SET(VERSION ${ELMER_FEM_VERSION})
SET(CMAKE_POSITION_INDEPENDENT_CODE TRUE)

FIND_PACKAGE(MPI REQUIRED)
ADD_DEFINITIONS(${MPI_Fortran_COMPILE_FLAGS})
INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATHS})
LINK_DIRECTORIES(${MPI_Fortran_LIBRARIES})

FIND_PACKAGE(BLAS REQUIRED)
FIND_PACKAGE(LAPACK REQUIRED)

# Check if Fortran compiler supports contiguous keyword
INCLUDE(testContiguous)
IF(CMAKE_Fortran_COMPILER_SUPPORTS_CONTIGUOUS)
  ADD_DEFINITIONS(-DCONTIG=,CONTIGUOUS)
ELSE()
  ADD_DEFINITIONS(-DCONTIG=)
ENDIF()

SET(FC_STDCALLBULL " ")
SET(FC_FUNC FC_GLOBAL)
SET(FC_FUNC_ FC_GLOBAL_)

INCLUDE(FortranCInterface)
FortranCInterface_HEADER(FCMangle.h MACRO_NAMESPACE "FC_")
FortranCInterface_VERIFY(CXX)

ADD_SUBDIRECTORY(umfpack)
ADD_SUBDIRECTORY(hutiter)
ADD_SUBDIRECTORY(eio)
ADD_SUBDIRECTORY(matc)
ADD_SUBDIRECTORY(fem)
ADD_SUBDIRECTORY(mathlibs)
ADD_SUBDIRECTORY(elmergrid)

MESSAGE(STATUS "------------------------------------------------")
MESSAGE(STATUS "  BLAS library:   " ${BLAS_LIBRARIES})
MESSAGE(STATUS "  LAPACK library: " ${LAPACK_LIBRARIES})
MESSAGE(STATUS "------------------------------------------------")
