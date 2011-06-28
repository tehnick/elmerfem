################################################
### Compiles the libraries, by Vili Forsell
################################################

##### MACROS

# The modules used by the main program
MODULES = NetCDFGeneralUtils.f90 NetCDFGridUtils.f90 NetCDFInterpolate.f90 CustomTimeInterpolation.f90
# The file for the solver
MAIN = GridDataMapper.f90 cs2cs_interface.c
# The example Solver Input Files:
# case.sif is the latest example for hands-on experiments
SIFS = $(WORK_DIRECTORY)case.sif
# Includes NetCDF and such from the include directory
INCLUDE_FLAGS = -I/usr/include 
# NetCDF library path and flags to catch the necessary headers
LIBRARY_FLAGS = -L/usr/lib -lnetcdff -lnetcdf -lproj
# The shared library (.so) for using in Elmer via a Solver Input File
OUTFILE = GridLib.so 
# Compiles it all with Elmer Fortran compiler
COMPILER = elmerf90
# A path for back-up folder (relatively obsolete via use of SVN)
BACKUP = ./Backup/
# Packaging macros for easily transporting latest backups elsewhere
PACKET = GridMap.tar
TAR_FLAGS = cf
# Documentation files
DOCUMENTATION = ./doc/*
# Work directory; chosen to be called "data" and assumed to contain mesh files, ELMER_STARTINFO, case.sif, etc.
WORK_DIRECTORY = ./data/

##### RULES

# Builds the shared library for Elmer
compile: modules
	$(COMPILER) $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) -o $(OUTFILE) $(MAIN) $(MODULES)
	cp $(OUTFILE) $(WORK_DIRECTORY) # Copies the library file to the work directory

# Compiles modules used by the main file
modules:
	$(COMPILER) -c $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) $(MODULES)

# Cleans extra stuff
clean:
	rm *.mod *.o *.so

# Saves the work in a backup location
save:
	cp $(MODULES) $(BACKUP)
	cp $(MAIN) $(BACKUP)
	cp $(SIFS) $(BACKUP)

# Packs the work for transportation
pack:
	tar $(TAR_FLAGS) $(PACKET) $(MODULES) $(SIFS) $(MAIN) $(DOCUMENTATION)
	cp $(PACKET) $(BACKUP)
