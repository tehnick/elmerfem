################################################
### Compiles the libraries, by Vili Forsell
################################################

##### MACROS

# Paths:
MODULE_PATH = ./src/GridDataMapper/
GEN_PATH = ./src/Generators/
POST_PATH = ./src/Postprocess/

TMP_PATH = ./tmp/
BACKUP_PATH = ./Backup/
BIN_PATH = ./bin/
SRC_PATH = ./src/
TEST_PATH = ./tests/
DOC_PATH = ./doc/

VPATH = $(MODULE_PATH)

# Work directory; chosen to be called "data" and assumed to contain mesh files, ELMER_STARTINFO, case.sif, etc.
WORK_DIRECTORY = ./data/

ROOT_FILES = Makefile README

# The modules used by the main program
SOLVER_SRC = NetCDFGeneralUtils.f90 NetCDFGridUtils.f90 NetCDFInterpolate.f90 CustomTimeInterpolation.f90 MapperUtils.f90
SOLVER_OBJS =  $(SOLVER_SRC:.f90=.o)
SOLVER_MAIN = GridDataMapper.f90 cs2cs_interface.c
SOLVER_OUTFILE = GridLib.so

GEN_SRC = $(wildcard $(GEN_PATH)*.f90) $(wildcard $(GEN_PATH)*.cc)
GEN_OBJS = $(addsuffix .o, $(basename $(shell find $(GEN_PATH) -name *.cc )) $(basename $(shell find $(GEN_PATH) -name *.f90)) $(basename $(shell find $(GEN_PATH) -name *.c)) )
GEN_MAIN = $(addsuffix .o, $(basename $(GEN_SRC)))
# outputs to the file names themselves

POST_SRC = $(wildcard $(POST_PATH)*.f90) $(wildcard $(POST_PATH)*.cc)
POST_OBJS = $(addsuffix .o, $(basename $(shell find $(POST_PATH) -name *.cc )) $(basename $(shell find $(POST_PATH) -name *.f90)) $(basename $(shell find $(POST_PATH) -name *.c)) )
POST_MAIN = $(addsuffix .o, $(basename $(POST_SRC)))
# outputs to the file names of the root themselves

ALL_HEADERS = $(shell find $(SRC_PATH) -name *.hh) $(shell find $(SRC_PATH) -name *.h)
HEADER_PATHS = $(sort $(dir $(ALL_HEADERS))) # sort removes duplicates

# Compilers
COMPILER_F90 = elmerf90
COMPILER_CXX = g++
COMPILER_C = gcc

# Includes NetCDF and such from the include directory
INCLUDE_FLAGS = -I/usr/include $(foreach hdr, $(HEADER_PATHS), -I$(hdr))
# NetCDF library path and flags to catch the necessary headers
LIBRARY_FLAGS = -L/usr/lib -lnetcdff -lnetcdf -lproj -L$(TMP_PATH)
# The shared library (.so) for using in Elmer via a Solver Input File

# Packaging macros for easily transporting latest backups elsewhere
PACKET = GridMap.tar
TEST_PACKET = Tests.tar
TAR_FLAGS = cf

##### RULES

# Builds the shared library for Elmer
solver: $(SOLVER_OBJS)
	$(COMPILER_F90) $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) -J$(TMP_PATH) $(addprefix $(TMP_PATH), $^) $(addprefix $(MODULE_PATH), $(SOLVER_MAIN)) -o $(addprefix $(BIN_PATH), $(SOLVER_OUTFILE))

generator: $(GEN_OBJS)
	$(foreach file,$(notdir $(GEN_MAIN)), $(COMPILER_F90) $(addprefix $(TMP_PATH), $(file)) -o $(addprefix $(BIN_PATH), $(basename $(file)));)

postprocessor: $(POST_OBJS) 
	$(foreach file,$(notdir $(POST_MAIN)), $(COMPILER_F90) $(addprefix $(TMP_PATH), $(file)) -o $(addprefix $(BIN_PATH), $(basename $(file)));)

%.o: %.f90
	$(COMPILER_F90) -c $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) $^ -J$(TMP_PATH) -o $(addprefix $(TMP_PATH), $(notdir $@))

%.o: %.cc
	$(COMPILER_CXX) -c $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) $^ -J$(TMP_PATH) -o $(addprefix $(TMP_PATH), $(notdir $@))

%.o: %.c
	$(COMPILER_C) -c $(INCLUDE_FLAGS) $(LIBRARY_FLAGS) $^ -J$(TMP_PATH) -o $(addprefix $(TMP_PATH), $(notdir $@))

# Cleans extra stuff
clean:
	rm $(TMP_PATH)* $(BIN_PATH)*

# Saves the work in a backup location
save:	pack
	- @cp -r $(DOC_PATH) $(SRC_PATH) $(ROOT_FILES) $(BACKUP_PATH)

# Too large to copy just as files; so they're compressed first
save_tests:
	tar $(TAR_FLAGS) $(addprefix $(BACKUP_PATH), $(TEST_PACKET)) $(TEST_PATH)

# Packs the work for transportation
pack:
	tar $(TAR_FLAGS) $(addprefix $(BACKUP_PATH), $(PACKET)) $(SRC_PATH) $(DOC_PATH) $(ROOT_FILES)
