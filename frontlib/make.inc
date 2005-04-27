# ELMER FrontLib (libfront.lib) makefile, objects and dependencies
# Needs gnu make!
# Martti Verho
# 17.03.03

all: $(TARGET) install

# Result directories
INCLDIR = $(ELMER_PREFIX)$(PATH_SEP)include
LIBDIR = $(ELMER_PREFIX)$(PATH_SEP)lib

#-Search paths
vpath %.$(CPP_EXT) src
vpath %.$(C_EXT) src
vpath %.h include
vpath %.hpp include
vpath %.h ../FrontLib/include

#-Objects 
OBJECTS = frontlib.$(OBJ_EXT)

#-Dependencies
frontlib.$(OBJ_EXT): frontlib.h

#-Rules
%.$(OBJ_EXT) : %.$(CPP_EXT) 
	$(CC) $(CFLAGS) $(INCLUDES) $<

%.$(OBJ_EXT) : %.$(C_EXT) 
	$(C) $(CFLAGS) $(INCLUDES) $<

#-Create library
$(TARGET):$(OBJECTS)
	$(AR) $(LINKOUT) $(OBJECTS)

install:
	$(CP) $(TARGET) $(LIBDIR)
	$(CP) .$(PATH_SEP)include$(PATH_SEP)frontlib.h $(INCLDIR)
	$(CP) .$(PATH_SEP)include$(PATH_SEP)front_egfdefs.h $(INCLDIR)

build: $(TARGET)

clean:
	$(RM) *.$(OBJ_EXT) $(TARGET)

cleanobject:
	$(RM) *.$(OBJ_EXT)

cleanlink:
	$(RM) $(TARGET)
