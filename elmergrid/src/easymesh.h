int EasymeshSave();
#if ELMER
int EasymeshCopy(struct FemType *data,struct BoundaryType *bound);
#endif
int Easymesh(int argc, char *argv[],
	     int *nodes,int *elements,int *sides);
