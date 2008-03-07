/* feminfo.h */
/* Functions providing the user information mesh creation,
   solution and calculation. Includes interfaces for input 
   and output files. */


#define MAXFORMATS 15
int Getline(char *line1,FILE *io);
int GetCommand(char *line1,char *line2,FILE *io);
int SaveElmergrid(struct GridType *grid,int nogrids,char *prefix,int info);
int LoadElmergrid(struct GridType **grid,int *nogrids,char *prefix,int info);
void InitParameters(struct ElmergridType *eg);
int LoadCommands(char *prefix,struct ElmergridType *eg,
		 struct GridType *grid, int mode,char *IOmethods[],int info);
int CreateElmerGridMesh(struct ElmergridType *eg,struct GridType *grid,
			struct FemType *data,struct BoundaryType *boundaries,
			Real relh,int info);
