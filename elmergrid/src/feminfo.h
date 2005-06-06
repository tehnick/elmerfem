/* feminfo.h */
/* Functions providing the user information mesh creation,
   solution and calculation. Includes interfaces for input 
   and output files. */

int Getline(char *line1,FILE *io);
int GetCommand(char *line1,char *line2,FILE *io);
int SaveSolutionDens(struct FemType *data,char *prefix,int info);
int SaveCellInfo(struct GridType *grid,struct CellType *cell,
		 char *prefix,int info);
int SaveBoundary(struct FemType *data,struct BoundaryType *bound,
		 char *prefix,int info);
int SaveBoundariesChain(struct FemType *data,struct BoundaryType *bound,
			char *prefix,int info);
int SaveBoundaryLine(struct FemType *data,int direction,
		     Real c0,char* prefix,int info);
int SaveBoundaryForm(struct FemType *data,struct CellType *cell, 
		     char* filename,int info);
int SaveSubcellForm(struct FemType *data,struct CellType *cell, 
		    char* filename,int info);
int SaveViewFactors(struct FemType *data,struct BoundaryType *bound,
		    char *prefix,int info);
int LoadViewFactors(struct FemType *data,struct BoundaryType *bound,
		    char *prefix,int info);
int SaveClosureFactors(struct BoundaryType *bound,char *prefix,int info);
int SaveElmergrid(struct GridType *grid,int nogrids,char *prefix,int info);
int LoadElmergrid(struct GridType **grid,int *nogrids,char *prefix,int info);
int SaveGridToGridMapping(struct CellType *cell1, struct GridType *grid1, 
			  struct CellType *cell2, struct GridType *grid2,
			  char *prefix);
int ShowCorners(struct FemType *knot,int variable,Real offset);
