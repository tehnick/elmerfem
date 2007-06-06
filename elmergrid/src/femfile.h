/* femfile.h */
/* Routines for the input and output of data from other
   FEM packages, such as Abaqus and Fidap. */

int SaveAbaqusInput(struct FemType *data,char *prefix,int info);
int LoadAbaqusInput(struct FemType *data,struct BoundaryType *bound,
		    char *prefix,int info);
int LoadAbaqusOutput(struct FemType *data,char *prefix,int info);
int SaveFidapOutput(struct FemType *data,char *prefix,int info,
		    int vctrs,Real *vect1, ...);
int LoadFidapInput(struct FemType *data,char *prefix,int info);
int LoadAnsysInput(struct FemType *data,struct BoundaryType *bound,
		   char *prefix,int info);
int LoadNastranInput(struct FemType *data,struct BoundaryType *bound,
		     char *prefix,int info);
int LoadFemlabMesh(struct FemType *data,struct BoundaryType *bound,
		   char *prefix,int info);
int LoadFieldviewInput(struct FemType *data,char *prefix,int info);
int LoadTriangleInput(struct FemType *data,struct BoundaryType *bound,
		      char *prefix,int info);
int LoadMeditInput(struct FemType *data,struct BoundaryType *bound,
		   char *prefix,int info);
int LoadComsolMesh(struct FemType *data,char *prefix,int info);
int LoadGidInput(struct FemType *data,struct BoundaryType *bound,
		    char *prefix,int info);
int LoadGmshInput(struct FemType *data,struct BoundaryType *bound,
		  char *prefix,int info);
int SaveFastcapInput(struct FemType *data,
		     struct BoundaryType *bound,char *prefix,int decimals,int info);
int LoadUniversalMesh(struct FemType *data,char *prefix,int info);

