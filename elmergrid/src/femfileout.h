/* femfileout.h */
/* Routines for exporting data to other
   FEM packages, such as Abaqus and Fidap. */

int SaveAbaqusInput(struct FemType *data,char *prefix,int info);
int SaveFidapOutput(struct FemType *data,char *prefix,int info,
		    int vctrs,Real *vect1, ...);
int SaveFastcapInput(struct FemType *data,
		     struct BoundaryType *bound,char *prefix,int decimals,int info);
