/* femelmer.h */
/* Routines for input, output and manipulation of Funcs and ElmerPost
   formats (programs created by Juha Ruokolainen at CSC). */ 
#define HAVE_METIS 0
#define HAVE_MATC 0

int FuseSolutionElmerPartitioned(char *prefix,char *outfile,int decimals,
				 int minstep, int maxstep, int dstep, int info);
int SaveSolutionElmer(struct FemType *data,struct BoundaryType *bound,
		      int nobound,char *prefix,int decimals,int info);
int SaveSolutionElmerTriangles(struct FemType *data,char *prefix,int info);
int SaveElmerInput(struct FemType *data,struct BoundaryType *bound,
		   char *prefix,int decimals, int info);
int SaveElmerInputFemBem(struct FemType *data,struct BoundaryType *bound,
			 char *prefix,int decimals, int info);
int PartitionSimpleElements(struct FemType *data,int dimpart[],int dimper[],
			    int partorder, Real corder[],int info);
int PartitionSimpleNodes(struct FemType *data,int dimpart[],int dimper[],
			 int partorder, Real corder[],int info);
#if HAVE_METIS
int PartitionMetisElements(struct FemType *data,int partitions,int dual,int info);
int PartitionMetisNodes(struct FemType *data,int partitions,int metisopt,int info);
int ReorderElementsMetis(struct FemType *data,int info);
#endif
int OptimizePartitioning(struct FemType *data,struct BoundaryType *bound,int noopt,int info);
int SaveElmerInputPartitioned(struct FemType *data,struct BoundaryType *bound,
			      char *prefix,int decimals,int halo,int indirect,
			      int info);

