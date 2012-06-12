/*
   Example 5

   Interface:    Linear-Algebraic (IJ)

   Compile with: make ex5

   Sample run:   mpirun -np 4 ex5

   Description:  This example solves the 2-D
                 Laplacian problem with zero boundary conditions
                 on an nxn grid.  The number of unknowns is N=n^2.
                 The standard 5-point stencil is used, and we solve
                 for the interior nodes only.

                 This example solves the same problem as Example 3.
                 Available solvers are AMG, PCG, and PCG with AMG or
                 Parasails preconditioners.
*/

#include "../config.h"

#ifdef HAVE_HYPRE
#include <math.h>
#include "_hypre_utilities.h"
#include "krylov.h"
#include "HYPRE.h"
#include "HYPRE_parcsr_ls.h"

typedef struct {

int ilower, iupper;

HYPRE_IJMatrix A;
HYPRE_IJMatrix Atilde;

int hypre_method;
HYPRE_Solver solver, precond;

} ElmerHypreContainer;

/* output and logging are enabled if the verbosity flag is larger than these: */
#define PRINTLEVEL 0
#define LOGLEVEL 0

#define FPRINTF if (verbosity>PRINTLEVEL) fprintf
#define STDOUT stdout

/* there are two possible procedures of calling HYPRE here, 
 the standard one (does everything once), and a step-wise
 procedure of setup, solve and cleanup.
 TO DO: we should add the possibility to keep the precon-
 ditioner the same but update the system matrix (SolveHYPRE3), right now
 calling SolveHYPRE2 solves with the matrix passed into   
 SolveHYPRE1.

 standard call: - convert matrix
                - convert vector b
                - setup solver and preconditioner
                - solve system
                - convert vector x back
                - destroy all data structures
*/
void STDCALLBULL FC_FUNC(solvehypre,SOLVEHYPRE)
 (
  int *nrows,int *rows, int *cols, double *vals, int *perm,
  int *invperm, int *globaldofs, int *owner,  double *xvec,
  double *rhsvec, int *pe, int *ILUn, int *Rounds, double *TOL,
  int *hypre_method, int *hypre_intpara, double *hypre_dppara
 )
{
   int i, j, k, *rcols;
   int myid, num_procs;
   int N, n;

   int ilower, iupper;
   int local_size, extra;

   int solver_id;
   int print_solution, print_system;

   double  *txvec, st, realtime_();

   HYPRE_IJMatrix A;
   HYPRE_ParCSRMatrix parcsr_A;
   HYPRE_IJVector b;
   HYPRE_ParVector par_b;
   HYPRE_IJVector x;
   HYPRE_ParVector par_x;

   HYPRE_Solver solver, precond;
   int verbosity = 10;
   
st  = realtime_();
   /* How many rows do I have? */
   local_size = *nrows;

   ilower=1000000000;
   iupper=0;
   for( i=0; i<local_size; i++ )
   {
      if ( owner[i] ) {
        if ( iupper < globaldofs[i] ) iupper=globaldofs[i];
        if ( ilower > globaldofs[i] ) ilower=globaldofs[i];
      }
   }

  /* which process number am I? */
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   /* Create the matrix.
      Note that this is a square matrix, so we indicate the row partition
      size twice (since number of rows = number of cols) */
   HYPRE_IJMatrixCreate(MPI_COMM_WORLD, ilower, iupper, ilower, iupper, &A);

   /* Choose a parallel csr format storage (see the User's Manual) */
   HYPRE_IJMatrixSetObjectType(A, HYPRE_PARCSR);

   /* Initialize before setting coefficients */
   HYPRE_IJMatrixInitialize(A);

   /* Now go through my local rows and set the matrix entries.
      Note that here we are setting one row at a time, though
      one could set all the rows together (see the User's Manual).
   */
   {
      int nnz,irow,i,j,k,*rcols,csize=32;

      rcols = (int *)malloc( csize*sizeof(int) );
      for (i = 0; i < local_size; i++)
      {
         nnz = rows[i+1]-rows[i];
         if ( nnz>csize ) {
           rcols = (int *)realloc( rcols, nnz*sizeof(int) );
           csize = nnz;
         }
         irow=globaldofs[i];
         for( k=0,j=rows[i]; j<rows[i+1]; j++,k++)
         {
           rcols[k] = globaldofs[cols[j-1]-1];
         }
         HYPRE_IJMatrixAddToValues(A, 1, &nnz, &irow, rcols, &vals[rows[i]-1]);
      }
        free( rcols );
   }

   /* Assemble after setting the coefficients */
   HYPRE_IJMatrixAssemble(A);

   /* Get the parcsr matrix object to use */
   HYPRE_IJMatrixGetObject(A, (void**) &parcsr_A);

   /* Create the rhs and solution */
   rcols = (int *)malloc( local_size*sizeof(int) );
   txvec = (double *)malloc( local_size*sizeof(double) );
   for( k=0,i=0; i<local_size; i++ ) rcols[k++] = globaldofs[i];

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&b);
   HYPRE_IJVectorSetObjectType(b, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(b);
   for( i=0; i<local_size; i++ ) txvec[i] = rhsvec[i];
   HYPRE_IJVectorAddToValues(b, local_size, rcols, txvec );

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&x);
   HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(x);
   for( i=0; i<local_size; i++ ) txvec[i] = xvec[i];
   HYPRE_IJVectorSetValues(x, local_size, rcols, txvec );

   HYPRE_IJVectorAssemble(b);
   HYPRE_IJVectorGetObject(b, (void **) &par_b);

   HYPRE_IJVectorAssemble(x);
   HYPRE_IJVectorGetObject(x, (void **) &par_x);
   FPRINTF( stderr, "ID no. %i: setup time: %g\n", myid, realtime_()-st );
   st = realtime_();


/*    FPRINTF(stderr,"HYRPE INT: %d %d  %d %d %d \n", hypre_intpara[0], hypre_intpara[1], hypre_intpara[2], hypre_intpara[3], hypre_intpara[4]);  */
/*    FPRINTF(stderr,"HYRPE DP: %d %d %d %d %d \n", hypre_dppara[0], hypre_dppara[1], hypre_dppara[2], hypre_dppara[3], hypre_dppara[4]);  */
   /* Choose a solver and solve the system */
   /* NB.: hypremethod = 0 ... BiCGStab + ILUn
                         1 ... BiCGStab + ParaSails
                         2 ... BiCGStab + BoomerAMG
                        10 ... BoomerAMG 
   */
   if ( *hypre_method < 10) { /* BiGSTAB methods */
     /* Create solver */
     HYPRE_ParCSRBiCGSTABCreate(MPI_COMM_WORLD, &solver);

     /* Set some parameters (See Reference Manual for more parameters) */
     HYPRE_ParCSRBiCGSTABSetMaxIter(solver, *Rounds); /* max iterations */
     HYPRE_ParCSRBiCGSTABSetTol(solver, *TOL);       /* conv. tolerance */
     HYPRE_ParCSRBiCGSTABSetStopCrit(solver, 0);     /* use the two norm as the stopping criteria */
     HYPRE_ParCSRBiCGSTABSetPrintLevel(solver, 2);   /* print solve info */
     HYPRE_ParCSRBiCGSTABSetLogging(solver, 1);      /* needed to get run info later */
     if ( *hypre_method == 0 ) {
       HYPRE_EuclidCreate( MPI_COMM_WORLD, &precond );
       {
         static char *argv[5], str[3];
         argv[0] = "-level";
         sprintf( str, "%d", *ILUn );
	 if (myid == 0) FPRINTF( stderr,"SolveHypre: using BiCGStab + ILU%i\n",*ILUn); 
         argv[1] = str;
         HYPRE_EuclidSetParams( precond, 2, argv );
       }

       /* Set the PCG preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_EuclidSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_EuclidSetup, precond);
     } else if (*hypre_method == 1) { 
       if (myid == 0) FPRINTF( stderr,"SolveHypre: using BiCGStab + paraSails\n"); 
       /* Now set up the ParaSails preconditioner and specify any parameters */
       HYPRE_ParaSailsCreate(MPI_COMM_WORLD, &precond);
       {
	 /* Set some parameters (See Reference Manual for more parameters) */
         /* threshold = dppara[0]; maxlevels= intpara[1] */
	 HYPRE_ParaSailsSetParams(precond, hypre_dppara[0], hypre_intpara[1]);
	 /* filter = dppara[1] */
	 HYPRE_ParaSailsSetFilter(precond, hypre_dppara[1]);
         /* symmetry = intpara[0] */
	 HYPRE_ParaSailsSetSym(precond, hypre_intpara[0]);
	 HYPRE_ParaSailsSetLogging(precond, 3);
       }
       /* Set the PCG preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSetup, precond);
     } else if(*hypre_method == 2){
       if (myid == 0) {
	 FPRINTF( stderr,"SolveHypre: using BiCGStab + boomerAMG\n");
	 FPRINTF( stderr,"RelaxType=%d\n",hypre_intpara[0]); 
	 FPRINTF( stderr,"CoarsenType=%d\n",hypre_intpara[1]); 
	 FPRINTF( stderr,"NumSweeps=%d\n",hypre_intpara[2]); 
	 FPRINTF( stderr,"MaxLevels=%d\n",hypre_intpara[3]); 
	 FPRINTF( stderr,"Interpolation Type=%d\n",hypre_intpara[4]); 
	 FPRINTF( stderr,"Smooth Type=%d\n",hypre_intpara[5]);
	 FPRINTF( stderr,"Cycle Type=%d\n",hypre_intpara[6]);
	 FPRINTF( stderr,"DOFs=%d\n",hypre_intpara[7]);
       }
       HYPRE_BoomerAMGCreate(&precond);
       {
	 /* Set some parameters (See Reference Manual for more parameters) */
	 HYPRE_BoomerAMGSetNumFunctions(precond, hypre_intpara[7]); /* No. of PDE's */
	 HYPRE_BoomerAMGSetPrintLevel(precond, 1); /* print amg solution info */
	 HYPRE_BoomerAMGSetNumSweeps(precond, 1); /* fixed for preconditioner to 1 */
	 HYPRE_BoomerAMGSetTol(precond, 0.0); /* conv. tolerance zero */
	 HYPRE_BoomerAMGSetMaxIter(precond, 1); /* do only one iteration! */
	 HYPRE_BoomerAMGSetRelaxType(precond, hypre_intpara[0]);   /* G-S/Jacobi hybrid relaxation */
	 HYPRE_BoomerAMGSetCoarsenType(precond, hypre_intpara[1]);  /* coarsening type */
	 
	 HYPRE_BoomerAMGSetMaxLevels(precond, hypre_intpara[3]); /* levels of coarsening */
	 HYPRE_BoomerAMGSetInterpType(precond, hypre_intpara[4]);  /* interpolation type */
	 HYPRE_BoomerAMGSetSmoothType(precond, hypre_intpara[5]);  /* smoother type */
       }
       /* Set the BiCGSTAB preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSetup, precond);
     } else if (*hypre_method!=9) {
       fprintf( stderr,"Hypre preconditioning method not implemented\n");
       exit(EXIT_FAILURE);
     }
     

     /* Now setup and solve! */
     HYPRE_ParCSRBiCGSTABSetup(solver, parcsr_A, par_b, par_x);
     HYPRE_ParCSRBiCGSTABSolve(solver, parcsr_A, par_b, par_x);

     /* Destroy solver and preconditioner */
     HYPRE_ParCSRBiCGSTABDestroy(solver);
     if ( *hypre_method == 0 ) {
       HYPRE_EuclidDestroy(precond);
     } else if ( *hypre_method == 1 ) {
       HYPRE_ParaSailsDestroy(precond);
     } else {
       HYPRE_BoomerAMGDestroy(precond);
     }
   } else if ( *hypre_method == 10 ) { /* boomer AMG */
      int num_iterations;
      double final_res_norm;

      if (myid == 0) {
	FPRINTF( stderr,"SolveHypre: using BoomerAMG\n"); 
	FPRINTF( stderr,"RelaxType=%d\n",hypre_intpara[0]); 
	FPRINTF( stderr,"CoarsenType=%d\n",hypre_intpara[1]); 
	FPRINTF( stderr,"NumSweeps=%d\n",hypre_intpara[2]); 
	FPRINTF( stderr,"MaxLevels=%d\n",hypre_intpara[3]); 
	FPRINTF( stderr,"Interpolation Type=%d\n",hypre_intpara[4]); 
	FPRINTF( stderr,"Smooth Type=%d\n",hypre_intpara[5]);
 	FPRINTF( stderr,"Cycle Type=%d\n",hypre_intpara[6]);
  	FPRINTF( stderr,"DOFs=%d\n",hypre_intpara[7]);
      }
      /* Create solver */
      HYPRE_BoomerAMGCreate(&solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_BoomerAMGSetNumFunctions(solver, hypre_intpara[7]); /* No. of PDE's */
      HYPRE_BoomerAMGSetPrintLevel(solver, 3);  
      HYPRE_BoomerAMGSetRelaxType(solver, hypre_intpara[0]);   /* G-S/Jacobi hybrid relaxation */
      HYPRE_BoomerAMGSetCoarsenType(solver, hypre_intpara[1]);  /* coarsening type */
      HYPRE_BoomerAMGSetNumSweeps(solver, hypre_intpara[2]);   /* Sweeeps on each level */
      HYPRE_BoomerAMGSetMaxLevels(solver, hypre_intpara[3]); /* levels of coarsening */
      HYPRE_BoomerAMGSetInterpType(solver, hypre_intpara[4]);  /* interpolation type */
      HYPRE_BoomerAMGSetSmoothType(solver, hypre_intpara[5]);  /* smoother type */
      HYPRE_BoomerAMGSetTol(solver, *TOL);      /* conv. tolerance */
      HYPRE_BoomerAMGSetMaxIter(solver, *Rounds); /* iteration rounds */

      /* Now setup and solve! */
      HYPRE_BoomerAMGSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_BoomerAMGSolve(solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
      HYPRE_BoomerAMGGetNumIterations(solver, &num_iterations);
      HYPRE_BoomerAMGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
	FPRINTF(stderr,"BoomerAMG:\n");
	FPRINTF(stderr,"Iterations = %d\n", num_iterations);
	FPRINTF(stderr,"Final Relative Residual Norm = %e\n", final_res_norm);
	FPRINTF(stderr,"\n");
      }

      /* Destroy solver */
      HYPRE_BoomerAMGDestroy(solver);
   } else {
     fprintf( stderr,"Hypre solver not implemented\n");
     exit(EXIT_FAILURE);
   }

   for( k=0,i=0; i<local_size; i++ )
      if ( owner[i] ) rcols[k++] = globaldofs[i];

   HYPRE_IJVectorGetValues(x, k, rcols, txvec );

   for( i=0,k=0; i<local_size; i++ )
     if ( owner[i] ) xvec[i] = txvec[k++];

   FPRINTF( stderr, "ID no. %i: solve time: %g\n", myid, realtime_()-st );
   free( txvec );
   free( rcols );

   /* Clean up */
   HYPRE_IJMatrixDestroy(A);
   HYPRE_IJVectorDestroy(b);
   HYPRE_IJVectorDestroy(x);
}

/*///////////////////////////////////////////////////////////////////////////////////////////////*/

/* initialization for a new matrix.
      - convert matrix
      - setup solver and preconditioner
      - return a pointer 'Container' which the calling fortran
        program should not alter but pass back into subsequent
        SolveHYPRE2, ~3 and ~4 calls.

 This function has an additional feature compared to the SolveHYPRE call above,
 namely to use a block diagonal approximation of A for the preconditioner setup.
 This mimics the behavior of the BILUn preconditioners in Elmer, although any   
 preconditioner (like ParaSails or BoomerAMG) can still be used in combination  
 with block diagonal approximation. 
 BILU=0 or 1 - use A. 
 BILU=k - assume k equations and use block diagonal A with k blocks.
*/
void STDCALLBULL FC_FUNC(solvehypre1,SOLVEHYPRE1)
 (
  int *nrows,int *rows, int *cols, double *vals,
  int *globaldofs, int *owner,
  int *ILUn, int *BILU,
  int *hypre_method, int *hypre_intpara, double *hypre_dppara,
  int *verbosityPtr,
  int** ContainerPtr
 )
{
   int i, j, k, *rcols;
   int myid, num_procs;
   int N, n, csize=128;

   int ilower, iupper;
   int local_size, extra;
   ElmerHypreContainer* Container;

   HYPRE_IJMatrix A, Atilde;
   HYPRE_ParCSRMatrix parcsr_A;
   HYPRE_IJVector b;
   HYPRE_ParVector par_b;
   HYPRE_IJVector x;
   HYPRE_ParVector par_x;

   HYPRE_Solver solver, precond;

   double  *txvec, st, realtime_();
   
   int verbosity = *verbosityPtr;

  /* which process number am I? */
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   
   if (myid==0) FPRINTF(STDOUT,"HYPRE Setup\n");

   if (*ContainerPtr != 0)
     {
     fprintf( stderr, "ID no. %i: pointer passed into SolveHypre1 not NULL, possible memory leak.\n", myid);
     }

   Container = (ElmerHypreContainer*)malloc(sizeof(ElmerHypreContainer));

   *ContainerPtr=(int*)(Container);

st  = realtime_();

   /* How many rows do I have? */
   local_size = *nrows;

   ilower=1000000000;
   iupper=0;
   for( i=0; i<local_size; i++ )
   {
      if ( owner[i] ) {
        if ( iupper < globaldofs[i] ) iupper=globaldofs[i];
        if ( ilower > globaldofs[i] ) ilower=globaldofs[i];
      }
   }

   /* Create the matrix.
      Note that this is a square matrix, so we indicate the row partition
      size twice (since number of rows = number of cols) */
   HYPRE_IJMatrixCreate(MPI_COMM_WORLD, ilower, iupper, ilower, iupper, &A);

   /* Choose a parallel csr format storage (see the User's Manual) */
   HYPRE_IJMatrixSetObjectType(A, HYPRE_PARCSR);

   /* Initialize before setting coefficients */
   HYPRE_IJMatrixInitialize(A);

   /* Now go through my local rows and set the matrix entries.
      Note that here we are setting one row at a time, though
      one could set all the rows together (see the User's Manual).
   */
   {
      int nnz,irow,i,j,k,*rcols;

      rcols = (int *)malloc( csize*sizeof(int) );
      for (i = 0; i < local_size; i++)
      {
         nnz = rows[i+1]-rows[i];
         if ( nnz>csize ) {
           csize = nnz+csize;
           rcols = (int *)realloc( rcols, csize*sizeof(int) );
         }
         irow=globaldofs[i];
         for( k=0,j=rows[i]; j<rows[i+1]; j++,k++)
         {
           rcols[k] = globaldofs[cols[j-1]-1];
         }
         HYPRE_IJMatrixAddToValues(A, 1, &nnz, &irow, rcols, &vals[rows[i]-1]);
      }
   free( rcols );
   }

   /* Assemble after setting the coefficients */
   HYPRE_IJMatrixAssemble(A);

   if (*BILU<=1)
     {
     Atilde = A;
     }
   else
     {
     int nnz,irow,jcol,i,j,k,*rcols;
     double *dbuf;
     if (myid==0) FPRINTF(STDOUT,"HYPRE: using BILU(%d) approximation for preconditioner\n",*BILU);

     HYPRE_IJMatrixCreate(MPI_COMM_WORLD, ilower, iupper, ilower, iupper, &Atilde);
     HYPRE_IJMatrixSetObjectType(Atilde, HYPRE_PARCSR);
     HYPRE_IJMatrixInitialize(Atilde);

     rcols = (int *)malloc( csize*sizeof(int) );
     dbuf = (double *)malloc( csize*sizeof(double) );
     for (i = 0; i < local_size; i++)
       {
       irow=globaldofs[i];
       nnz = 0;
       for (j=rows[i];j<rows[i+1];j++)
         {
         jcol = globaldofs[cols[j-1]-1];
         /*TODO - is the block ordering preserved in the linear numbering?
                  Here we assume it is.
          */
         if ((irow%*BILU)==(jcol%*BILU))
           {
           rcols[nnz] = jcol;
           dbuf[nnz] = vals[j-1];
           nnz++;
           }
         }
       HYPRE_IJMatrixAddToValues(Atilde, 1, &nnz, &irow, rcols, dbuf);
       }
     free( rcols );
     free( dbuf );
     /* Assemble after setting the coefficients */
     HYPRE_IJMatrixAssemble(Atilde);     
     }

   /* Get the parcsr matrix object to use */
   /* note: this is only used for setup,  */
   /* so we put in the possibly approxima-*/
   /* ted matrix Atilde                   */
   HYPRE_IJMatrixGetObject(Atilde, (void**) &parcsr_A);

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&b);
   HYPRE_IJVectorSetObjectType(b, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(b);
   HYPRE_IJVectorAssemble(b);
   HYPRE_IJVectorGetObject(b, (void **) &par_b);

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&x);
   HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(x);
   HYPRE_IJVectorAssemble(x);
   HYPRE_IJVectorGetObject(x, (void **) &par_x);

/*    FPRINTF(stderr,"HYRPE INT: %d %d  %d %d %d \n", hypre_intpara[0], hypre_intpara[1], hypre_intpara[2], hypre_intpara[3], hypre_intpara[4]);  */
/*    FPRINTF(stderr,"HYRPE DP: %d %d %d %d %d \n", hypre_dppara[0], hypre_dppara[1], hypre_dppara[2], hypre_dppara[3], hypre_dppara[4]);  */
   /* Choose a solver and solve the system */
   /* NB.: hypremethod = 0 ... BiCGStab + ILUn
                         1 ... BiCGStab + ParaSails
                         2 ... BiCGStab + BoomerAMG
                        10 ... BoomerAMG 
                        20 ... CG + ILUn 
                        21 ... CG + ParaSails 
                        22 ... CG + ParaSails
   */
   
   /* create preconditioner for Krylov methods */
   /* for Boomer as solver we create it as a   */
   /* preconditioner here and set the pointer  */
   if (*hypre_method%10 == 0 )      
       {
       HYPRE_EuclidCreate( MPI_COMM_WORLD, &precond );
       static char *argv[5], str[3];
       argv[0] = "-level";
       sprintf( str, "%d", *ILUn );
       if (myid == 0) FPRINTF( stderr,"SolveHypre: using ILU%i\n",*ILUn); 
       argv[1] = str;
       HYPRE_EuclidSetParams( precond, 2, argv );
       }
     else if (*hypre_method%10 == 1 )
       {
       if (myid == 0) FPRINTF( stderr,"SolveHypre: using ParaSails\n"); 
       /* Now set up the ParaSails preconditioner and specify any parameters */
       HYPRE_ParaSailsCreate(MPI_COMM_WORLD, &precond);
       /* Set some parameters (See Reference Manual for more parameters) */
       /* threshold = dppara[0]; maxlevels= intpara[1] */
       HYPRE_ParaSailsSetParams(precond, hypre_dppara[0], hypre_intpara[1]);
       /* filter = dppara[1] */
       HYPRE_ParaSailsSetFilter(precond, hypre_dppara[1]);
       /* symmetry = intpara[0] */
       HYPRE_ParaSailsSetSym(precond, hypre_intpara[0]);
       if (verbosity>LOGLEVEL)
           {
           HYPRE_ParaSailsSetLogging(precond, 3);
           }
         else
	   {
           HYPRE_ParaSailsSetLogging(precond, 0);
           }
         
       }
     else if ((*hypre_method%10 == 2) || (*hypre_method == 10))
       {
       if (myid == 0) {
	 FPRINTF( stderr,"SolveHypre: using BoomerAMG\n");
	 FPRINTF( stderr,"RelaxType=%d\n",hypre_intpara[0]); 
	 FPRINTF( stderr,"CoarsenType=%d\n",hypre_intpara[1]); 
	 FPRINTF( stderr,"NumSweeps=%d\n",hypre_intpara[2]); 
	 FPRINTF( stderr,"MaxLevels=%d\n",hypre_intpara[3]); 
	 FPRINTF( stderr,"Interpolation Type=%d\n",hypre_intpara[4]); 
	 FPRINTF( stderr,"Smooth Type=%d\n",hypre_intpara[5]);
	 FPRINTF( stderr,"Cycle Type=%d\n",hypre_intpara[6]);
	 FPRINTF( stderr,"DOFs=%d\n",hypre_intpara[7]);
       }
       HYPRE_BoomerAMGCreate(&precond);
       /* Set some parameters (See Reference Manual for more parameters) */
       HYPRE_BoomerAMGSetNumFunctions(precond, hypre_intpara[7]); /* No. of PDE's */
       if (verbosity>PRINTLEVEL)
         {
         HYPRE_BoomerAMGSetPrintLevel(precond, 1); /* print amg solution info */
         }
       else
         {
         HYPRE_BoomerAMGSetPrintLevel(precond, 0); 
         }
       HYPRE_BoomerAMGSetNumSweeps(precond, 1); /* fixed for preconditioner to 1 */
       HYPRE_BoomerAMGSetTol(precond, 0.0); /* conv. tolerance zero */
       HYPRE_BoomerAMGSetMaxIter(precond, 1); /* do only one iteration! */
       HYPRE_BoomerAMGSetRelaxType(precond, hypre_intpara[0]);   /* G-S/Jacobi hybrid relaxation */
       HYPRE_BoomerAMGSetCoarsenType(precond, hypre_intpara[1]);  /* coarsening type */
	 
       HYPRE_BoomerAMGSetMaxLevels(precond, hypre_intpara[3]); /* levels of coarsening */
       HYPRE_BoomerAMGSetInterpType(precond, hypre_intpara[4]);  /* interpolation type */
       HYPRE_BoomerAMGSetSmoothType(precond, hypre_intpara[5]);  /* smoother type */
     } else if (*hypre_method%10!=9) {
       fprintf( stderr,"Hypre preconditioning method not implemented\n");
       exit(EXIT_FAILURE);
     }

   /* create solver */
   if ( *hypre_method < 10) { /* BiGSTAB methods */
     /* Create solver */
     HYPRE_ParCSRBiCGSTABCreate(MPI_COMM_WORLD, &solver);

     /* Set some parameters (See Reference Manual for more parameters) */
     HYPRE_ParCSRBiCGSTABSetStopCrit(solver, 0);     /* use the two norm as the stopping criteria */
     if (verbosity>PRINTLEVEL)
       {
       HYPRE_ParCSRBiCGSTABSetPrintLevel(solver, 2);   /* print solve info */
       }
     else
       {
       HYPRE_ParCSRBiCGSTABSetPrintLevel(solver, 0);   
       }
     
     if (verbosity>LOGLEVEL)
       {
       HYPRE_ParCSRBiCGSTABSetLogging(solver, 1);      /* needed to get run info later */
       }
     else
       {
       HYPRE_ParCSRBiCGSTABSetLogging(solver, 0);      /* needed to get run info later */
       }

     if (hypre_method==0)
       {
       /* Set the PCG preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_EuclidSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_EuclidSetup, precond);
     } else if (*hypre_method == 1) { 
       /* Set the PCG preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSetup, precond);
     } else if(*hypre_method == 2){
       /* Set the BiCGSTAB preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSetup, precond);
       }
   /* compute the preconditioner */
   if (myid==0) FPRINTF(STDOUT,"create preconditioner...");
   HYPRE_ParCSRBiCGSTABSetup(solver, parcsr_A, par_b, par_x);

   } else if ( *hypre_method == 10 ) { /* boomer AMG */
      int num_iterations;
      double final_res_norm;
      
      solver = precond;
      precond = NULL;

      /* Now setup - note that the input vectors are ignored so we can pass in NULLs */
      if (myid==0) FPRINTF(STDOUT,"construct BoomerAMG solver");
      HYPRE_BoomerAMGSetup(solver, parcsr_A, par_b, par_x);
     }
   else if ((int)(*hypre_method/10)==2) { /* CG */
     /* Create solver */
     HYPRE_ParCSRPCGCreate(MPI_COMM_WORLD, &solver);

     /* Set some parameters (See Reference Manual for more parameters) */
     HYPRE_ParCSRPCGSetTwoNorm(solver, 1);     /* use the two norm as the stopping criteria */
     if (verbosity>PRINTLEVEL)
       {
       HYPRE_ParCSRPCGSetPrintLevel(solver, 2);   /* print solve info */
       }
     else
       {
       HYPRE_ParCSRPCGSetPrintLevel(solver, 0);   
       }
     
     if (verbosity>LOGLEVEL)
       {
       HYPRE_ParCSRPCGSetLogging(solver, 1);      /* needed to get run info later */
       }
     else
       {
       HYPRE_ParCSRPCGSetLogging(solver, 0);      /* needed to get run info later */
       }

     if (*hypre_method%10==0)
       {
       /* Set the PCG preconditioner */
       HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_EuclidSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_EuclidSetup, precond);
     } else if (*hypre_method%10 == 1) { 
       /* Set the PCG preconditioner */
       HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSetup, precond);
     } else if(*hypre_method%10 == 2){
       /* Set the BiCGSTAB preconditioner */
       HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSetup, precond);
       }
   /* compute the preconditioner */
   if (myid==0) FPRINTF(STDOUT,"create preconditioner...");
   HYPRE_ParCSRPCGSetup(solver, parcsr_A, par_b, par_x);
     }
   else
     {
       fprintf( stderr,"Hypre solver method not implemented\n");
       exit(EXIT_FAILURE);
     }

Container->ilower = ilower;
Container->iupper = iupper;     
Container->hypre_method = *hypre_method;
Container->A = A;
Container->Atilde = Atilde;
Container->solver = solver;
Container->precond = precond;

/* FPRINTF( STDOUT, "ID no. %i: setup time: %g\n", myid, realtime_()-st ); */
if (myid==0) FPRINTF( STDOUT, "setup time: %g\n", myid, realtime_()-st );

}/* SolveHypre1 - matrix conversion and solver setup */

/*////////////////////////////////////////////////////////////////////////////////////////////////*/

/* solve a linear system with previously constructed solver and preconditioner */
void STDCALLBULL FC_FUNC(solvehypre2,SOLVEHYPRE2)
 (
  int *nrows, int *globaldofs, int *owner,  double *xvec,
  double *rhsvec, int *Rounds, double *TOL,
  int *verbosityPtr, int** ContainerPtr
 )
{

   int i, j, k, *rcols;
   int myid, num_procs;
   int N, n;

   int ilower, iupper;
   int local_size, extra;

   int print_solution, print_system;

   double  *txvec, st, realtime_();
   
   HYPRE_Solver solver, precond;

   HYPRE_ParCSRMatrix parcsr_A;
   HYPRE_IJVector b;
   HYPRE_ParVector par_b;
   HYPRE_IJVector x;
   HYPRE_ParVector par_x;
   
   ElmerHypreContainer *Container;

   int verbosity = *verbosityPtr;

  Container = (ElmerHypreContainer*)(*ContainerPtr);

  /* which process number am I? */
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);

  if (myid==0) FPRINTF(STDOUT,"HYPRE Solve\n");

if (Container==NULL)
  {
  fprintf( stderr, "ID no. %i: pointer passed into SolveHypre2 is NULL, not solving",myid);
  return;
  }
  
  st = realtime_();

   HYPRE_IJMatrixGetObject(Container->A, (void**) &parcsr_A);
   solver = Container->solver;
   precond = Container->precond;

   ilower = Container->ilower;
   iupper = Container->iupper;
   local_size = *nrows;

   /* Create the rhs and solution */
   rcols = (int *)malloc( local_size*sizeof(int) );
   txvec = (double *)malloc( local_size*sizeof(double) );

   for( k=0,i=0; i<local_size; i++ ) rcols[k++] = globaldofs[i];

   for( i=0; i<local_size; i++ ) txvec[i] = rhsvec[i];

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&b);
   HYPRE_IJVectorSetObjectType(b, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(b);
   for( i=0; i<local_size; i++ ) txvec[i] = rhsvec[i];
   HYPRE_IJVectorAddToValues(b, local_size, rcols, txvec );

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&x);
   HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(x);
   for( i=0; i<local_size; i++ ) txvec[i] = xvec[i];
   HYPRE_IJVectorSetValues(x, local_size, rcols, txvec );

   HYPRE_IJVectorAssemble(b);
   HYPRE_IJVectorGetObject(b, (void **) &par_b);

   HYPRE_IJVectorAssemble(x);
   HYPRE_IJVectorGetObject(x, (void **) &par_x);

   if (Container->hypre_method<10) 
     {
     /* Now setup and solve! */
     HYPRE_ParCSRBiCGSTABSetMaxIter(solver, *Rounds); /* max iterations */
     HYPRE_ParCSRBiCGSTABSetTol(solver, *TOL);       /* conv. tolerance */
     HYPRE_ParCSRBiCGSTABSolve(Container->solver, parcsr_A, par_b, par_x);
     }
   else if (Container->hypre_method==10)
     {
      int num_iterations;
      double final_res_norm;

      HYPRE_BoomerAMGSetTol(solver, *TOL);      /* conv. tolerance */
      HYPRE_BoomerAMGSetMaxIter(solver, *Rounds); /* iteration rounds */
      HYPRE_BoomerAMGSolve(Container->solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
      HYPRE_BoomerAMGGetNumIterations(Container->solver, &num_iterations);
      HYPRE_BoomerAMGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
	FPRINTF(STDOUT,"BoomerAMG:\n");
	FPRINTF(STDOUT,"Iterations = %d\n", num_iterations);
	FPRINTF(STDOUT,"Final Relative Residual Norm = %e\n", final_res_norm);
	FPRINTF(STDOUT,"\n");
      }
     }
   else if ((int)(Container->hypre_method/10)==2) 
     {
     /* Now setup and solve! */
     HYPRE_ParCSRPCGSetMaxIter(solver, *Rounds); /* max iterations */
     HYPRE_ParCSRPCGSetTol(solver, *TOL);       /* conv. tolerance */
     HYPRE_ParCSRPCGSolve(Container->solver, parcsr_A, par_b, par_x);
     }

   for( k=0,i=0; i<local_size; i++ )
      if ( owner[i] ) rcols[k++] = globaldofs[i];
     
   HYPRE_IJVectorGetValues(x, k, rcols, txvec );

   for( i=0,k=0; i<local_size; i++ )
     if ( owner[i] ) xvec[i] = txvec[k++];

   if (myid==0) FPRINTF( STDOUT, "solve time: %g\n", myid, realtime_()-st );
/*   FPRINTF( STDOUT, "ID no. %i: solve time: %g\n", myid, realtime_()-st ); */
   free( txvec );
   free( rcols );

 HYPRE_IJVectorDestroy(x);
 HYPRE_IJVectorDestroy(b);

}


/*TODO - add function solvehypre3 that e..g updates the matrix in the
       Container and Krylov solver but leaves the preconditioner   
       unchanged.
*/

/* destroy HYPRE data structure stored in a fortran environment */
void STDCALLBULL FC_FUNC(solvehypre4,SOLVEHYPRE4)(int** ContainerPtr)
   {
   ElmerHypreContainer* Container = (ElmerHypreContainer*)(*ContainerPtr);
   if (Container==0) return;

   if ( Container->hypre_method == 10 ) { /* boomer AMG */

      /* Destroy solver */
      HYPRE_BoomerAMGDestroy(Container->solver);
     }
   else
     {
     /* Destroy solver and preconditioner */
     if (Container->hypre_method<10) {
       HYPRE_ParCSRBiCGSTABDestroy(Container->solver);
       }
     else if ((int)(Container->hypre_method/10)==2) {
       HYPRE_ParCSRPCGDestroy(Container->solver);
       }
     if ( Container->hypre_method % 10 == 0 ) {
       HYPRE_EuclidDestroy(Container->precond);
     } else if ( Container->hypre_method % 10 == 1 ) {
       HYPRE_ParaSailsDestroy(Container->precond);
     } else if (Container->hypre_method % 10 == 2 ) {
       HYPRE_BoomerAMGDestroy(Container->precond);
     }
   }
 if (Container->Atilde != Container->A)
   {
   HYPRE_IJMatrixDestroy(Container->Atilde);
   }
 free(Container);
 *ContainerPtr=NULL;
 }

#endif
