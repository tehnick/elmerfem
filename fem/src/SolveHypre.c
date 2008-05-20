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
         irow = invperm[i];
         irow = globaldofs[irow-1];
         for( k=0,j=rows[i]; j<rows[i+1]; j++,k++)
         {
           rcols[k] = invperm[cols[j-1]-1];
           rcols[k] = globaldofs[rcols[k]-1];
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
   for( i=0; i<local_size; i++ ) txvec[invperm[i]-1] = rhsvec[i];
   HYPRE_IJVectorAddToValues(b, local_size, rcols, txvec );

   HYPRE_IJVectorCreate(MPI_COMM_WORLD, ilower, iupper,&x);
   HYPRE_IJVectorSetObjectType(x, HYPRE_PARCSR);
   HYPRE_IJVectorInitialize(x);
   for( i=0; i<local_size; i++ ) txvec[invperm[i]-1] = xvec[i];
   HYPRE_IJVectorSetValues(x, local_size, rcols, txvec );

   HYPRE_IJVectorAssemble(b);
   HYPRE_IJVectorGetObject(b, (void **) &par_b);

   HYPRE_IJVectorAssemble(x);
   HYPRE_IJVectorGetObject(x, (void **) &par_x);
   fprintf( stderr, "ID no. %i: setup time: %g\n", myid, realtime_()-st );
   st = realtime_();


/*    fprintf(stderr,"HYRPE INT: %d %d  %d %d %d \n", hypre_intpara[0], hypre_intpara[1], hypre_intpara[2], hypre_intpara[3], hypre_intpara[4]);  */
/*    fprintf(stderr,"HYRPE DP: %d %d %d %d %d \n", hypre_dppara[0], hypre_dppara[1], hypre_dppara[2], hypre_dppara[3], hypre_dppara[4]);  */
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
	 if (myid == 0) fprintf( stderr,"SolveHypre: using BiCGStab + ILU%i\n",*ILUn); 
         argv[1] = str;
         HYPRE_EuclidSetParams( precond, 2, argv );
       }

       /* Set the PCG preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_EuclidSolve,
				(HYPRE_PtrToSolverFcn) HYPRE_EuclidSetup, precond);
     } else if (*hypre_method == 1) { 
       if (myid == 0) fprintf( stderr,"SolveHypre: using BiCGStab + paraSails\n"); 
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
	 fprintf( stderr,"SolveHypre: using BiCGStab + boomerAMG\n");
	 fprintf( stderr,"RelaxType=%d\n",hypre_intpara[0]); 
	 fprintf( stderr,"CoarsenType=%d\n",hypre_intpara[1]); 
	 fprintf( stderr,"NumSweeps=%d\n",hypre_intpara[2]); 
	 fprintf( stderr,"MaxLevels=%d\n",hypre_intpara[3]); 
	 fprintf( stderr,"Interpolation Type=%d\n",hypre_intpara[4]); 
	 fprintf( stderr,"Smooth Type=%d\n",hypre_intpara[5]);
	 fprintf( stderr,"Cycle Type=%d\n",hypre_intpara[6]);
	 fprintf( stderr,"DOFs=%d\n",hypre_intpara[7]);
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
     } else {
       fprintf( stderr,"Hypre precodnitioning mehtod not implemented\n");
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
	fprintf( stderr,"SolveHypre: using BoomerAMG\n"); 
	fprintf( stderr,"RelaxType=%d\n",hypre_intpara[0]); 
	fprintf( stderr,"CoarsenType=%d\n",hypre_intpara[1]); 
	fprintf( stderr,"NumSweeps=%d\n",hypre_intpara[2]); 
	fprintf( stderr,"MaxLevels=%d\n",hypre_intpara[3]); 
	fprintf( stderr,"Interpolation Type=%d\n",hypre_intpara[4]); 
	fprintf( stderr,"Smooth Type=%d\n",hypre_intpara[5]);
 	fprintf( stderr,"Cycle Type=%d\n",hypre_intpara[6]);
  	fprintf( stderr,"DOFs=%d\n",hypre_intpara[7]);
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
	fprintf(stderr,"BoomerAMG:\n");
	fprintf(stderr,"Iterations = %d\n", num_iterations);
	fprintf(stderr,"Final Relative Residual Norm = %e\n", final_res_norm);
	fprintf(stderr,"\n");
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
     if ( owner[i] ) xvec[perm[i]-1] = txvec[k++];

   fprintf( stderr, "ID no. %i: solve time: %g\n", myid, realtime_()-st );
   free( txvec );
   free( rcols );

   /* Clean up */
   HYPRE_IJMatrixDestroy(A);
   HYPRE_IJVectorDestroy(b);
   HYPRE_IJVectorDestroy(x);
}
#endif
