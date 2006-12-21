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
#include "utilities.h"
#include "krylov.h"
#include "HYPRE.h"
#include "HYPRE_parcsr_ls.h"

void solvehypre_( int *nrows,int *rows, int *cols, double *vals, int *perm,
      int *invperm, int *globaldofs, int *owner, double *xvec, double *rhsvec, int *dofs, int *pe )
{
   int i, j, k, *rcols;
   int myid, num_procs;
   int N, n;

   int ilower, iupper;
   int local_size, extra;

   int solver_id;
   int print_solution, print_system;

   double h, h2, *txvec, st, realtime_();

   HYPRE_IJMatrix A;
   HYPRE_ParCSRMatrix parcsr_A;
   HYPRE_IJVector b;
   HYPRE_ParVector par_b;
   HYPRE_IJVector x;
   HYPRE_ParVector par_x;

   HYPRE_Solver solver, precond;

   /* Default problem parameters */
   n = 33;
   solver_id = 0;
   print_solution  = 0;
   print_system = 0;

st  = realtime_();
   /* How many rows do I have? */
   local_size = *nrows;

// ilower = *dofs*globaldofs[0]-*dofs+1;
// iupper = *dofs*globaldofs[0];
   ilower=10000000000;
   iupper=0;
   for( i=0; i<local_size / *dofs; i++ )
   {
      if ( owner[i] ) {
        if ( iupper < *dofs*globaldofs[i] ) iupper=*dofs*globaldofs[i];
        if ( ilower > *dofs*(globaldofs[i]-1)+1 ) ilower=*dofs*(globaldofs[i]-1)+1;
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
      int nnz,irow,i,j,k,*rcols,csize=32;

      rcols = (int *)malloc( csize*sizeof(int) );
      for (i = 0; i < local_size; i++)
      {
         nnz = rows[i+1]-rows[i];
         if ( nnz>csize ) {
           rcols = (int *)realloc( rcols, nnz*sizeof(int) );
           csize = nnz;
         }
         irow = (invperm[i]+*dofs-1) / *dofs;
         irow = *dofs*(globaldofs[irow-1]-1)+i%*dofs+1;
         for( k=0,j=rows[i]; j<rows[i+1]; j++,k++)
         {
           rcols[k] = (invperm[cols[j-1]-1]+*dofs-1) / *dofs;
           rcols[k] = *dofs*(globaldofs[rcols[k]-1]-1)+(cols[j-1]-1)%*dofs+1;
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
   for( k=0,i=0; i < local_size / *dofs; i++ )
   {
      for( j=1; j<=*dofs; j++,k++ ) rcols[k] = *dofs*(globaldofs[i]-1)+j;
   }

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
fprintf( stderr, "setup time: %g\n", realtime_()-st );
st = realtime_();

   /* Choose a solver and solve the system */

   solver_id = 2;
   /* AMG */
   if (solver_id == 0)
   {
      int num_iterations;
      double final_res_norm;

      /* Create solver */
      HYPRE_BoomerAMGCreate(&solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_BoomerAMGSetPrintLevel(solver, 3);  /* print solve info + parameters */
      HYPRE_BoomerAMGSetCoarsenType(solver, 6); /* Falgout coarsening */
      HYPRE_BoomerAMGSetRelaxType(solver, 3);   /* G-S/Jacobi hybrid relaxation */
      HYPRE_BoomerAMGSetNumSweeps(solver, 1);   /* Sweeeps on each level */
      HYPRE_BoomerAMGSetMaxLevels(solver, 20);  /* maximum number of levels */
      HYPRE_BoomerAMGSetTol(solver, 1e-7);      /* conv. tolerance */

      /* Now setup and solve! */
      HYPRE_BoomerAMGSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_BoomerAMGSolve(solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
      HYPRE_BoomerAMGGetNumIterations(solver, &num_iterations);
      HYPRE_BoomerAMGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", num_iterations);
         printf("Final Relative Residual Norm = %e\n", final_res_norm);
         printf("\n");
      }

      /* Destroy solver */
      HYPRE_BoomerAMGDestroy(solver);
   }
   /* PCG */
   else if (solver_id == 50)
   {
      int num_iterations;
      double final_res_norm;

      /* Create solver */
      HYPRE_ParCSRPCGCreate(MPI_COMM_WORLD, &solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_PCGSetMaxIter(solver, 1000); /* max iterations */
      HYPRE_PCGSetTol(solver, 1e-7); /* conv. tolerance */
      HYPRE_PCGSetTwoNorm(solver, 1); /* use the two norm as the stopping criteria */
      HYPRE_PCGSetPrintLevel(solver, 2); /* prints out the iteration info */
      HYPRE_PCGSetLogging(solver, 1); /* needed to get run info later */

      /* Now setup and solve! */
      HYPRE_ParCSRPCGSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_ParCSRPCGSolve(solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
      HYPRE_PCGGetNumIterations(solver, &num_iterations);
      HYPRE_PCGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", num_iterations);
         printf("Final Relative Residual Norm = %e\n", final_res_norm);
         printf("\n");
      }

      /* Destroy solver */
      HYPRE_ParCSRPCGDestroy(solver);
   }
   /* PCG with AMG preconditioner */
   else if (solver_id == 1)
   {
      int num_iterations;
      double final_res_norm;

      /* Create solver */
      HYPRE_ParCSRPCGCreate(MPI_COMM_WORLD, &solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_PCGSetMaxIter(solver, 1000); /* max iterations */
      HYPRE_PCGSetTol(solver, 1e-7); /* conv. tolerance */
      HYPRE_PCGSetTwoNorm(solver, 1); /* use the two norm as the stopping criteria */
      HYPRE_PCGSetPrintLevel(solver, 2); /* print solve info */
      HYPRE_PCGSetLogging(solver, 1); /* needed to get run info later */

#if 1
      /* Now set up the AMG preconditioner and specify any parameters */
      HYPRE_BoomerAMGCreate(&precond);
      HYPRE_BoomerAMGSetPrintLevel(precond, 0); /* print amg solution info */
      HYPRE_BoomerAMGSetCoarsenType(precond, 6);
      HYPRE_BoomerAMGSetRelaxType(precond, 6); /* Sym G.S./Jacobi hybrid */ 
      HYPRE_BoomerAMGSetNumSweeps(precond, 1);
      HYPRE_BoomerAMGSetTol(precond, 0.0); /* conv. tolerance zero */
      HYPRE_BoomerAMGSetMaxIter(precond, 1); /* do only one iteration! */

      /* Set the PCG preconditioner */
      HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSolve,
             (HYPRE_PtrToSolverFcn) HYPRE_BoomerAMGSetup, precond);
#else
      HYPRE_ParCSRPilutCreate(MPI_COMM_WORLD, &precond );
      HYPRE_ParCSRPilutSetMaxIter( precond, 1000 );
      HYPRE_ParCSRPilutSetDropTolerance( precond, 0.0 );
      HYPRE_ParCSRPilutSetFactorRowSize( precond, 2500 );

      /* Set the PCG preconditioner */
       HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_ParCSRPilutSolve,
              (HYPRE_PtrToSolverFcn) HYPRE_ParCSRPilutSetup, precond);
#endif


      /* Now setup and solve! */
      HYPRE_ParCSRPCGSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_ParCSRPCGSolve(solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
      HYPRE_PCGGetNumIterations(solver, &num_iterations);
      HYPRE_PCGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", num_iterations);
         printf("Final Relative Residual Norm = %e\n", final_res_norm);
         printf("\n");
      }

      /* Destroy solver and preconditioner */
      HYPRE_ParCSRPCGDestroy(solver);
#if 0
      HYPRE_BoomerAMGDestroy(precond);
#else
      HYPRE_ParCSRPilutDestroy(precond);
#endif
   }
   /* bicg with AMG preconditioner */
   else if (solver_id == 2)
   {
      int num_iterations;
      double final_res_norm;

      /* Create solver */
      HYPRE_ParCSRBiCGSTABCreate(MPI_COMM_WORLD, &solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_ParCSRBiCGSTABSetMaxIter(solver, 1000); /* max iterations */
      HYPRE_ParCSRBiCGSTABSetTol(solver, 1e-7); /* conv. tolerance */
      HYPRE_ParCSRBiCGSTABSetStopCrit(solver, 1); /* use the two norm as the stopping criteria */
      HYPRE_ParCSRBiCGSTABSetPrintLevel(solver, 2); /* print solve info */
      HYPRE_ParCSRBiCGSTABSetLogging(solver, 1); /* needed to get run info later */

      HYPRE_EuclidCreate( MPI_COMM_WORLD, &precond );
      {
          char *argv[5];
          argv[0] = "-level";
          argv[1] = "3";
          HYPRE_EuclidSetParams( precond, 2, argv );
      }

      /* Set the BiCGSTAB preconditioner */
       HYPRE_BiCGSTABSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_EuclidSolve,
              (HYPRE_PtrToSolverFcn) HYPRE_EuclidSetup, precond);

      /* Now setup and solve! */
      HYPRE_ParCSRBiCGSTABSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_ParCSRBiCGSTABSolve(solver, parcsr_A, par_b, par_x);

      /* Run info - needed logging turned on */
/*
      HYPRE_BiCGSTABGetNumIterations(solver, &num_iterations);
      HYPRE_BiCGSTABGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (*pe == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", num_iterations);
         printf("Final Relative Residual Norm = %e\n", final_res_norm);
         printf("\n");
      }
*/

      /* Destroy solver and preconditioner */
      HYPRE_ParCSRBiCGSTABDestroy(solver);
      HYPRE_EuclidDestroy(precond);
   }
   /* PCG with Parasails Preconditioner */
   else if (solver_id == 8)
   {
      int    num_iterations;
      double final_res_norm;

      int      sai_max_levels = 1;
      double   sai_threshold = 0.1;
      double   sai_filter = 0.05;
      int      sai_sym = 1;

      /* Create solver */
      HYPRE_ParCSRPCGCreate(MPI_COMM_WORLD, &solver);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_PCGSetMaxIter(solver, 1000); /* max iterations */
      HYPRE_PCGSetTol(solver, 1e-7); /* conv. tolerance */
      HYPRE_PCGSetTwoNorm(solver, 1); /* use the two norm as the stopping criteria */
      HYPRE_PCGSetPrintLevel(solver, 2); /* print solve info */
      HYPRE_PCGSetLogging(solver, 1); /* needed to get run info later */

      /* Now set up the ParaSails preconditioner and specify any parameters */
      HYPRE_ParaSailsCreate(MPI_COMM_WORLD, &precond);

      /* Set some parameters (See Reference Manual for more parameters) */
      HYPRE_ParaSailsSetParams(precond, sai_threshold, sai_max_levels);
      HYPRE_ParaSailsSetFilter(precond, sai_filter);
      HYPRE_ParaSailsSetSym(precond, sai_sym);
      HYPRE_ParaSailsSetLogging(precond, 3);

      /* Set the PCG preconditioner */
      HYPRE_PCGSetPrecond(solver, (HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSolve,
                          (HYPRE_PtrToSolverFcn) HYPRE_ParaSailsSetup, precond);

      /* Now setup and solve! */
      HYPRE_ParCSRPCGSetup(solver, parcsr_A, par_b, par_x);
      HYPRE_ParCSRPCGSolve(solver, parcsr_A, par_b, par_x);


      /* Run info - needed logging turned on */
      HYPRE_PCGGetNumIterations(solver, &num_iterations);
      HYPRE_PCGGetFinalRelativeResidualNorm(solver, &final_res_norm);
      if (myid == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", num_iterations);
         printf("Final Relative Residual Norm = %e\n", final_res_norm);
         printf("\n");
      }

      /* Destory solver and preconditioner */
      HYPRE_ParCSRPCGDestroy(solver);
      HYPRE_ParaSailsDestroy(precond);
   }
   else
   {
      if (myid ==0) printf("Invalid solver id specified.\n");
   }

   for( k=0,i=0; i<local_size/ *dofs; i++ )
   {
      if ( owner[i] ) {
        for( j=1; j<=*dofs; j++,k++ ) rcols[k] = *dofs*(globaldofs[i]-1)+j;
      }
   }

   HYPRE_IJVectorGetValues(x, k, rcols, txvec );

   for( i=0,k=0; i<local_size/ *dofs; i++ ) {
     if ( owner[i] ) {
        for( j=0; j<*dofs; j++,k++ ) {
         xvec[*dofs*(perm[i]-1)+j] = txvec[k];
        }
      }
   }
fprintf( stderr, "solve time: %g\n", realtime_()-st );
   free( txvec );
   free( rcols );

   /* Clean up */
   HYPRE_IJMatrixDestroy(A);
   HYPRE_IJVectorDestroy(b);
   HYPRE_IJVectorDestroy(x);
}
#endif
