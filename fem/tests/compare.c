#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define MAX_LENGTH 255

int main(int argc, char **argv)
{
   double norm1, norm2, eps;
   int n;

   norm2 = atof( argv[1] );
   n = sscanf( argv[2], "%lf %lf", &norm1, &eps );
   if ( n != 2 ) {
     if ( argc>3 ) eps=atof( argv[3] ); else eps=1.0e-5;
   }

   if ( norm1 != -1 ) {
      if ( eps < 0 ) { 
         if ( norm2 < norm1 )
            fprintf( stdout, "1\n" );
         else
            fprintf( stdout, "0\n" );
      } else  if ( 2 * fabs(norm1-norm2) / (norm1+norm2) < eps )
         fprintf( stdout, "1\n" );
      else
         fprintf( stdout, "0\n" );
   } else {
      fprintf( stdout, "1\n" );
   }
}
