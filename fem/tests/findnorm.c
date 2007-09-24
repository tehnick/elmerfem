#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

#define MAX_LENGTH 255

int main(int argc, char **argv)
{
   FILE *fp = fopen( argv[1], "r" );
   char line[MAX_LENGTH+1], *ptr;

   static double norm, f, target_nrm, target_eps = 1e-5;
   int n, case_ind = 0, success;

   success = 0;
   while( fgets( line, MAX_LENGTH, fp ) )
   {
      if ( strstr( line, "END TEST CASE" ) ) {
         ptr = strstr( line, "NRM=" );         
         if ( ptr ) sscanf( ptr,"NRM=%lf", &target_nrm );
         ptr = strstr( line, "EPS=" );         
         if ( ptr ) sscanf( ptr,"EPS=%lf", &target_eps );         
         case_ind++;
         success = compare( norm, target_nrm, target_eps );
         if ( !success ) break;
      }
      else if ( strstr( line, "(NRM,RELC)" ) ) {
        ptr = line;
        while( *ptr != '\0' && *ptr!='+' && *ptr != '-' && *ptr != '.' && !isdigit(*ptr) ) ptr++;
        n = sscanf( ptr, "%lf", &f );
        if ( n==1 && f != 0.0 ) norm = f;
      }
   }

   fprintf( stdout, "%d\n", success );
}



int compare( double norm1, double norm2, double eps )
{
   int n;

   if ( norm1 != -1 ) {
      if ( eps < 0 ) { 
         if ( norm2 < norm1 )
            return 1;
         else
            return 0;
      } else  if ( 2 * fabs(norm1-norm2) / (norm1+norm2) < eps )
         return 1;
      else
         return 0;
   } else {
      return 1;
   }
}
