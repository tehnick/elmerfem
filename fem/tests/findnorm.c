#include <stdio.h>
#include <ctype.h>

#define MAX_LENGTH 255

int main(int argc, char **argv)
{
   FILE *fp = fopen( argv[1], "r" );
   char line[MAX_LENGTH+1], *ptr;

   double norm, f;
   int n;

   norm = 0.0;
   while( fgets( line, MAX_LENGTH, fp ) )
   {
      ptr = line;
      while( *ptr != '\0' && *ptr != '.' && !isdigit(*ptr) ) ptr++;
      n = sscanf( ptr, "%lf", &f );
      if ( n==1 && f != 0.0 )  norm = f;
   }

   fprintf( stdout, "%.12g\n", norm );
}
