/* -------------------------------:  COMMON.C  :----------------------------

   Includes common operations for operating vectors and such.
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "common.h" 


void bigerror(char error_text[])
{
  fprintf(stderr,"The program encountered a major error...\n");
  fprintf(stderr,"%s\n",error_text);
  fprintf(stderr,"...now exiting to system...\n");
  exit(1);
}


void smallerror(char error_text[])
{
  fprintf(stderr,"The program encountered a minor error...\n");
  fprintf(stderr,"%s\n",error_text);
  fprintf(stderr,"...we'll try to continue...\n");
  exit(1);
}



int FileExists(char *filename)
{
  FILE *in;

  if((in = fopen(filename,"r")) == NULL) 
    return(FALSE);  
  else {
    fclose(in);
    return(TRUE);
  }
}


Real Minimum(Real *vector,int first,int last)
/* Returns the smallest value of vector in range [first,last]. */
{
  Real min;
  int i;

  min=vector[first];
  for(i=first+1;i<=last;i++)
    if(min>vector[i]) min=vector[i];

  return(min);
}


int Minimi(Real *vector,int first,int last)
/* Returns the position of the smallest value of vector in range [first,last]. */
{
  Real min;
  int i,mini;

  min=vector[first];
  for(i=first+1;i<=last;i++)
    if(min>vector[i]) 
      min=vector[(mini=i)];

  return(mini);
}


Real Maximum(Real *vector,int first,int last)
/* Returns the largest value of vector in range [first,last]. */
{
  Real max;
  int i;

  max=vector[first];
  for(i=first+1;i<=last;i++)
    if(max<vector[i]) max=vector[i];

  return(max);
}


int Maximi(Real *vector,int first,int last)
/* Returns the position of the largest value of vector in range [first,last]. */
{
  Real max;
  int i,maxi;

  max=vector[first];
  for(i=first+1;i<=last;i++) 
    if(max<vector[i]) 
      max=vector[(maxi=i)];

  return(maxi);
}



void InspectVector(Real *vector,int first,int last,Real *min,
		    Real *max,int *mini,int *maxi)
/* Returns the value and position of the smallest and largest element
   in vector[first,last]. 
   */
{
  int i;
  *min  = vector[first];
  *mini = first;
  *max  = vector[first];
  *maxi = first;

  for(i=first+1;i<=last;i++) {
    if (vector[i]>*max) {
      *max=vector[i];
      *maxi=i;
    }
    if (vector[i]<*min) {
      *min=vector[i];
      *mini=i;
    }
  }
}


int Steepest(Real *vector,int first,int last)
/* Finds the position where vector is at its steepest */
{
  int i,steep;
  Real aid,sub=0;

  for(i=first;i<last;i++) {
    aid=fabs(vector[i+1]-vector[i]);
    if ( aid > sub) {
      sub=aid;
      steep=i;
    }
  }
  return(steep);
}


Real MeanVector(Real *vector,int first,int last)
/* Calculates the mean of vector[first,last] */
{
  Real sum=0.0;
  int i;

  for(i=first;i<=last;i++)
    sum += vector[i];

  return(sum/(last-first+1));
}



Real AbsMeanVector(Real *vector,int first,int last)
/* Calculates the absolute mean of vector[first,last] */
{
  Real sum=0.0;
  int i;

  for(i=first;i<=last;i++)
    sum += fabs(vector[i]);

  return(sum/(last-first+1));
}


Real DifferVector(Real *vector1,Real *vector2,int first,int last)
/* Calcultes the mean of the relative difference of two vectors */
{
  Real sum=0.0, eps=1.0E-50;
  int i,n;

  for (i=first;i<=last;i++) {
      if ( fabs(vector1[i]+vector2[i]) > eps)
      sum += fabs(2*(vector1[i]-vector2[i])/(vector1[i]+vector2[i]));
    }
  n=last-first+1;

  return ( sum/(Real)(n) );
}


void ReformVector(Real *vector1,int n1,Real *vector2,int n2)
/* Adjusts the values of a vector to another vector with a different number
   of elements 
   */
{
  int i1,i2;
  Real x1,d1;

  for(i2=0;i2<n2;i2++) {
    x1=(n1)*((Real)(i2)/(Real)(n2));
    i1=(int)(x1);
    d1=x1-(Real)(i1);
    vector2[i2]=d1*vector1[i1+1]+(1.0-d1)*vector1[i1];
  }
  vector2[n2]=vector1[n1];
}



void AdjustVector(Real max,Real min,Real *vector,int first,int last)
/* Scales the values of a vector to range [min,max] using a linear model. */
{
  int i;
  Real oldmax,oldmin;

  oldmax=vector[first];
  oldmin=oldmax;
  for(i=first+1;i<=last;i++) {
    if (oldmax < vector[i]) 
      oldmax=vector[i];
    if (oldmin > vector[i])
      oldmin=vector[i];
  }
  for(i=first;i<=last;i++)
    vector[i]=(max-min)*(vector[i]-oldmin)/(oldmax-oldmin)+min;
}



int ReadRealVector(Real *vector,int first,int last,char *filename)
/* Reads a Real vector from an ascii-file with a given name. */
{
  int i;
  FILE *in;
  Real num;

  if ((in = fopen(filename,"r")) == NULL) {
    printf("The opening of the real vector file '%s' wasn't succesfull !\n",filename);
    return(1);
  }
  for(i=first;i<=last;i++) {
    fscanf(in,"%le\n",&num);
    vector[i]=num;
    }
  fclose(in);

  return(0);
}



void SaveRealVector(Real *vector,int first,int last,char *filename)
/* Saves an Real vector to an ascii-file with a given name. */
{
  int i;
  FILE *out;

  out = fopen(filename,"w");
  for (i=first;i<=last;i++) {
    fprintf(out,"%6le",vector[i]);
    fprintf(out,"\n");
    }
  fclose(out);
}



int ReadIntegerVector(int *vector,int first,int last,char *filename)
{
  int i;
  FILE *in;
  int num;

  if ((in = fopen(filename,"r")) == NULL) {
    printf("The opening of the int vector file '%s' wasn't succesfull !\n",filename);
    return(1);
  }
  for(i=first;i<=last;i++) {
    fscanf(in,"%d\n",&num);
    vector[i]=num;
    }
  fclose(in);

  return(0);
}



void SaveIntegerVector(int *vector,int first,int last,char *filename)
{
  int i;
  FILE *out;

  out = fopen(filename,"w");
  for (i=first;i<=last;i++) {
    fprintf(out,"%d",vector[i]);
    fprintf(out,"\n");
    }
  fclose(out);
}



int ReadRealMatrix(Real **matrix,int row_first,int row_last,
		int col_first,int col_last,char *filename)
{
  int i,j;
  FILE *in;
  Real num;

  if ((in = fopen(filename,"r")) == NULL) {
    printf("The opening of the real matrix file '%s' wasn't succesfull!\n",filename);
    return(1);
  }

  for(j=row_first;j<=row_last;j++) {
    for(i=col_first;i<=col_last;i++) {
      fscanf(in,"%le\n",&num);
      matrix[j][i]=num;
    }
  }
  fclose(in);

  return(0);
}



void SaveRealMatrix(Real **matrix,int row_first,int row_last,
		    int col_first,int col_last,char *filename)
{
  int i,j;
  FILE *out;

  out = fopen(filename,"w");
  for (j=row_first;j<=row_last;j++) {
    for (i=col_first;i<=col_last;i++) {
      fprintf(out,"%-14.6lg",matrix[j][i]);
      fprintf(out,"\t");
    }
    fprintf(out,"\n");
  }
  fclose(out);
}



int ReadIntegerMatrix(int **matrix,int row_first,int row_last,
		int col_first,int col_last,char *filename)
{
  int i,j;
  FILE *in;
  int num;

  if ((in = fopen(filename,"r")) == NULL) {
    printf("The opening of the int matrix file '%s' wasn't succesfull!\n",filename);
    return(FALSE);
  }

  for(j=row_first;j<=row_last;j++) {
    for(i=col_first;i<=col_last;i++) {
      fscanf(in,"%d\n",&num);
      matrix[j][i]=num;
    }
  }
  fclose(in);

  return(TRUE);
}



void SaveIntegerMatrix(int **matrix,int row_first,int row_last,
		int col_first,int col_last,char *filename)
{
  int i,j;
  FILE *out;

  out = fopen(filename,"w");
  for (j=row_first;j<=row_last;j++) {
    for (i=col_first;i<=col_last;i++) {
      fprintf(out,"%-8d",matrix[j][i]);
      fprintf(out,"\t");
    }
    fprintf(out,"\n");
  }
  fclose(out);
}


void AddExtension(const char *fname1,char *fname2,const char *newext)
/* Changes the extension of a filename.
   'fname1' - the original filename
   'fname2' - the new filename
   'newext' - the new extension
   If there is originally no extension it's appended. In this case 
   there has to be room for the extension. 
   */
{
  char *ptr1;

  strcpy(fname2,fname1); 
  ptr1 = strchr(fname2, '.');
  if (ptr1) *ptr1 = '\0';
  strcat(fname2, ".");
  strcat(fname2,newext);
}


void SaveNonZeros(Real **matrix,int row_first,int row_last,
		  int col_first,int col_last,char *filename)
/* Saves the nonzero elements in an file. */
{
  int i,j;
  FILE *out;
  Real nearzero=1.0e-20;

  out = fopen(filename,"w");
  for (j=row_first;j<=row_last;j++) 
    for (i=col_first;i<=col_last;i++) 
      if (fabs(matrix[j][i]) > nearzero) 
        fprintf(out,"%d\t %d\t %-12.6le\n",j,i,matrix[j][i]);
  
  fclose(out);
}


int StringToReal(const char *buf,Real *dest,int maxcnt,char separator)
/*  Finds real numbers separated by a certain separator from a string.
    'buf'       - input string ending to a EOF
    'dest'      - a vector of real numbers
    'maxcnt'    - maximum number of real numbers to be read
    'separator'	- the separator of real numbers
    The number of numbers found is returned in the function value.
    */
{
  int cnt = 0;
  char *ptr1 = (char *)buf, *ptr2;
  
  if (!buf[0]) return 0;
  do {
    ptr2 = strchr(ptr1,separator);
    if (ptr2) ptr2[0] = '\0';
    dest[cnt++] = atof(ptr1);
    if (ptr2) ptr1 = ptr2+1;
  } while (cnt < maxcnt && ptr2 != NULL);

  return cnt;
}


int StringToInteger(const char *buf,int *dest,int maxcnt,char separator)
{
  int cnt = 0;
  char *ptr1 = (char *)buf, *ptr2;

  if (!buf[0]) return 0;
  do {
    ptr2 = strchr(ptr1,separator);
    if (ptr2) ptr2[0] = '\0';
    dest[cnt++] = atoi(ptr1);
    if (ptr2) ptr1 = ptr2+1;
  } while (cnt < maxcnt && ptr2 != NULL);

  return cnt;
}


int EchoFile(char *filename)
#define LINELENGTH 100
{
  FILE *in;
  char line[LINELENGTH];

  if((in = fopen(filename,"r")) == NULL) {
    printf("Could not open file '%s'.\n",filename);
    return(1);
  }

  while(fgets(line,LINELENGTH,in) != NULL)
    printf("%s",line);

  fclose(in);
  return(0);
}


int next_int(char **start)
{
  int i;
  char *end;

  i = strtol(*start,&end,10);
  *start = end;
  return(i);
}


Real next_real(char **start)
{
  Real r;
  char *end;

  r = strtod(*start,&end);

  *start = end;
  return(r);
}


