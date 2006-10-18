#include <R.h>
#include <Rdefines.h>
#include <ctype.h>
#include <elmerparam.h>

SEXP elmerparam_rwrapper (SEXP nfun, SEXP xr, SEXP xi, SEXP tag)
{
  int nr, ni;
  int *ip, *nfunp;
  double *rp, *yp, *tp;
  char *tagp;
  SEXP y;
  int i;
  FILE *fd;

  PROTECT(nfun = AS_INTEGER(nfun));
  PROTECT(xi = AS_INTEGER(xi));
  ni = LENGTH(xi);
  PROTECT(xr = AS_NUMERIC(xr));
  nr = LENGTH(xr);
  PROTECT(tag = AS_CHARACTER(tag));

  nfunp = INTEGER_POINTER(nfun);
  ip = INTEGER_POINTER(xi);
  rp = NUMERIC_POINTER(xr);
  tagp = CHAR(tag);

  PROTECT(y = NEW_NUMERIC(*nfunp));
  yp = NUMERIC_POINTER(y);

  #if 0
  fd = fopen("fuling", "w");
  for (i = 0; i <= 255; i++) {
    if (isprint(tagp[i]))
      fprintf(fd,"%i: %c\n", i, tagp[i]);
    else
      fprintf(fd, "%i: :%i:\n", i, tagp[i]);
  }
  fclose(fd);
  #endif

  /* I have no idea if this way of passing tagp is safe, but it seems to work.*/
  elmer_param_vec(*nfunp, yp, nr, rp, ni, ip, &tagp[32]);

  UNPROTECT(5);

  return y;
}
