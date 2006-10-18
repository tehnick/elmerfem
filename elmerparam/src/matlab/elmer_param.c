#include <elmerparam.h>
#include "mex.h"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  int i;
  char *tag;
  int *xi, ni, nr;
  double *xr, *tmp, *y;
  int n, m, len, nfun;

  tag = NULL;
  xi = NULL;
  xr = NULL;
  ni = nr = 0;
  nfun = 1;

  switch (nrhs) {

  case 4:
    if (mxIsDouble(prhs[2]) != 1)
      mexErrMsgTxt("4:th argument must be a number.");
    n = mxGetN(prhs[2]);
    m = mxGetM(prhs[2]);
    /*
    if (n != 1 || m != 1)
      mexErrMsgTxt("4:rd argument must be a scalar");
    */

    tmp = mxGetPr(prhs[3]);
    nfun = (int) *tmp;

    /* Fall through.  */
  case 3:
    n = mxGetN(prhs[2]);
    m = mxGetM(prhs[2]);
    len = n*m+1;
    if (len > 1) { 
      if (mxIsChar(prhs[2]) != 1)
        mexErrMsgTxt("3:th argument must be a string.");
      tag = mxCalloc(len, sizeof(char));
      mxGetString(prhs[2], tag, len);
    }

    /* Fall through.  */
  case 2:
    if (!mxIsDouble(prhs[1]))
      mexErrMsgTxt("2:rd argument must numerical.");
    n = mxGetN(prhs[1]);
    m = mxGetM(prhs[1]);
    if (n > 1 && m > 1)
      mexErrMsgTxt("2:rd argument must be a vector.");

    ni = n*m;
    xi = mxCalloc(ni, sizeof(int));
    tmp = mxGetPr(prhs[1]);
    for (i = 0; i < ni; i++)
      xi[i] = (int)(tmp[i] + 0.5);

    /* Fall through.  */
  case 1:
    if (!mxIsDouble(prhs[0]))
      mexErrMsgTxt("1:st argument must be numerical.");
    n = mxGetN(prhs[0]);
    m = mxGetM(prhs[0]);
    if (n > 1 && m > 1)
      mexErrMsgTxt("1:st argument must be a vector.");

    xr = mxGetPr(prhs[0]);
    nr = n*m;
  }

  plhs[0] = mxCreateDoubleMatrix(1,nfun, mxREAL);
  y = mxGetPr(plhs[0]);
  elmer_param_vec (nfun, y, nr, xr, ni, xi, tag);
}
