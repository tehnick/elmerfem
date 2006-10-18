.First.lib <- function(lib, pkg)
{
  library.dynam("elmerparam", pkg, lib)
}

elmer_param <- function(xr = NULL, xi = NULL, tag = "", nfun = 1)
{
  .Call("elmerparam_rwrapper", c(nfun), xr, xi, tag)
}

