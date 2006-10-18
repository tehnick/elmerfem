.First.lib <- function(lib, pkg)
{
  library.dynam("elmerparam", pkg, lib)
}

elmer_param <- function(xr = NULL, xi = NULL, tag = "")
{
  .Call("elmerparam_rwrapper", xr, xi, tag)
}

