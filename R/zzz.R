### This file loads synapseclient and makes it globally available to the app
### Filename is zzz.R as per convention

synapse <- NULL

.onLoad <- function(libname, pkgname) {
  if("use_conda_env.R" %in% list.files("R")) source("R/use_conda_env.R")
  .GlobalEnv$synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
}