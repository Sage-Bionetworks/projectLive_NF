### This file loads synapseclient and makes it globally available to the app
### Filename is zzz.R as per convention

synapse <- NULL

.onLoad <- function(libname, pkgname) {
  .GlobalEnv$synapseclient <- reticulate::import("synapseclient", delay_load = TRUE)
}