<<<<<<< HEAD
require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(tools, "tools.RDS")
store_file_in_synapse(
  "tools.RDS",
  "syn22281727"
=======
## code to prepare `DATASET` dataset goes here

source("get_synapse_tbl.R")

library(magrittr)

# use your own condaenv here!!!!!
reticulate::use_condaenv(
  condaenv = "py37b",
  required = TRUE,
  conda = "/home/aelamb/anaconda3/condabin/conda"
>>>>>>> master
)

synapseclient <- reticulate::import("synapseclient")
syn <- synapseclient$Synapse()
syn$login()

tools <- get_synapse_tbl(syn, "syn16859448")

usethis::use_data(tools, overwrite = TRUE)
