<<<<<<< HEAD
require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
files <- get_synapse_tbl(syn, "syn16858331")
saveRDS(files, "files.RDS")
store_file_in_synapse(
  "files.RDS",
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

files <- get_synapse_tbl(syn, "syn16858331")

usethis::use_data(files, overwrite = TRUE)



