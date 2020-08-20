<<<<<<< HEAD
require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
pubs <- get_synapse_tbl(syn, "syn16857542")
saveRDS(pubs, "pubs.RDS")
store_file_in_synapse(
  "pubs.RDS",
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

pubs <- get_synapse_tbl(syn, "syn16857542")

usethis::use_data(pubs, overwrite = TRUE)
