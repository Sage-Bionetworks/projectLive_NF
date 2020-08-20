<<<<<<< HEAD
require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
studies <- get_synapse_tbl(syn, "syn16787123")
saveRDS(studies, "studies.RDS")
store_file_in_synapse(
  "studies.RDS",
  "syn22281727"
=======
# ## code to prepare `DATASET` dataset goes here

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

studies <- get_synapse_tbl(syn, "syn16787123")

usethis::use_data(studies, overwrite = TRUE)
