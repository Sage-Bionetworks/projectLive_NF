## code to prepare `DATASET` dataset goes here


source("get_synapse_tbl.R")

library(magrittr)

# use your own condaenv here!!!!!
reticulate::use_condaenv(
  condaenv = "py37b",
  required = TRUE,
  conda = "/home/aelamb/anaconda3/condabin/conda"
)

synapseclient <- reticulate::import("synapseclient")
syn <- synapseclient$Synapse()
syn$login()

files <- get_synapse_tbl(syn, "syn16858331")

usethis::use_data(files, overwrite = TRUE)



