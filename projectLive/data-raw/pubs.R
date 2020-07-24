## code to prepare `DATASET` dataset goes here
#
library(reticulate)
library(purrr)
library(glue)
library(dplyr)

# use your own condaenv here!!!!!
# reticulate::use_condaenv(
#   condaenv = "py37b",
#   required = TRUE,
#   conda = "/home/aelamb/anaconda3/condabin/conda"
# )

synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
syn$login()

synid <- "syn16857542"
pubs <- syn$tableQuery(glue::glue("SELECT * FROM {synid}")) %>% 
  purrr::pluck("filepath") %>% 
  readr::read_csv(.) %>% 
  dplyr::select(!dplyr::contains("depr"))

usethis::use_data(pubs, overwrite = TRUE)
