## code to prepare `DATASET` dataset goes here
#

library(magrittr)

# use your own condaenv here!!!!!
reticulate::use_condaenv(
  condaenv = "py37b",
  required = TRUE,
  conda = "/home/aelamb/anaconda3/condabin/conda"
)

synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
syn$login()

synid <- "syn16857542"

list_columns <- synid %>% 
  syn$getTableColumns() %>% 
  reticulate::iterate(.) %>% 
  purrr::keep(
    ., 
    stringr::str_detect(purrr::map_chr(., purrr::pluck, "columnType"), "_LIST")
  ) %>% 
  purrr::map_chr(purrr::pluck("name"))
  

pubs <- syn$tableQuery(glue::glue("SELECT * FROM {synid}")) %>% 
  purrr::pluck("filepath") %>% 
  readr::read_csv(.) %>% 
  dplyr::select(!dplyr::contains("depr")) %>% 
  dplyr::mutate_at(list_columns, ~stringr::str_remove_all(.x, '[\\"\\[\\]\\\\]')) %>% 
  dplyr::mutate_at(list_columns, ~stringr::str_split(.x, ", "))

usethis::use_data(pubs, overwrite = TRUE)
