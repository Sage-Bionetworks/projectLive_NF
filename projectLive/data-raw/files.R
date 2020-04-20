## code to prepare `DATASET` dataset goes here

library(synapser)
library(purrr)
library(glue)
library(dplyr)

synapser::synLogin()

#select columns from a synTable that are not STRING_LISTs
synid <- "syn16858331"
columns <- as.list(synapser::synGetTableColumns(glue::glue("{synid}")))
select_cols <- columns %>% purrr::keep(function(x) !x$columnType == "STRING_LIST")
select_colnames <- base::unlist(base::lapply(select_cols, '[[', "name"))
files <- synapser::synTableQuery(glue::glue("SELECT '{paste(select_colnames,collapse=\"','\")}' FROM {synid}"))$asDataFrame()

#files <- synapser::synTableQuery("SELECT * FROM syn16858331")$asDataFrame()

#load("data-raw/pubs.RData")
usethis::use_data(files, overwrite = TRUE)

