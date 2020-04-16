## code to prepare `DATASET` dataset goes here

library(synapser)
library(purrr)
library(glue)
library(dplyr)

synapser::synLogin()

synid <- "syn16859448"
columns <- as.list(synapser::synGetTableColumns(glue::glue("{synid}")))
select_cols <- columns %>% purrr::keep(function(x) !x$columnType == "STRING_LIST")
select_colnames <- base::unlist(base::lapply(select_cols, '[[', "name"))
tools <- synapser::synTableQuery(glue::glue("SELECT '{paste(select_colnames,collapse=\"','\")}' FROM {synid}"))$asDataFrame()

# tools <- synapser::synTableQuery("SELECT 'softwareName', 'summary', 'softwareLink', 'featured', 'link', 
#                                  'studyId','studyName', 'fundingAgency', 'contact', 'type', 'subtype', 
#                                  'diseaseFocus', 'manifestation','name' FROM syn16859448")$asDataFrame()

usethis::use_data(tools)