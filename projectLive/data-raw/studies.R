## code to prepare `DATASET` dataset goes here

library(synapser)
library(purrr)
library(glue)
library(dplyr)

synapser::synLogin()

#select columns from a synTable that are not STRING_LISTs
synid <- "syn16787123"
columns <- as.list(synapser::synGetTableColumns(glue::glue("{synid}")))
select_cols <- columns %>% purrr::keep(function(x) !x$columnType == "STRING_LIST")
select_colnames <- base::unlist(base::lapply(select_cols, '[[', "name"))
studies <- synapser::synTableQuery(glue::glue("SELECT '{paste(select_colnames,collapse=\"','\")}' FROM {synid}"))$asDataFrame()


# studies <- synapser::synTableQuery("SELECT 'studyName','studyId','studyFileviewId','studyStatus',
#                                    'dataStatus', 'studyLeads', 'fundingAgency', 'summary', 'diseaseFocus', 
#                                    'manifestation', 'consortium','id', 'accessRequirements', 'acknowledgementStatements',
#                                    'relatedStudies', 'institutions' FROM syn16787123")$asDataFrame()

#load("data-raw/pubs.RData")
usethis::use_data(studies)