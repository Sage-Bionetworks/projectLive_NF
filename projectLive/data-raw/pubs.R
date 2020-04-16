## code to prepare `DATASET` dataset goes here
#
library(synapser)
library(purrr)
library(glue)
library(dplyr)

synapser::synLogin()

#select columns from a synTable that are not STRING_LISTs
synid <- "syn16857542"
columns <- as.list(synapser::synGetTableColumns(glue::glue("{synid}")))
select_cols <- columns %>% purrr::keep(function(x) !x$columnType == "STRING_LIST")
select_colnames <- base::unlist(base::lapply(select_cols, '[[', "name"))
pubs <- synapser::synTableQuery(glue::glue("SELECT '{paste(select_colnames,collapse=\"','\")}' FROM {synid}"))$asDataFrame()

# pubs <- synapser::synTableQuery("SELECT 'studyId', 'studyName', 'fundingAgency_depr', 'doi', 'diseaseFocus',
#                                 'featured', 'journal', 'title', 'author', 'year', 
#                                 'pmid', 'manifestation_depr' FROM syn16857542")$asDataFrame()

#get rid of "_depr" from colnames
names(pubs) <- gsub("_depr","",names(pubs),ignore.case=T)

#load("data-raw/pubs.RData")
usethis::use_data(pubs)
