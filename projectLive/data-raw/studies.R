## code to prepare `DATASET` dataset goes here

library(synapser)
synapser::synLogin()
studies <- synapser::synTableQuery("SELECT * FROM syn16787123")$asDataFrame()

#load("data-raw/pubs.RData")
usethis::use_data(studies)