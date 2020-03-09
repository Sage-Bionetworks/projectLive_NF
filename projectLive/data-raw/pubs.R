## code to prepare `DATASET` dataset goes here
#
library(synapser)
synapser::synLogin()
pubs <- synapser::synTableQuery("SELECT * FROM syn16857542")$asDataFrame()

#load("data-raw/pubs.RData")
usethis::use_data(pubs)
