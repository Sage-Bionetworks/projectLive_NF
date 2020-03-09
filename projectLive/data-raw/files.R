## code to prepare `DATASET` dataset goes here

library(synapser)
synapser::synLogin()
files <- synapser::synTableQuery("SELECT * FROM syn16858331")$asDataFrame()

#load("data-raw/pubs.RData")
usethis::use_data(files)