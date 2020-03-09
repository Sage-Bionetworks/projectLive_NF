## code to prepare `DATASET` dataset goes here

library(synapser)
synapser::synLogin()
tools <- synapser::synTableQuery("SELECT * FROM syn16859448")$asDataFrame()

usethis::use_data(tools)