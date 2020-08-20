require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
pubs <- get_synapse_tbl(syn, "syn16857542")
saveRDS(pubs, "pubs.RDS")
store_file_in_synapse(
  "pubs.RDS",
  "syn22281727"
)
