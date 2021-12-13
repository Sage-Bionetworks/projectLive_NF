require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
pubs <- get_synapse_tbl(syn, "syn16857542")
saveRDS(pubs, "pubs.RDS")

# live
store_file_in_synapse(
  "pubs.RDS",
  "syn22281727"
)

# dev
store_file_in_synapse(
  "pubs.RDS",
  "syn24474593"
)