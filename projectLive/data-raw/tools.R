require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(tools, "tools.RDS")
store_file_in_synapse(
  "tools.RDS",
  #"syn22281727",
  "syn22282669"
)

