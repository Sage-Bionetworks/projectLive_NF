require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
studies <- get_synapse_tbl(syn, "syn16787123") 

saveRDS(studies, "studies.RDS")

# dev ----
store_file_in_synapse(
  "studies.RDS",
  "syn24474593"
)

# live ----
store_file_in_synapse(
  "studies.RDS",
  "syn22281727"
)