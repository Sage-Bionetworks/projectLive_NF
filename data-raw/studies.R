require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
studies <- get_synapse_tbl(syn, "syn16787123") %>%
   dplyr::rename("initiative" = "consortium")

saveRDS(studies, "studies.RDS")
store_file_in_synapse(
  "studies.RDS",
  "syn22281727"
)

