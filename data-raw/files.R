require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
files <- get_synapse_tbl(syn, "syn16858331") %>% 
  dplyr::rename("initiative" = "consortium")
studies <- get_synapse_tbl(syn, "syn16787123") 
files <- files %>% 
  dplyr::filter(.data$type == "file") %>% 
  dplyr::mutate(fundingAgency = studies$fundingAgency[match(projectId, studies$studyId)]) 

saveRDS(files, "files.RDS")
store_file_in_synapse(
  "files.RDS",
  "syn22281727"
)

