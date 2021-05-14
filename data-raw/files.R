require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
<<<<<<< HEAD
studies <- get_synapse_tbl(syn, "syn16787123") 

# live ----

files <- get_synapse_tbl(syn, "syn16858331") 

files <- files %>% 
=======
files <- get_synapse_tbl(syn, "syn16858331")
studies <- get_synapse_tbl(syn, "syn16787123")
files <- files %>%
  dplyr::rename("consortium" = "initiative") %>%
>>>>>>> master
  dplyr::filter(.data$type == "file") %>% 
  dplyr::mutate(fundingAgency = studies$fundingAgency[match(projectId, studies$studyId)]) 

saveRDS(files, "files.RDS")
store_file_in_synapse(
  "files.RDS",
  "syn22281727"
)

# develop ----
dev_files <- 
  projectlive.modules::get_synapse_tbl(
    syn, 
    "syn16858331",
    columns = c(
      "id",
      "individualID",
      "specimenID",
      "assay",
      "initiative",
      "dataType",
      "fileFormat",
      "resourceType",
      "studyName",
      "accessType",
      "tumorType",
      "species",
      "projectId",
      "benefactorId",
      "name",
      "parentId",
      "createdOn",
      "type"
    )
  ) %>% 
  dplyr::filter(.data$type == "file") %>% 
  dplyr::mutate(
    fundingAgency = studies$fundingAgency[match(projectId, studies$studyId)]
  ) %>% 
  format_date_columns() %>% 
  dplyr::select(-c("createdOn", "ROW_ID", "ROW_VERSION", "ROW_ETAG", "type")) %>%
  dplyr::left_join(
    dplyr::select(
      studies,
      "studyName",
      "studyLeads"
    ),
    by = "studyName"
  )
  

saveRDS(dev_files, "files.RDS")
store_file_in_synapse(
  "files.RDS",
  "syn24474593"
)

