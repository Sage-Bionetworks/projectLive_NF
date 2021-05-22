require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
studies <- get_synapse_tbl(syn, "syn16787123") 

# live ----

files <- get_synapse_tbl(syn, "syn16858331") 

files <- files %>% 
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
      "name",
      "individualID",
      "parentId",
      "specimenID",
      "assay",
      "initiative",
      "dataType",
      "fileFormat",
      "resourceType",
      "accessType",
      "initiative",
      "tumorType",
      "species",
      "projectId",
      "benefactorId",
      "reportMilestone",
      "createdOn"
    ),
    col_types = readr::cols(
      "consortium" = readr::col_character(),
      "reportMilestone" = readr::col_integer()
    )
  ) %>%
  format_date_columns() %>%
  dplyr::select(-c("createdOn")) %>%
  dplyr::inner_join(
    dplyr::select(
      studies,
      "studyName",
      "studyLeads",
      "fundingAgency",
      "studyId"
    ),
    by = c("projectId" = "studyId")
  )
  

saveRDS(dev_files, "files.RDS")
store_file_in_synapse(
  "files.RDS",
  "syn24474593"
)

