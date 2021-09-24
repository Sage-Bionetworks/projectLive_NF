require(magrittr)

live_folder <- "syn22281727"
dev_folder  <- "syn24474593"

source("https://raw.githubusercontent.com/Sage-Bionetworks/projectlive.modules/231f14aa9a4c35a4cad46c851ff05eb07dff3f19/R/synapse_functions.R")
source("https://raw.githubusercontent.com/Sage-Bionetworks/projectlive.modules/231f14aa9a4c35a4cad46c851ff05eb07dff3f19/R/data_manipulation_functions.R")

reticulate::use_condaenv("sage-bionetworks", required = T)
synapseclient <- reticulate::import("synapseclient")
syn <- synapseclient$Synapse()
syn$login()

store_file_in_synapse <- function(syn, file, parent_id){
  file <- reticulate::import("synapseclient")$File(file, parent_id)
  syn$store(file)
}



# studies ----
studies <- get_synapse_tbl(syn, "syn16787123")
saveRDS(studies, "studies.RDS")

store_file_in_synapse(
  syn,
  "studies.RDS",
  dev_folder
)

store_file_in_synapse(
  syn,
  "studies.RDS",
  live_folder
)


# files ----
files <-
  get_synapse_tbl(
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
      "tumorType",
      "species",
      "projectId",
      "benefactorId",
      "consortium",
      "progressReportNumber",
      "createdOn",
      "type"
    ),
    col_types = readr::cols(
      "consortium" = readr::col_character(),
      "progressReportNumber" = readr::col_integer()
    )
  ) %>%
  dplyr::filter(type == "file") %>%
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
  ) %>% 
  dplyr::mutate("reportMilestone" = .data$progressReportNumber)

saveRDS(files, "files.RDS")
store_file_in_synapse(syn, "files.RDS", dev_folder)
store_file_in_synapse(syn, "files.RDS", live_folder)

# incoming data ----

incoming_data <-
  get_synapse_tbl(
    syn,
    "syn23364404",
    columns = c(
      "fileFormat",
      "date_uploadestimate",
      "progressReportNumber",
      "estimatedMinNumSamples",
      "fundingAgency",
      "projectSynID",
      "dataType"
    ),
    col_types = readr::cols(
      "estimatedMinNumSamples" = readr::col_integer(),
      "progressReportNumber" = readr::col_integer()
    )
  ) %>%
  dplyr::left_join(
    dplyr::select(studies, "studyName", "studyId"),
    by = c("projectSynID" = "studyId")
  ) %>%
  dplyr::mutate(
    "date_uploadestimate" = lubridate::mdy(date_uploadestimate),
  ) %>%
  dplyr::select(-"projectSynID") %>%
  dplyr::filter(
    !is.na(.data$date_uploadestimate) | !is.na(.data$progressReportNumber)
  ) %>%
  tidyr::unnest("fileFormat") %>%
  dplyr::group_by(
    .data$fileFormat,
    .data$date_uploadestimate,
    .data$progressReportNumber,
    .data$fundingAgency,
    .data$studyName,
    .data$dataType
  ) %>%
  dplyr::summarise("estimatedMinNumSamples" = sum(.data$estimatedMinNumSamples)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    "estimatedMinNumSamples" = dplyr::if_else(
      is.na(.data$estimatedMinNumSamples),
      0L,
      .data$estimatedMinNumSamples
    ),
    "reportMilestone" = .data$progressReportNumber
  )

saveRDS(incoming_data, "incoming_data.RDS")
store_file_in_synapse(syn, "incoming_data.RDS", live_folder)
store_file_in_synapse(syn, "incoming_data.RDS", dev_folder)

# publications ----
pubs <- get_synapse_tbl(syn, "syn16857542")
saveRDS(pubs, "pubs.RDS")
store_file_in_synapse(syn, "pubs.RDS", live_folder)
store_file_in_synapse(syn, "pubs.RDS", dev_folder)

# tools ----
tools <- get_synapse_tbl(syn, "syn16859448")
saveRDS(tools, "tools.RDS")
store_file_in_synapse(syn, "tools.RDS", live_folder)
store_file_in_synapse(syn, "tools.RDS", dev_folder)