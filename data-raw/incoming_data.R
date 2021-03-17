require(magrittr)
devtools::load_all()
syn <- create_synapse_login()
studies <- get_synapse_tbl(syn, "syn16787123") 

# live ----

# develop ----

dev_incoming_data <-
  get_synapse_tbl(
    syn,
    "syn23364404",
    columns = c(
      "fileFormat",
      "date_uploadestimate",
      "reportMilestone",
      "estimatedMinNumSamples",
      "fundingAgency",
      "projectSynID"
    ),
    col_types = readr::cols(
      "estimatedMinNumSamples" = readr::col_integer(),
      "reportMilestone" = readr::col_integer()
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
    !is.na(.data$date_uploadestimate) | !is.na(.data$reportMilestone)
  ) %>%
  tidyr::unnest("fileFormat") %>%
  dplyr::group_by(
    .data$fileFormat,
    .data$date_uploadestimate,
    .data$reportMilestone,
    .data$fundingAgency,
    .data$studyName
  ) %>%
  dplyr::summarise("estimatedMinNumSamples" = sum(.data$estimatedMinNumSamples)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    "estimatedMinNumSamples" = dplyr::if_else(
      is.na(.data$estimatedMinNumSamples),
      0L,
      .data$estimatedMinNumSamples
    )
  )


saveRDS(dev_incoming_data, "incoming_data.RDS")
store_file_in_synapse(
  "incoming_data.RDS",
  "syn24474593"
)