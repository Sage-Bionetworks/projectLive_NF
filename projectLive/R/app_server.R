#' @import shiny
app_server <- function(input, output,session) {
  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()
  data_config <- jsonlite::read_json("inst/data_config.json")
  
  # List the first level callModules here
  group_object <- shiny::callModule(mod_about_page_server, "about_page_ui_1", syn, data_config)
  shiny::callModule(mod_summary_snapshot_server, "summary_snapshot_ui_1", group_object, data_config)
  shiny::callModule(mod_file_status_server, "file_status_ui_1", group_object, data_config)
  shiny::callModule(mod_study_lead_server, "study_lead_ui_1", group_object, data_config)
  shiny::callModule(mod_study_summary_server, "study_summary_ui_1", group_object, data_config)
}
