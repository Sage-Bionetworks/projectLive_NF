#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_summary_snapshot_server, "summary_snapshot_ui_1")
  callModule(mod_file_status_server, "file_status_ui_1")
  callModule(mod_study_lead_server, "study_lead_ui")
}
