#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  funding_partner <- callModule(mod_about_page_server, "about_page_ui_1")
  callModule(mod_summary_snapshot_server, "summary_snapshot_ui_1", funding_partner)
  callModule(mod_file_status_server, "file_status_ui_1", funding_partner)
  callModule(mod_study_lead_server, "study_lead_ui_1", funding_partner)
  callModule(mod_study_summary_server, "study_summary_ui_1", funding_partner)
}
