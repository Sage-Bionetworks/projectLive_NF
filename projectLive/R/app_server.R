#' @import shiny
app_server <- function(input, output,session) {
    session$sendCustomMessage(type="readCookie", message=list())

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
      )
    )
  })

  syn <- synapseclient$Synapse()

  foo <- observeEvent(input$cookie, { 
    syn$login(sessionToken=input$cookie)
    
    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
    })

  # List the first level callModules here
  funding_partner <- callModule(mod_about_page_server, "about_page_ui_1", syn)
  callModule(mod_summary_snapshot_server, "summary_snapshot_ui_1", funding_partner)
  callModule(mod_file_status_server, "file_status_ui_1", funding_partner)
  callModule(mod_study_lead_server, "study_lead_ui_1", funding_partner)
  callModule(mod_study_summary_server, "study_summary_ui_1", funding_partner)
  })
}
