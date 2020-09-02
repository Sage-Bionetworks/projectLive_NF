#' @import shiny
app_server <- function(input, output,session) {
  
  #Add synapse login
  session$sendCustomMessage(type = "readCookie", message = list())
  
  ## Show message if user is not logged in to synapse
  shiny::observeEvent(input$authorized, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
      )
    )
  })
  
  syn <- .GlobalEnv$synapseclient$Synapse()
  
  shiny::observeEvent(input$cookie, { 
    syn$login(sessionToken = input$cookie)
    
    output$title <- shiny::renderUI({
      shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
    })
    
    require(magrittr)
    require(rlang)
    data_config <- jsonlite::read_json("inst/data_config.json")
    
    group_object <- shiny::callModule(
      mod_about_page_server, 
      "about_page_ui_1", 
      syn, 
      data_config
    )
    
    purrr::walk2(
      list(
        mod_summary_snapshot_server,
        mod_file_status_server,
        mod_study_summary_server,
        mod_new_submissions_server
      ),
      list(
        "summary_snapshot_ui_1",
        "file_status_ui_1",
        "study_summary_ui_1",
        "new_submissions_ui_1"
      ),
      shiny::callModule,
      group_object,
      data_config
    )
  })
}
