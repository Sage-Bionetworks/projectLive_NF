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
    
    syn$login(authToken = input$cookie)
    #locally
    # syn$login(sessionToken = input$cookie)
    
    output$title <- shiny::renderUI({
      shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
    })
    
    require(magrittr)
    require(rlang)
    
    app_config <- jsonlite::read_json("inst/app_config.json")
    ## live
    data_config <- jsonlite::read_json("inst/data_config.json")
    ## dev
    #data_config <- jsonlite::read_json("inst/dev_data_config.json")
    
    data <- shiny::callModule(
      mod_about_page_server, 
      "about_page_ui_1", 
      syn, 
      data_config
    )
    
    projectlive.modules::summary_snapshot_module_server(
      id = "summary_snapshot_ui_1",
      data = data,
      config = shiny::reactive(
        jsonlite::read_json("inst/summary_snapshot_module.json")
      )
    )
    
    projectlive.modules::publication_status_module_server(
      id = "file_status_ui_1",
      data = data,
      config = shiny::reactive(
        jsonlite::read_json("inst/publication_status_module.json")
      )
    )
    
    projectlive.modules::study_summary_module_server(
      id = "study_summary_ui_1",
      data = data,
      config = shiny::reactive(
        jsonlite::read_json("inst/study_summary_module.json")
      )
    )
    
    purrr::walk2(
      list(
        mod_new_submissions_server
      ),
      list(
        "new_submissions_ui_1"
      ),
      shiny::callModule,
      data,
      app_config
    ) 
  })
}
