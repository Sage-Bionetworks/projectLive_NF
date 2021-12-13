require(magrittr)
require(rlang)

app_server <- shinyServer(function(input, output, session) {
  
  access_token = get_oauth_access_token(
    oauth_list = OAUTH_LIST, session = session
  )
  
  syn <- synapseclient$Synapse()
  syn$login(authToken = access_token)
  
  output$title <- shiny::renderUI({
    shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
  })

  
  app_config <- jsonlite::read_json("inst/app_config.json")
  #data_config <- jsonlite::read_json("inst/data_config.json")
  data_config <- jsonlite::read_json("inst/dev_data_config.json")
  
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
