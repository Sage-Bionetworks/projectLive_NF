require(magrittr)
require(rlang)

app_server <- shinyServer(function(input, output, session) {
  
  access_token = projectlive.modules::get_oauth_access_token(
    oauth_list = OAUTH_LIST, session = session
  )
  
  syn <- synapseclient$Synapse()
  syn$login(authToken = access_token)

  output$title <- shiny::renderUI({
    shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
  })
  
  data <- projectlive.modules::synapse_module_server2(
    id = "synapse_module",
    syn = syn,
    config = shiny::reactive(
      jsonlite::read_json("inst/synapse_module.json")
    )
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
  
  projectlive.modules::new_submissions_module_server(
    id = "new_submissions_module",
    data = data,
    config = shiny::reactive(
      jsonlite::read_json("inst/new_submissions_module.json")
    )
  )
  
})
