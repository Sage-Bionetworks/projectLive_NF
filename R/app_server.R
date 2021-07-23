
library(shiny)
library(reticulate)
library(waiter)

# Don't necessarily have to set `RETICULATE_PYTHON` env variable
# reticulate::use_condaenv("synapse")
synapseclient <- import('synapseclient')

app_server <- shinyServer(function(input, output, session) {
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  redirect_url <- paste0(api$access, '?', 'redirect_uri=',
                         APP_URL, '&grant_type=',
                         'authorization_code' ,'&code=', params$code)
  # get the access_token and userinfo token
  req <- POST(redirect_url,
              encode = "form",
              body = '',
              authenticate(app$key, app$secret, type = "basic"),
              config = list())
  # Stop the code if anything other than 2XX status code is returned
  stop_for_status(req, task = "get an access token")
  token_response <- content(req, type = NULL)
  access_token <- token_response$access_token
  # Create Synapse connection
  syn <- synapseclient$Synapse()
  syn$login(authToken=access_token)
  
  output$title <- shiny::renderUI({
    shiny::titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
  })
  
  require(magrittr)
  require(rlang)
  
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
