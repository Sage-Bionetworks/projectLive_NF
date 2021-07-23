app_ui <- function(req) {
  oauth_ui(req, ui_function)
}

oauth_ui <- function(req, ui_function) {
  if (!has_auth_code(shiny::parseQueryString(req$QUERY_STRING))) {
    
    script_html <- create_oauth_url_script_html(
      endpoint = OAUTH_LIST$endpoint, 
      app      = OAUTH_LIST$app, 
      scope    = OAUTH_LIST$scope
    )
    
    return(script_html)
    
  } else {
    ui_function()
  }
}

ui_function <- function(){
  shiny::tagList(
    golem_add_external_resources(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = span(
      style="color:white;",
      waiter::spin_pulsar(),
      shiny::h3("logging in...")
    )),
    shiny::navbarPage(   
      title = shiny::strong("projectLive"), selected = "About",	
      shiny::tabPanel(
        "About",
        mod_about_page_ui("about_page_ui_1"),
        icon = shiny::icon("info-circle")
      ),
      shiny::tabPanel(
        "Snapshot",
        projectlive.modules::summary_snapshot_module_ui("summary_snapshot_ui_1"),
        icon = shiny::icon("chart-area")
      ),
      shiny::tabPanel(
        "Publications",
        projectlive.modules::publication_status_module_ui("file_status_ui_1"),
        icon = shiny::icon("book-reader")
      ),
      shiny::tabPanel(
        "Participating Studies",
        projectlive.modules::study_summary_module_ui("study_summary_ui_1"),
        icon = shiny::icon("bar-chart-o")
      ),
      shiny::tabPanel(
        "New Submissions",
        mod_new_submissions_ui("new_submissions_ui_1"),
        icon = shiny::icon("bar-chart-o")
      ),
      collapsible = TRUE,	inverse = TRUE,
      windowTitle = "projectLive"
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'projectLive')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # add the next line to enable collection of synapse session token from browser cookie
    shiny::includeScript(system.file("inst/app/www/readCookie.js", package = "projectLive")), 
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}