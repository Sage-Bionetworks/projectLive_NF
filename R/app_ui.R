library(shiny)
library(waiter)

app_ui <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    authorization_url = oauth2.0_authorize_url(api, app, scope = scope)
    return(tags$script(HTML(sprintf("location.replace(\"%s\");",
                                    authorization_url))))
  } else {
    ui_function()
  }
}

ui_function <- function(){
  tagList(
    golem_add_external_resources(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = span(
      style="color:white;",
      waiter::spin_pulsar(),
      h3("logging in...")
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
    includeScript(system.file("inst/app/www/readCookie.js", package = "projectLive")), 
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}