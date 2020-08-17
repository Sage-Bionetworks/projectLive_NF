# Module UI
  
#' @title   mod_about_page_ui and mod_about_page_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_about_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = T),
      dashboardSidebar(disable = T),
      
      dashboardBody(
        fluidPage(
          
          shinydashboard::infoBoxOutput(ns('about'), width = 12),
          
          box(title = "Funding Partner",
              width = 12,
              solidHeader = T,
              status = "primary",
              shiny::textOutput(ns('welcome')),
              shiny::uiOutput(ns("agency_selection_ui")),
              shiny::textOutput(ns('funding_agency'))
          )
  
))))
}
    
# Module Server
    
#' @rdname mod_about_page
#' @export
#' @keywords internal


mod_about_page_server <- function(input, output, session, syn){
  ns <- session$ns
  
  output$about <- shinydashboard::renderInfoBox({
    
    shinydashboard::infoBox(
      " ",
      print("projectLive: Track the progress and impact of our funding partners in real time"),
      icon = icon("university", "fa-1x"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
      )
  })
   
  output$welcome <- shiny::renderText({
    print(glue::glue("Welcome {syn$getUserProfile()$displayName} !"))
  })
  
  
  current_user_synapse_id <- shiny::reactive({
    # code to get the synapse id of the current user here
    user <- syn$getUserProfile()[['ownerId']]
    return(user)
  })
  
  agencies_allowed <- shiny::reactive({    
    # The following code chunk ensures that only members of specific teams can access the files below. 
    # Individual users will not be able to access the tables/files through this code even if they have access to the entity on synapse
    entity <- "syn22281727"
    
    # the teams that user belongs to
    user_teams <- syn$restGET(
      glue::glue("/user/{syn$getUserProfile()[['ownerId']]}/team?limit=10000"))$results 
    all_teams <- purrr::map_chr(user_teams, function(x) x$id)
    all_team_names <- purrr::map_chr(user_teams, function(x) x$name)
    
    # the teams allowed to view the dashboard
    dashboard_teams <- syn$restGET(glue::glue("/entity/{entity}/acl"))
    allowed_teams <- purrr::map_chr(dashboard_teams$resourceAccess, function(x) x$principalId)
                                    
    #dashboard_team_names <- syn$tableQuery("SELECT * FROM syn22279138")$asDataFrame()
    
    #final allowed agencies:
     all_team_names[all_teams %in% allowed_teams] 

  })
  
  output$agency_selection_ui <- shiny::renderUI({
    shiny::selectizeInput(
      ns("funder"), 
      label = "", 
      choices = agencies_allowed(),
      multiple = F
    )
  })
  
  output$funding_agency <- shiny::renderText({
    #print(sprintf("Welcome, %s", syn$getUserProfile()$userName))
    print(glue::glue("You are now viewing studies moderated by the {input$funder}. 
                     Navigate to the tabs at the top of the page to get more information about the participating investigators and the various resources that they have generated."))
  })
  
  
  funding_partner <- reactive({ input$funder })
  return(funding_partner)
 
}


    
## To be copied in the UI
# mod_about_page_ui("about_page_ui_1")
    
## To be copied in the server
# callModule(mod_about_page_server, "about_page_ui_1")
 
