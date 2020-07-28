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
          #add analytics
          #         tags$head(includeScript("<!-- Global site tag (gtag.js) - Google Analytics -->
          # <script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-160814003-1\"></script>
          # <script>
          #   window.dataLayer = window.dataLayer || [];
          #   function gtag(){dataLayer.push(arguments);}
          #   gtag('js', new Date());
          # 
          #   gtag('config', 'UA-160814003-1');
          # </script>
          # "),
          #includeScript("www/google_analytics.js")),
          
          # box(title = "About",
          #     width = 12,
          #     solidHeader = T,
          #     status = "primary",
          #     shiny::htmlOutput(ns('about'))
          #     #DT::dataTableOutput(ns('study_table'))
          # ),
          
          # box(title = "About",
          #     status = "primary",
          #     solidHeader = F,
          #     width = 12,
          #     collapsible = FALSE,
              shinydashboard::infoBoxOutput(ns('about'), width = 12),
          #),
          
          box(title = "Funding Partner",
              width = 12,
              solidHeader = T,
              status = "primary",
              shiny::uiOutput(ns("agency_selection_ui")),
              shiny::textOutput(ns('funding_agency'))
              #DT::dataTableOutput(ns('study_table'))
          )
  
))))
}
    
# Module Server
    
#' @rdname mod_about_page
#' @export
#' @keywords internal
    
mod_about_page_server <- function(input, output, session){
  ns <- session$ns
  
  # Adding in-app authentication
  session$sendCustomMessage(type = "readCookie", message = list())
  
  observeEvent(input$cookie, {
    
    syn_login(sessionToken=input$cookie, rememberMe = TRUE)
    
    ## Show message if user is not logged in to synapse
    unauthorized <- observeEvent(input$authorized, {
      showModal(
        modalDialog(
          title = "Not logged in",
          HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
        )
      )
    })
    
    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", syn_getUserProfile()$userName))
    })
  
  current_user_synapse_id <- shiny::reactive({
    # code to get the synapse id of the current user here
    user <- synGetUserProfile()[['ownerId']]
    # This user has permisions to CTF and NTAP
    #return(273966)
    return(user)
  })
  
  agencies_allowed <- shiny::reactive({
    synapser::synLogin()
    
    team_id_list <- c(
      "NF-OSI" = 3378999L,
      "CTF" = 3359657L,
      "GFF" = 3406072L,
      "NTAP" = 3331266L,
      "test_team" = 3413244L
    )
    
    team_permission_list <- list(
      "NF-OSI" = c("CTF", "GFF", "NTAP"),
      "CTF" = "CTF",
      "GFF" = "GFF",
      "NTAP" = "NTAP",
      "test_team" = "NTAP"
    )
    
    get_team_members <- function(team_id){
      team_id %>% 
        synapser::synGetTeamMembers() %>%
        synapser::as.list() %>% 
        purrr::map(., purrr::pluck("member")) %>% 
        purrr::map_chr(., purrr::pluck("ownerId")) %>% 
        as.integer()
    }
  
    team_member_list <- purrr::map(
      team_id_list,
      get_team_members
    )
    
    teams_user_is_in <-
      purrr::map_lgl(team_member_list, ~ (current_user_synapse_id() %in% .x)) %>% 
      purrr::keep(., .) %>% 
      names()
    
    if(length(teams_user_is_in) == 0) return(NULL)
    
    allowed_agencies <- team_permission_list %>% 
      purrr::keep(., . %in% teams_user_is_in) %>% 
      unlist() %>% 
      unname()
  })
  
  output$agency_selection_ui <- shiny::renderUI({
    shiny::selectizeInput(ns("funder"), 
                          label = "", 
                          choices = agencies_allowed(),
                          selected = "NTAP", 
                          multiple = F)
  })
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {input$funder}. 
                     Navigate to the tabs at the top of the page to get more information about the funded investigators and the various resources that they have generated."))
  })
  
 
  output$about <- shinydashboard::renderInfoBox({

    shinydashboard::infoBox(
      " ",
      print("projectLive: Track the progress and impact of our funding partners in real time"),
      icon = icon("university", "fa-1x"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
    )
  })
  
  
  funding_partner <- reactive({ input$funder })
  return(funding_partner)
}
    
## To be copied in the UI
# mod_about_page_ui("about_page_ui_1")
    
## To be copied in the server
# callModule(mod_about_page_server, "about_page_ui_1")
 
