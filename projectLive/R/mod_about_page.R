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
              shiny::selectizeInput(ns("funder"), 
                                    label = "", 
                                    choices = unique(projectLive::studies$fundingAgency),
                                    selected = "NTAP", 
                                    multiple = F),
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
 
