# Module UI

#' @title   mod_file_status_ui and mod_file_status_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_file_status
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import plotly
mod_file_status_ui <- function(id){
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
          
          box(title = "Funding Partner",
              width = 12,
              solidHeader = T,
              status = "primary",
              shiny::selectizeInput(ns("funder"), 
                                    label = "", 
                                    choices = unique(projectLive::studies$fundingAgency),
                                    selected = "NTAP", 
                                    multiple = F),
              shiny::textOutput(ns('funding_agency')),
          ),
        
        
        box(title = "Publication Status", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('pub_status'))
        ),
        
        box(title = "Publication status by Disease Manifestation", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('pub_disease'))
        )
        
      )
  )))
}

# Module Server

#' @rdname mod_file_status
#' @export
#' @keywords internal

mod_file_status_server <- function(input, output, session){
  ns <- session$ns
  
  # filter the data
  plotdata <- reactive({
    print(input$funder)
    print(projectLive::pubs)
    projectLive::pubs %>% 
      dplyr::filter(fundingAgency == input$funder)
  })

  output$funding_agency <- shiny::renderText({
    
      print(glue::glue("You are now viewing studies funded by {input$funder}. 
                       Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))

    
  })
  
  output$pub_status <- plotly::renderPlotly({
    
    data <- plotdata() %>% 
      dplyr::select(year, studyName) %>% 
      tidyr::unnest(cols = studyName) %>% 
      print()
      #make plot
    ggplot(data, aes(x=year, fill=studyName, color= studyName)) + 
      geom_histogram( binwidth=0.5, alpha=0.8, position="stack") +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of publications") +
      ylim(0, 10) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            legend.position="none",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "grey95")) 

  })
  
  
  output$pub_disease <- plotly::renderPlotly({
    
    data <- plotdata() %>% 
      dplyr::select(year, manifestation) %>% 
      tidyr::unnest(cols = manifestation) %>% 
      print()
    #make plot
    ggplot(data, aes(x=year, fill=manifestation, color= manifestation)) + 
      geom_histogram( binwidth=0.5, alpha=0.8, position="stack") +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of publications") +
      ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            legend.position="bottom",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "grey95"))
    
  })
  
}

## To be copied in the UI
# mod_file_status_ui("summary_snapshot_ui")

## To be copied in the server
# callModule(mod_file_status_server, "file_status_ui")

