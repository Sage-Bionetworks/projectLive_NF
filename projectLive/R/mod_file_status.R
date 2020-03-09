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
      dashboardSidebar(
        h3("Funding Partner"),
        shiny::selectizeInput(ns("funder"), 
                              label = "Select a funding partner", 
                              choices = unique(projectLive::studies$fundingAgency),
                              selected = "NTAP", 
                              multiple = F)),
      dashboardBody(
        fluidPage(
        box(title = "Funding Partner", 
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('funding_agency'))),
        
        
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
            #height = 6,
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
  
  #funder <- funder
  # filter the data
  plotdata <- reactive({
    projectLive::pubs %>% 
      dplyr::filter(fundingAgency == input$funder)
  })

  output$funding_agency <- shiny::renderText({
    
    #funder <- reactive({funder})
    
      print(glue::glue("You are now viewing studies funded by {input$funder}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))

    
  })
  
  output$pub_status <- plotly::renderPlotly({
      
    data <- as.data.frame(plotdata())
      #make plot
      ggplot(data, aes(x=year, fill=studyName, color= studyName)) + 
        geom_histogram( binwidth=0.5, alpha=0.8, position="dodge") +
        theme(legend.position="right") +
        scale_fill_manual(values=color_list[[4]])+
        scale_color_manual(values=color_list[[4]])+
        labs(title="", y = "Number of publications") +
        ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8), 
              axis.text.x  = element_text(size=10),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              legend.position="none") 

  })
  
  
  output$pub_disease <- plotly::renderPlotly({
    
    data <- as.data.frame(plotdata())
    #make plot
    ggplot(data, aes(x=year, fill=manifestation, color= manifestation)) + 
      geom_histogram( binwidth=0.5, alpha=0.8, position="dodge") +
      theme(legend.position="right") +
      scale_fill_manual(values=color_list[[4]])+
      scale_color_manual(values=color_list[[4]])+
      labs(title="", y = "Number of publications") +
      ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            legend.position="none")
    
  })
  
}

## To be copied in the UI
# mod_file_status_ui("summary_snapshot_ui")

## To be copied in the server
# callModule(mod_file_status_server, "file_status_ui")

