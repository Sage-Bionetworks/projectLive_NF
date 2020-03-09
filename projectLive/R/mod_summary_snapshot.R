# Module UI

#' @title   mod_summary_snapshot_ui and mod_summary_snapshot_server
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
mod_summary_snapshot_ui <- function(id){
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
      
      
      box(title = "Consortium Activity", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('study_per_consortium'))
      ),
      
      box(title = "Study Activity", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          #height = 6,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('files_per_study'))
      )
      
    )
  )))
}

# Module Server

#' @rdname mod_summary_snapshot
#' @export
#' @keywords internal

mod_summary_snapshot_server <- function(input, output, session){
  ns <- session$ns

  # filter the data
  plotdata <- reactive({
    projectLive::files %>% 
      dplyr::filter(fundingAgency == input$funder) 
  })
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {input$funder}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$study_per_consortium <- plotly::renderPlotly({
    
    data <- as.data.frame(plotdata()) 
    data <- data %>% 
      mutate(year= lubridate::year(data$createdOn)) 
    
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"
    data$accessType[is.na(data$accessType) == TRUE] <- "Not Annotated"
    
    #make plot
    ggplot(data, aes(x=consortium, y= studyName, fill=accessType, color= accessType)) +
      geom_bar(stat="identity", position= "stack", alpha=0.8, na.rm=TRUE) +
      coord_flip() +
      scale_fill_manual(values=color_list[[4]])+
      scale_color_manual(values=color_list[[4]])+
      labs(title="", y = "Number of studies per Consortium") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_text(size=8),
            axis.text.x  = element_blank(), #element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="none") +
    facet_grid(.~ year)
  })
  
  
  output$files_per_study <- plotly::renderPlotly({
    
    data <- as.data.frame(plotdata())
    data <- data %>% 
      mutate(year= lubridate::year(data$createdOn)) 
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"
    
    #make plot
    ggplot(data, aes(x=dataType, fill=studyName, color= studyName)) + 
      geom_bar(stat="count", position= "stack", alpha=0.8, na.rm=TRUE) +
      coord_flip() +
      scale_fill_manual(values=color_list[[4]])+
      scale_color_manual(values=color_list[[4]])+
      labs(title="", y = "Number of files per study") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="none") +
      facet_grid(. ~ year, scales="free")
    
  })
  
}

## To be copied in the UI
# mod_file_status_ui("file_status_ui")

## To be copied in the server
# callModule(mod_file_status_server, "file_status_ui")

