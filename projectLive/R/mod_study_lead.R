# Module UI

#' @title   mod_study_lead_ui and mod_study_lead_server
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
mod_study_lead_ui <- function(id){
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
      
      
      box(title = "Yearly Upload Status", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('upload_status'))
      ),
      
      box(title = "Yearly Annotation Status", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          # height = 12,
          collapsible = FALSE,
          shiny::selectizeInput(ns("studylead"), 
                                label = "Select a principal investigator", 
                                choices = sort(unique(projectLive::studies$studyLeads)),
                                selected = "Select an investigator", 
                                multiple = F),
          plotly::plotlyOutput(ns('anno_status'))
      )
      
    )
  )
  
  ))
}

# Module Server

#' @rdname mod_study_lead
#' @export
#' @keywords internal

mod_study_lead_server <- function(input, output, session){
  ns <- session$ns
  
  # filter the data
  plotdata1 <- reactive({
    projectLive::studies %>% 
      dplyr::filter(fundingAgency == input$funder)
  })
  
  plotdata2 <- reactive({
    projectLive::files %>% 
      dplyr::filter(fundingAgency == input$funder)
  })
  
  anno_data <- reactive({
    data1 <- as.data.frame(plotdata1())
    data2 <- as.data.frame(plotdata2())
    data2 <- data2 %>% 
      mutate(year= lubridate::year(data2$createdOn)) 
    
    data <- merge(data1[,c("studyLeads", "studyName")], data2, by= "studyName")
    
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == FALSE] <- "Annotated"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"

    data %>% 
      filter(studyLeads == input$studylead)
    
  })
  
  
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {input$funder}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$upload_status <- plotly::renderPlotly({
    
    data1 <- as.data.frame(plotdata1())
    data2 <- as.data.frame(plotdata2())
    data2 <- data2 %>% 
      mutate(year= lubridate::year(data2$createdOn)) 
    
    data <- merge(data1[,c("studyLeads", "studyName")], data2, by= "studyName")
    
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"
    
    #make plot
    ggplot(data, aes(x=studyLeads, fill=resourceType, color=resourceType)) + 
      geom_bar(stat= "count", alpha=0.8, position="stack") +
      coord_flip() +
      scale_fill_manual(values=color_list[[4]])+
      scale_color_manual(values=color_list[[4]])+
      labs(title="", y = "Number of files uploaded") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10, angle = 45),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="none") +
      facet_grid(. ~ year, scales="free")
  })
  
  
  output$anno_status <- plotly::renderPlotly({
    
    plot_anno_data <- as.data.frame(anno_data())
    
    validate(need(length(plot_anno_data$studyLeads) > 0 , 
                  "The selected investigator/investigators is/are not funded by the selected funder. Please modify your selections to include an investigator from the above list."))
    
    validate(need(length(plot_anno_data$assay) > 0 , 
                  "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
    
    
    #make plot
    ggplot(plot_anno_data, aes(x=studyLeads, fill=assay, color=assay)) + 
      geom_bar(stat= "count", alpha=0.8, position="stack") +
      coord_flip() +
      scale_fill_manual(values=color_list[[4]])+
      scale_color_manual(values=color_list[[4]])+
      labs(title="", y = "Number of files annotated") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="none") +
      facet_grid(. ~ year, scales="fixed")
    
  })
  
}

## To be copied in the UI
# mod_study_lead_ui("study_lead_ui")

## To be copied in the server
# callModule(mod_study_lead_server, "study_lead_ui")

