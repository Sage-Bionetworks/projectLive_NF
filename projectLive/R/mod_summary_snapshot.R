# Module UI

#' @title   mod_summary_snapshot_ui and mod_summary_snapshot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_summary_snapshot
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
              shiny::textOutput(ns('funding_agency')),
              #DT::dataTableOutput(ns('study_table'))
          ),
          
      
      box(title = "Overview",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          fluidRow(
          shinydashboard::infoBoxOutput(ns('centersBox'), width = 3),
                        #shinycssloaders::withSpinner(proxy.height = "125px"), ## throws an ui_element error if uncommented
          shinydashboard::infoBoxOutput(ns('filesBox'), width = 3),
          shinydashboard::infoBoxOutput(ns('samplesBox'), width = 3),
          shinydashboard::infoBoxOutput(ns('pubsBox'), width = 3) #,
                      #shinydashboard::infoBoxOutput("pubsBox", width = 3)
                    )),
      
      box(title = "Consortium Activity", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('study_per_consortium'))
      ),
      
      box(title = "Resources Generated", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
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

mod_summary_snapshot_server <- function(input, output, session, group_object){
  ns <- session$ns
  
  files_table <- shiny::reactive({
    shiny::req(group_object())
    group_object()$files_table
  })
  
  pubs_table <- shiny::reactive({
    shiny::req(group_object())
    group_object()$pubs_table 
  })
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {group_object()$selected_group}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$centersBox <- shinydashboard::renderInfoBox({
    shiny::req(files_table())
    n_centers <- files_table() %>% 
      dplyr::pull("projectId") %>% 
      dplyr::n_distinct()

    shinydashboard::infoBox(
      "Studies",
      n_centers,
      icon = icon("university"), ### changed to studies
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$filesBox <- shinydashboard::renderInfoBox({
    shiny::req(files_table())
    n_files <- files_table() %>% 
      dplyr::pull("id") %>% 
      dplyr::n_distinct()

    shinydashboard::infoBox(
      "Files",
      n_files,
      icon = icon("file"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
    )
  })
  
  output$samplesBox <- shinydashboard::renderInfoBox({
    shiny::req(files_table())
    n_samples <- base::sum(
      dplyr::n_distinct(files_table()$individualID),
      dplyr::n_distinct(files_table()$specimenID)
    )
    shinydashboard::infoBox(
      "Samples",
      n_samples,
      icon = icon("tag"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$pubsBox <- shinydashboard::renderInfoBox({
    n_centers <- pubs_table() %>% 
      dplyr::pull("title") %>% 
      dplyr::n_distinct()
    infoBox(
      "Publications",
      n_centers, 
      icon = icon("pencil"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$study_per_consortium <- plotly::renderPlotly({
    data <- files_table() %>% 
      dplyr::select("year", "studyName", "consortium", "accessType") 
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$accessType[is.na(data$accessType) == TRUE] <- "Not Annotated"
    #Catch errors where no files are present
    validate(need(nrow(data) > 0 , 
                  "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
    #make plot
    ggplot(data, aes(x=consortium, y= studyName, fill=accessType, color= accessType)) +
      geom_bar(stat="identity", position= "stack", alpha=0.8, na.rm=TRUE) +
      coord_flip() +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of studies per Consortium") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_text(size=8),
            axis.text.x  = element_blank(), #element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="right",
            panel.grid.major.y = element_blank(),
            panel.background = element_rect(fill = "grey95")) +
    facet_grid(.~ year)
  })
  
  
  output$files_per_study <- plotly::renderPlotly({
    data <- files_table() %>% 
      dplyr::select("year", "studyName", "dataType") 
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$dataType[data$dataType == "drugScreen"] <- "drugScreening"
    data$dataType[data$dataType == "drugCombinationScreen"] <- "drugScreening"
    data$dataType[!data$dataType %in% c("immunofluorescence", "genomicVariants", "geneExpression", "drugScreening", "cellularPhysiology", "chromatinActivity")] <- "Other"
    #Catch errors where no files are present
    validate(need(nrow(data) > 0 , 
                  "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
    #make plot
    ggplot(data, aes(x=dataType, fill=studyName, color= studyName)) + 
      geom_bar(stat="count", position= "stack", alpha=1.0, na.rm=TRUE) +
      coord_flip() +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of files per study") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="none",
            panel.grid.major.y = element_blank(),
            panel.background = element_rect(fill = "grey95")) +
      facet_grid(. ~ year, scales="free")
    
  })
  
}

## To be copied in the UI
# mod_summary_snapshot_ui("summary_snapshot_ui")

## To be copied in the server
# callModule(mod_summary_snapshot_server, "summary_snapshot_ui")

