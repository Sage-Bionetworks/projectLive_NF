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
mod_summary_snapshot_ui <- function(id, funding_partner){
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
<<<<<<< HEAD
          
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('funding_agency'))
          ),
          shinydashboard::box(
            title = "Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::fluidRow(
              shinydashboard::infoBoxOutput(ns('box1'), width = 3),
              shinydashboard::infoBoxOutput(ns('box2'), width = 3),
              shinydashboard::infoBoxOutput(ns('box3'), width = 3),
              shinydashboard::infoBoxOutput(ns('box4'), width = 3)
            )
          ),
          shinydashboard::box(
            title = "Consortium Activity", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("consortium_activity"))
          ),
          shinydashboard::box(
            title = "Resources Generated", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = 800,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("resources_generated"))
          )
        )
=======
  
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
>>>>>>> master
      )
      
    )
  )))
}

# Module Server

#' @rdname mod_summary_snapshot
#' @export
#' @keywords internal

mod_summary_snapshot_server <- function(input, output, session, funding_partner){
  ns <- session$ns
  
  # filter the data
  plotdata <- reactive({
    projectLive::files %>% 
      dplyr::filter(fundingAgency == funding_partner()) 
  })
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {funding_partner()}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$centersBox <- shinydashboard::renderInfoBox({
    data <- as.data.frame(plotdata())
    
    centers <- as.numeric(dplyr::n_distinct(data$projectId))

    shinydashboard::infoBox(
      "Studies",
      centers,
      icon = icon("university"), ### changed to studies
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$filesBox <- shinydashboard::renderInfoBox({
    data <- as.data.frame(plotdata())
    files <- dplyr::n_distinct(data$id)

    shinydashboard::infoBox(
      "Files",
      files,
      icon = icon("file"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
    )
  })
  
  output$samplesBox <- shinydashboard::renderInfoBox({
    data <- as.data.frame(plotdata())
    samples <- base::sum(
      dplyr::n_distinct(data$individualID),
      dplyr::n_distinct(data$specimenID)
    )

    shinydashboard::infoBox(
      "Samples",
      samples,
      icon = icon("tag"),
      color = "light-blue",
      fill = TRUE
    )
  })
  # 
  pubs <- reactive({
    projectLive::pubs %>%
      dplyr::filter(fundingAgency == funding_partner())
  })
  
  output$pubsBox <- shinydashboard::renderInfoBox({
    pubdata <- as.data.frame(pubs())
    
   pubinfo <- pubdata %>% dplyr::tally()
    infoBox(
      "Publications", pubinfo, icon = icon("pencil"),
      color = "light-blue", fill = TRUE
    )
  })
  
<<<<<<< HEAD
  output$consortium_activity <- plotly::renderPlotly({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "consortium_activity"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      format_plot_data_with_param_list(param_list)
    
    shiny::validate(shiny::need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data,
      param_list,
      "create_consortium_activity_plot"
    )
  })
  
  
  output$resources_generated <- plotly::renderPlotly({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "resources_generated"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      format_plot_data_with_param_list(param_list)
      
    shiny::validate(shiny::need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data,
      param_list,
      "create_resources_generated_plot",
      height = 700
    ) %>%
    plotly::layout(
      autosize = T, 
      legend = list(
        orientation = "v", 
        x = 0.25, 
        y = -1.5, 
        title = list(
          text = '\n Double-click on individual studies below to see yearly additions of resources in the plot above \n'
        ),
        bgcolor = "#E9EAEC",
        bordercolor = "#676E79",
        borderwidth = 1
      )
    )
=======
  output$study_per_consortium <- plotly::renderPlotly({
    
    data <- plotdata() %>%
      dplyr::mutate("year" = synapse_dates_to_year(createdOn)) 
    
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"
    data$accessType[is.na(data$accessType) == TRUE] <- "Not Annotated"
    
    #Catch errors where no files are present
    validate(need(length(data$resourceType) > 0 , 
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
    
    data <- plotdata()
    data <- data %>% 
      dplyr::mutate("year" = synapse_dates_to_year(createdOn)) 
    
    data$studyName[is.na(data$studyName) == TRUE] <- "Not Annotated"
    data$consortium[is.na(data$consortium) == TRUE] <- "Not Applicable"
    data$assay[is.na(data$assay) == TRUE] <- "Not Annotated"
    data$dataType[data$dataType == "drugScreen"] <- "drugScreening"
    data$dataType[data$dataType == "drugCombinationScreen"] <- "drugScreening"
    data$dataType[!data$dataType %in% c("immunofluorescence", "genomicVariants", "geneExpression", "drugScreening", "cellularPhysiology", "chromatinActivity")] <- "Other"
    
    #Catch errors where no files are present
    validate(need(length(data$resourceType) > 0 , 
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
    
>>>>>>> master
  })
  
}

## To be copied in the UI
# mod_summary_snapshot_ui("summary_snapshot_ui")

## To be copied in the server
# callModule(mod_summary_snapshot_server, "summary_snapshot_ui")

