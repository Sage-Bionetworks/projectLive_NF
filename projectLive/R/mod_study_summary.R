# Module UI

#' @title   mod_study_summary_ui and mod_study_summary_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_study_summary
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import plotly
mod_study_summary_ui <- function(id){
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
          # 
          
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
          
          
          box(title = "Participating Studies",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              DT::dataTableOutput(ns('study_table')),
          ),
          
          box(title = "",
              status = "primary",
              solidHeader = F,
              width = 12,
              collapsible = FALSE,
              shinydashboard::infoBoxOutput(ns('study'), width = 12)
          ),
              
          
          box(title = "Resources Added",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              # shiny::selectizeInput(ns('variable'),
              #                       label = "Choose to view", 
              #                       choices = c("resourceType", "tumorType", "assay"),
              #                       selected = "resourceType", 
              #                       multiple = F),
              #shinydashboard::infoBoxOutput(ns('study'), width = 12),
              #shiny::textOutput(ns('study')),
              plotly::plotlyOutput(ns('study_timeline'))
          ),

          box(title = "Study Summary",
              status = "primary",
              solidHeader = T,
              width = 12,
              collapsible = FALSE,
              shiny::htmlOutput(ns('study_details'))
          )
          
        )
      )))
}

# Module Server

#' @rdname mod_study_summary
#' @export
#' @keywords internal

mod_study_summary_server <- function(input, output, session){
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
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {input$funder}. Please select a study from the table below to view the details."))
  })
  
  output$study_table <- DT::renderDataTable({
    (as.data.frame(plotdata1()) %>% 
      dplyr::select(studyName, studyStatus, dataStatus, studyLeads, diseaseFocus, manifestation))
  }, server = TRUE, selection = 'single')
  
  
  shiny::observeEvent(input$study_table_rows_selected, {
    
    output$study_timeline <- plotly::renderPlotly({
      
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- plotdata1()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      #Prep the files and studies df to get info
      data1 <- as.data.frame(plotdata1())
      data2 <- as.data.frame(plotdata2())
      data2 <- data2 %>%
        mutate(year= lubridate::year(data2$createdOn)) %>% 
        mutate(month= lubridate::month(data2$createdOn,label = TRUE, abbr = FALSE))
      data <- merge(data1[,c("studyLeads", "studyName")], data2, by= "studyName")
      data <- data %>%
        dplyr::filter(studyName == selected_studyName)
      
      #Remind user of selected studyName 
      output$study <- shinydashboard::renderInfoBox({
        selected_data <- plotdata1()[selected_study, ]
        selected_studyName <- selected_data$studyName
        
        shinydashboard::infoBox(
          "You have selected",
          selected_studyName,
          icon = icon("file"),
          color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
          fill = FALSE
        )
      })
      
      #Catch errors where no files are present
      validate(need(length(data$resourceType) > 0 , 
                    "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
      
      #populate the choose menu
      # shiny::observeEvent(data(), {
      #   shiny::updateSelectInput(session = session, 
      #                            inputId = "variable", 
      #                            choices = c("resourceType", "tumorType", "assay"))
      # })

      #Plot the results
      ggplot(data, aes(x=studyName, fill=resourceType, color=resourceType)) +
        geom_bar(stat= "count", alpha=0.8, position="stack") +
        #coord_flip() +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="left",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) +
        facet_grid(. ~year+month, scales="fixed", labeller = label_value)
    
      })
    
    output$study_details <- shiny::renderText({
      
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- plotdata1()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      center_data_df <- selected_data %>%
        base::as.data.frame() %>% 
        dplyr::filter(studyName == selected_studyName) %>%
        dplyr::select(studyId, studyStatus, dataStatus, summary, diseaseFocus) %>%
        tidyr::gather(field, val) %>%
        dplyr::mutate(field = stringr::str_to_title(field),
                      field = stringr::str_c("<b>", field, "</b>")
        ) 
      
      knitr::kable(center_data_df, "html", escape = FALSE, col.names = NULL,
                   align = c('r', 'l')) %>%
        kableExtra::kable_styling("striped", full_width = T)
    })
      
  })

  
}

  
  


## To be copied in the UI
# mod_summary_snapshot_ui("study_summary_ui")

## To be copied in the server
# callModule(mod_study_summary_server, "study_summary_ui")

