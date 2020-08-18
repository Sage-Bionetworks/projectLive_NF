# Module UI

#' @title   mod_study_lead_ui and mod_study_lead_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_study_lead
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import plotly
mod_study_lead_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
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
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('funding_agency'))
          ),
          shinydashboard::box(
            title = "File Upload Timeline", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('file_upload_timeline'))
          ),
          shinydashboard::box(
            title = "Annotation Activity", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::uiOutput(ns("study_lead_ui")),
              # shiny::selectInput(ns("time"), 
              #                    label = "Select a time window", 
              #                    choices = c("year", "month"),
              #                    selected = "year", 
              #                    multiple = F),
            plotly::plotlyOutput(ns('annotation_activity'))
          )
          
        )
      )
      
    ))
}

# Module Server

#' @rdname mod_study_lead
#' @export
#' @keywords internal

mod_study_lead_server <- function(
  input, output, session, group_object, data_config
){
  ns <- session$ns
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies funded by {group_object()$selected_group}.
      Please hover your cursor over the plots to view more information.
      You can also zoom into parts of the plot."
    ))
  })
  
  merged_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "merged_table"
    )
    
    group_object() %>% 
      .[unlist(param_list$tables)] %>% 
      purrr::reduce(dplyr::inner_join, by = param_list$join_column) 
  })
  
  output$file_upload_timeline <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "file_upload_timeline"
    )
    
    data <- merged_table() %>% 
      format_plot_data_with_param_list(param_list) 
    
    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data, param_list, "create_file_upload_timeline_plot"
    )
  })
  
  output$study_lead_ui <- shiny::renderUI({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "study_lead_ui"
    )
    
    choices <- merged_table() %>% 
      dplyr::pull(param_list$column) %>% 
      unlist(.) %>% 
      unique() %>% 
      sort() 
    
    shiny::selectInput(
      inputId = ns("studylead"),
      label   = "Select a principal investigator",
      choices = choices
    )
  })
  
  output$annotation_activity <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config, input$studylead)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "annotation_activity"
    )
    
    data <- merged_table() %>% 
      dplyr::filter(purrr::map_lgl(
        .data$studyLeads, 
        ~input$studylead %in% .x
      )) %>% 
      format_plot_data_with_param_list(param_list) 

    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data, param_list, "create_annotation_activity_plot"
    )
  })
}


