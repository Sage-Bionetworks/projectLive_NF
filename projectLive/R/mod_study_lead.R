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
            plotly::plotlyOutput(ns('upload_status'))
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
            plotly::plotlyOutput(ns('annotation_status'))
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
  
  output$upload_status <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "upload_status"
    )
    
    data <- merged_table() %>% 
      concatenate_df_list_columns_with_param_list(param_list) %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list)  
    
    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    create_upload_status_plot(
      data, 
      x = purrr::pluck(param_list, "columns", "x", "display_name"),
      fill = purrr::pluck(param_list, "columns", "fill", "display_name"),
      color = purrr::pluck(param_list, "columns", "fill", "display_name"),
      purrr::pluck(param_list, "columns", "facet", "display_name")
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
    
    choices <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
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
  
  output$annotation_status <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config, input$studylead)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "annotation_status"
    )
    
    data <- merged_table() %>% 
      dplyr::filter(purrr::map_lgl(
        .data$studyLeads, 
        ~input$studylead %in% .x
      )) %>% 
      concatenate_df_list_columns_with_param_list(param_list) %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list)  
      

    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    create_annotation_status_plot(
      data, 
      x = purrr::pluck(param_list, "columns", "x", "display_name"),
      fill = purrr::pluck(param_list, "columns", "fill", "display_name"),
      color = purrr::pluck(param_list, "columns", "fill", "display_name"),
      purrr::pluck(param_list, "columns", "facet", "display_name")
    )
  })
}


