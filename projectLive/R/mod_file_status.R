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
            shiny::textOutput(ns('funding_agency')),
          ),
          shinydashboard::box(
            title = "Publication Status", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('publication_status'))
          ),
          shinydashboard::box(
            title = "Publication status by Disease Manifestation", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('publication_disease'))
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_file_status
#' @export
#' @keywords internal

mod_file_status_server <- function(
  input, output, session, group_object, data_config
){
  ns <- session$ns
  
  publications_table <- shiny::reactive({
    shiny::req(group_object())
    group_object()$publications_table %>% 
      dplyr::select("year", "studyName", "manifestation")
  })

  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies funded by {group_object()$selected_group}. 
      Please hover your cursor over the plots to view more information. 
      You can also zoom into parts of the plot."
    ))
  })
  
  output$publication_status <- plotly::renderPlotly({
    
    shiny::req(data_config, group_object())
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "file_status",
      "outputs",
      "publication_status"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      concatenate_df_list_columns_with_param_list(param_list) %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list) 
    
    create_publication_status_plot(
      data, 
      x     = purrr::pluck(param_list, "columns", "x", "display_name"),
      fill  = purrr::pluck(param_list, "columns", "fill", "display_name")
    ) %>% 
      plotly::ggplotly(
        tooltip = c("count", param_list$columns$fill$display_name)
      )
  })
  
  output$publication_disease <- plotly::renderPlotly({
    
    shiny::req(data_config, group_object())
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "file_status",
      "outputs",
      "publication_disease"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      concatenate_df_list_columns_with_param_list(param_list) %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list) 
    
    create_publication_disease_plot(
      data, 
      x     = purrr::pluck(param_list, "columns", "x", "display_name"),
      fill  = purrr::pluck(param_list, "columns", "fill", "display_name")
    ) %>% 
      plotly::ggplotly(
        tooltip = c("count", param_list$columns$fill$display_name)
      )
  })
  
}


