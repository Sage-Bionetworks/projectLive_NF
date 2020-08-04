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
            plotly::plotlyOutput(ns('study_per_consortium'))
          ),
          shinydashboard::box(
            title = "Resources Generated", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('files_per_study'))
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_summary_snapshot
#' @export
#' @keywords internal

mod_summary_snapshot_server <- function(
  input, output, session, group_object, data_config
){
  ns <- session$ns
  
  files_table <- shiny::reactive({
    shiny::req(group_object())
    group_object()$files_table
  })
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {group_object()$selected_group}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$box1 <- shinydashboard::renderInfoBox({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "overview_boxes",
      "box1"
    )
    create_info_box(param_list, group_object())
  })
  
  output$box2 <- shinydashboard::renderInfoBox({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "overview_boxes",
      "box2"
    )
    create_info_box(param_list, group_object())
  })
  
  output$box3 <- shinydashboard::renderInfoBox({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "overview_boxes",
      "box3"
    )
    create_info_box(param_list, group_object())
  })
  
  output$box4 <- shinydashboard::renderInfoBox({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "overview_boxes",
      "box4"
    )
    create_info_box(param_list, group_object())
  })
  
  output$study_per_consortium <- plotly::renderPlotly({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "study_per_consortium"
    )
    
    data <- group_object()[[param_list$table]] %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list)
    
    #Catch errors where no files are present
    validate(need(
      nrow(data) > 0 , 
      "The investigator/investigators has/have not uploaded any files yet. Please check back later."
    ))
    
    create_study_per_consortium_plot(
      data  = data, 
      x     = param_list$columns$x$display_name,
      y     = param_list$columns$y$display_name,
      fill  = param_list$columns$fill$display_name,
      color = param_list$columns$fill$display_name,
      param_list$columns$facet$display_name
    )
  })
  
  
  output$files_per_study <- plotly::renderPlotly({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "files_per_study"
    )
    
    data <- group_object()[[param_list$table]] %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list)
    
    validate(need(
      nrow(data) > 0, 
      "The investigator/investigators has/have not uploaded any files yet. Please check back later."
    ))
    create_files_per_study_plot(
      data  = data,
      x     = param_list$columns$x$display_name,
      fill  = param_list$columns$fill$display_name,
      color = param_list$columns$fill$display_name,
      param_list$columns$facet$display_name
    )
  })
}
