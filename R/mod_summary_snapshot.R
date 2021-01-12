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
            title = "Initiative Activity", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("initiative_activity"))
          ),
          shinydashboard::box(
            title = "Resources Generated", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            plotly::plotlyOutput(ns("resources_generated"))
          ),
          shinydashboard::box(
            title = "File Upload Timeline", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = 1000,
            collapsible = FALSE,
            shiny::uiOutput(ns("file_upload_timeline_filter_ui")),
            plotly::plotlyOutput(ns('file_upload_timeline'))
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
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies moderated by {group_object()$selected_group}. 
      Please hover your cursor over the plots to view more information. 
      You can also zoom into parts of the plot."
    ))
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
  
  output$initiative_activity <- plotly::renderPlotly({
    shiny::req(data_config, group_object())
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "initiative_activity"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      format_plot_data_with_param_list(param_list)
    
    shiny::validate(shiny::need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data,
      param_list,
      "create_initiative_activity_plot"
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
      "create_resources_generated_plot"
    ) 
  })
  
  merged_table <- shiny::reactive({

    shiny::req(group_object(), data_config)

    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "merged_table"
    )

    create_merged_table_with_param_list(group_object(), param_list)
  })
  
  output$file_upload_timeline_filter_ui <- shiny::renderUI({
    
    shiny::req(merged_table(), data_config)
    
    column <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "file_upload_timeline",
      "filter_column"
    ) 
    
    choices <- merged_table() %>% 
      dplyr::pull(column) %>% 
      unlist(.) %>% 
      unique() %>% 
      sort() %>% 
      c("All", .) 
    
    shiny::selectInput(
      inputId = ns("file_upload_timeline_filter_value"),
      label   = "Select an initiative",
      choices = choices
    )
  })
  
  output$file_upload_timeline <- plotly::renderPlotly({
    
    shiny::req(
      merged_table(), 
      data_config, 
      input$file_upload_timeline_filter_value
    )
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "summary_snapshot",
      "outputs",
      "file_upload_timeline"
    )

    if (input$file_upload_timeline_filter_value != "All"){
      data <- merged_table() %>% 
        filter_list_column(
          param_list$filter_column, 
          input$file_upload_timeline_filter_value
        )
    } else {
      data <- merged_table() 
    }
    
    data <- data %>% 
      format_plot_data_with_param_list(param_list) %>% 
      dplyr::mutate("Study Name" = stringr::str_trunc(.data$`Study Name`, 40)) %>% 
      create_plot_count_df(
        factor_columns   = c(param_list$plot$x), 
        complete_columns = c(param_list$plot$x, param_list$plot$facet)
      ) 
    
    validate(need(sum(data$Count) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data, param_list, "create_file_upload_timeline_plot", height = 870
    ) %>%
      plotly::layout(
        autosize = T
      )
  })
  
}
