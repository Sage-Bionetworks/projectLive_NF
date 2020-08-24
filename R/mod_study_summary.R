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
            title = "Participating Studies",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            DT::dataTableOutput(ns('study_table')),
          ),
          shinydashboard::box(title = "",
              status = "primary",
              solidHeader = F,
              width = 12,
              collapsible = FALSE,
              shinydashboard::infoBoxOutput(ns('study'), width = 12)
          ),
          shinydashboard::box(
            title = "Data Focus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            #shinydashboard::infoBoxOutput(ns('study'), width = 12),
            #shiny::textOutput(ns('study')),
            shiny::uiOutput(ns("data_focus_selection_ui")),
            plotly::plotlyOutput(ns('data_focus_plot'))
          ),
          shinydashboard::box(
            title = "Study Timeline",
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
            plotly::plotlyOutput(ns('study_timeline_plot'))
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
            title = "Study Summary",
            status = "primary",
            solidHeader = T,
            width = 12,
            collapsible = FALSE,
            shiny::htmlOutput(ns('study_details'))
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_study_summary
#' @export
#' @keywords internal

mod_study_summary_server <- function(
  input, output, session, group_object, data_config
){
  ns <- session$ns
  
  merged_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "merged_table"
    )
    
    create_merged_table_with_param_list(group_object(), param_list)
    
  })
  
  study_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "study_table"
    ) 
    
    merged_table() %>% 
      dplyr::select_at(
        unlist(c(param_list$group_columns, param_list$count_columns))
      ) %>% 
      dplyr::group_by_at(unlist(param_list$group_columns))%>% 
      dplyr::summarise_at(
        unlist(param_list$count_columns), 
        dplyr::n_distinct,
        na.rm = T
      ) %>% 
      dplyr::ungroup() %>% 
      format_plot_data_with_param_list(param_list) %>% 
      dplyr::arrange(!!rlang::sym(param_list$id_column))
  })
  
  ##start making outputs
  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies funded by {group_object()$selected_group}. 
      Please select a study from the table below to view the details."
    ))
  })
  
  output$study_table <- DT::renderDataTable(
    base::as.data.frame(study_table()), 
    server = TRUE, 
    selection = 'single'
  )
  
  selected_study_name <- shiny::reactive({
    shiny::req(!is.null(input$study_table_rows_selected), data_config)
    
    column_name <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "study_table",
      "id_column"
    ) 
    
    study_table() %>% 
      dplyr::slice(input$study_table_rows_selected) %>% 
      dplyr::pull(column_name)
  })
  
  output$study <- shinydashboard::renderInfoBox({
    shiny::req(selected_study_name())
    shinydashboard::infoBox(
      "You have selected",
      selected_study_name(),
      icon = shiny::icon("file"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = FALSE
    )
  })
  
  filtered_merged_table <- shiny::reactive({
    column <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "merged_table",
      "filter_column"
    ) 
    shiny::req(merged_table(), selected_study_name())
    filter_list_column(merged_table(), column, selected_study_name()) 
  })
  
  output$data_focus_selection_ui <- shiny::renderUI({
    shiny::req(data_config)
    choices <- data_config %>% 
      purrr::pluck(
        "modules", 
        "study_summary", 
        "outputs", 
        "data_focus", 
        "plot",
        "fill"
      )
    shiny::selectizeInput(
      ns('data_focus_columns'),
      label = "Choose to view",
      choices = choices,
      selected = choices,
      multiple = T
    )
  })
  
  output$data_focus_plot <- plotly::renderPlotly({
    
    shiny::req(
      filtered_merged_table(), 
      input$data_focus_columns,
      data_config
    )
    
    param_list <- data_config %>% 
      purrr::pluck(
        "modules", 
        "study_summary", 
        "outputs", 
        "data_focus"
      ) 
    
    data_list <- filtered_merged_table() %>% 
      format_plot_data_with_param_list(param_list) %>% 
      create_data_focus_tables(param_list$plot$x, input$data_focus_columns)
    
    validate(need(length(data_list) > 0 , param_list$empty_table_message))
    
    create_data_focus_plots(data_list, param_list)
  })
  
  output$study_timeline_plot <- plotly::renderPlotly({
    shiny::req(filtered_merged_table(), data_config)
    
    param_list <- data_config %>% 
      purrr::pluck(
        "modules", 
        "study_summary", 
        "outputs", 
        "study_timeline"
      ) 

    data <- filtered_merged_table() %>%
      format_plot_data_with_param_list(param_list) %>% 
      tidyr::drop_na()
    
    validate(need(nrow(data) > 0 , param_list$empty_table_message))
    
    create_plot_with_param_list(
      data, param_list, "create_study_timeline_plot"
    )
  })
  
  output$publication_status <- plotly::renderPlotly({
    
    shiny::req(data_config, group_object(), selected_study_name())
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "publication_status"
    )
    
    data <- group_object() %>% 
      purrr::pluck(param_list$table) %>% 
      filter_list_column(param_list$filter_column, selected_study_name()) %>% 
      format_plot_data_with_param_list(param_list)
    
    validate(need(nrow(data) > 0 , param_list$empty_table_message))
    
    create_plot_with_param_list(
      data,
      param_list,
      "create_publication_status_plot"
    )
  })
  
  output$study_details <- shiny::renderText({
    
    shiny::req(filtered_merged_table(), data_config)
    
    param_list <- data_config %>% 
      purrr::pluck(
        "modules", 
        "study_summary", 
        "outputs", 
        "study_details"
      ) 

    filtered_merged_table() %>% 
      format_plot_data_with_param_list(param_list) %>% 
      dplyr::distinct() %>% 
      tidyr::pivot_longer(dplyr::everything()) %>% 
      dplyr::mutate(
        "name" = stringr::str_to_title(.data$name),
        "name" = stringr::str_c("<b>", .data$name, "</b>")
      ) %>% 
      knitr::kable(
        "html", escape = FALSE, col.names = NULL, align = c('r', 'l')
      ) %>% 
      kableExtra::kable_styling("striped", full_width = T)
  })
}

  
  


## To be copied in the UI
# mod_summary_snapshot_ui("study_summary_ui")

## To be copied in the server
# callModule(mod_study_summary_server, "study_summary_ui")

