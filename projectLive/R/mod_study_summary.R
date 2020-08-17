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
            shiny::uiOutput(ns("data_focus_selection")),
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
            plotly::plotlyOutput(ns('study_timeline'))
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
  
  merged_dataset <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_summary",
      "outputs",
      "merged_table"
    )
    
    table1 <- group_object()[[param_list$table1]] %>% 
      dplyr::select_at(unlist(param_list$table1_cols))
    
    table2 <- group_object()[[param_list$table2]] %>% 
      dplyr::select_at(unlist(param_list$table2_cols))
    
    table3 <- group_object()[[param_list$table3]] %>% 
      dplyr::select_at(unlist(param_list$table3_cols))
    
    table1 %>%
      dplyr::full_join(table2, by = param_list$join_column1) %>%
      dplyr::left_join(table3, by = param_list$join_column2)
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
    
    merged_dataset() %>% 
      dplyr::select_at(
        unlist(c(param_list$group_columns, param_list$count_columns))
      ) %>% 
      dplyr::group_by_at(unlist(param_list$group_columns))%>% 
      dplyr::summarise_at(unlist(param_list$count_columns), dplyr::n_distinct) %>% 
      dplyr::ungroup() %>% 
      concatenate_df_list_columns_with_param_list(param_list) %>% 
      recode_df_with_param_list(param_list) %>% 
      rename_df_columns_with_param_list(param_list) 
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
    shiny::req(!is.null(input$study_table_rows_selected))
    study_table() %>% 
      dplyr::slice(input$study_table_rows_selected) %>% 
      dplyr::pull("Name")
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
  
  output$data_focus_selection <- shiny::renderUI({
    shiny::req(data_config)
    choices <- data_config %>% 
      purrr::pluck(
        "modules", 
        "study_summary", 
        "outputs", 
        "data_focus", 
        "columns"
      ) %>% 
      purrr::map(purrr::pluck, "name")
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
      merged_dataset(), 
      selected_study_name(),
      input$data_focus_columns
    )
    
    column_list <- c(
      "assay" = "Assays Used",
      "resourceType" = "Resource Added",
      "species" = "Species Used",
      "tumorType" = "Tumor Types Investigated"
    )

    columns <- input$data_focus_columns
    count_columns <- column_list[input$data_focus_columns]
    
    data <- merged_dataset() %>%
      dplyr::select(c("studyName", columns)) %>% 
      dplyr::filter(studyName == selected_study_name()) %>% 
      tidyr::pivot_longer(-"studyName") %>% 
      tidyr::drop_na() %>% 
      dplyr::group_by(.data$studyName, .data$name, .data$value) %>% 
      dplyr::summarise("count" = dplyr::n()) 
    
    dfs <- purrr::map2(
      columns,
      count_columns,
      create_plot_df_from_count_df,
      data
    )

    plots <- purrr::pmap(
      dplyr::tibble(
        "data" = dfs,
        "x" = "studyName",
        "y" = count_columns,
        "color" = columns
      ) %>%
        dplyr::mutate("fill" = .data$color),
      create_study_summary_plot
    )

    plotly::subplot(plots, titleX = TRUE)
    
  })
  
  output$study_timeline <- plotly::renderPlotly({
    shiny::req(merged_dataset(), selected_study_name())
    data <- merged_dataset() %>%
      dplyr::filter(
        .data$studyName == selected_study_name(),
        !is.na(.data$resourceType)
      ) %>% 
      dplyr::select(
        "Study Name" = "studyName", 
        "Resource Type" = "resourceType", 
        "Year" = "year", 
        "Month" = "month"
      ) 
    
    #Catch errors where no files are present
    validate(need(
      nrow(data) > 0 , 
      "The investigator/investigators has/have not uploaded any files yet. Please check back later."
    ))
    
    create_study_summary_grid_plot(
      data, 
      x = `Study Name`, 
      fill = `Resource Type`, 
      color = `Resource Type`,
      Year,
      Month
    )
  
  })
  
  output$study_details <- shiny::renderText({
    
    shiny::req(merged_dataset(), selected_study_name())
    
    merged_dataset() %>%
      dplyr::filter(.data$studyName == selected_study_name()) %>%
      dplyr::select(
        "projectId", "studyStatus", "dataStatus", "summary", "diseaseFocus"
      ) %>%
      dplyr::distinct() %>% 
      dplyr::mutate(
        "diseaseFocus" = purrr::map_chr(.data$diseaseFocus, stringr::str_c, collapse = " | ")
      ) %>% 
      tidyr::pivot_longer(
        cols = c("projectId", "studyStatus", "dataStatus", "summary", "diseaseFocus")
      ) %>%
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

