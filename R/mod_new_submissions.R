# Module UI

#' @title   mod_new_submissions_ui and mod_new_submissions_server
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
mod_new_submissions_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(disable = T),
      shinydashboard::dashboardSidebar(disable = T),
      shinydashboard::dashboardBody(
        shiny::fluidPage(
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::textOutput(ns('funding_agency'))
          ),
          shinydashboard::box(
            title = "New Files",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::numericInput(
              ns("new_files_day_choice"),
              "Display files uploaded within the last N days:",
              62,
              min = 1,
              step = 1
            ),
            DT::dataTableOutput(ns('new_files_dt')),
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_new_submissions
#' @export
#' @keywords internal

mod_new_submissions_server <- function(
  input, output, session, group_object, data_config
){
  ns <- session$ns
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing recent additions to studies moderated by {group_object()$selected_group}"
    ))
  })
  
  new_files_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config, input$new_files_day_choice)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "new_submissions",
      "outputs",
      "new_files_table"
    )
    
    minimum_date <- 
      lubridate::now() - 
      lubridate::ddays(input$new_files_day_choice)
    
    files_table <- group_object() %>%
      purrr::pluck("files_table") %>% 
      dplyr::filter(!!rlang::sym(param_list$filter_column) > minimum_date) %>% 
      dplyr::arrange(!!rlang::sym(param_list$filter_column))
    
    studies_table <- group_object() %>%
      purrr::pluck("studies_table") 

    data1 <- files_table %>%
      dplyr::inner_join(
        dplyr::select(studies_table, studyName, studyLeads),
        by = "studyName"
      )

    data2 <- files_table %>%
      dplyr::filter(!id %in% data1$id) %>%
      dplyr::select(-studyName) %>%
      dplyr::inner_join(
        dplyr::select(studies_table, studyName, studyId, studyLeads),
        by = c("projectId" = "studyId")
      )

    data3 <- files_table %>%
      dplyr::filter(!id %in% c(data1$id, data2$id)) %>%
      dplyr::select(-studyName) %>%
      dplyr::inner_join(
        dplyr::select(studies_table, studyName, studyId, studyLeads),
        by = c("benefactorId" = "studyId")
      ) 
    
    data_a <-  
      dplyr::bind_rows(data1, data2, data3) %>% 
      format_plot_data_with_param_list(param_list$table1)

    data_b <- files_table %>%
      dplyr::filter(!id %in% c(data1$id, data2$id, data3$id)) %>%
      dplyr::mutate(
        "studyName" = NA_character_, "studyLeads" = NA_character_
      ) %>% 
      format_plot_data_with_param_list(param_list$table2)
      
    
    data <- dplyr::bind_rows(data_a, data_b)
    
    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    return(data)
  })
  
  output$new_files_dt <- DT::renderDataTable(
    new_files_table(),
    server = TRUE, 
    selection = 'single'
  )

}
