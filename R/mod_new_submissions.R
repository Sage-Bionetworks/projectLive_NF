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
      "You are now viewing new files funded by {group_object()$selected_group}."
    ))
  })
  
  merged_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "new_submissions",
      "outputs",
      "merged_table"
    )
    
    create_merged_table_with_param_list(group_object(), param_list) 
})
  
  new_files_table <- shiny::reactive({
    
    shiny::req(merged_table(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "new_submissions",
      "outputs",
      "new_files_table"
    )
    
    data <- merged_table() %>% 
      dplyr::arrange(dplyr::desc(.data$date)) %>% 
      format_plot_data_with_param_list(param_list) %>% 
      print()

    filtered_data <- data %>% 
      dplyr::filter(.data$Date > (lubridate::now() - lubridate::ddays(7)))
    
    if(nrow(filtered_data) == 0) {
      data <- dplyr::slice(data, 1:10)
    } else {
      data <- filtered_data
    }
  })
  
  output$new_files_dt <- DT::renderDataTable(
    new_files_table(),
    server = TRUE, 
    selection = 'single'
  )

}
