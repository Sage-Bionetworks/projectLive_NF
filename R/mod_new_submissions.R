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
    
    # files_table <- files %>%
    #   dplyr::select("id", "name", "studyName", "createdOn", "projectId", "benefactorId")
    # 
    # studies_table <- studies %>%
    #   dplyr::select("studyName", "studyId", "studyLeads")
    # 
    # data1 <- files_table %>%
    #   dplyr::select(id, name, studyName) %>%
    #   dplyr::inner_join(
    #     dplyr::select(studies_table, studyName, studyLeads),
    #     by = "studyName"
    #   )
    # 
    # data2 <- files_table %>%
    #   dplyr::filter(!id %in% data1$id) %>%
    #   dplyr::select(id, name, projectId) %>%
    #   dplyr::inner_join(
    #     dplyr::select(studies_table, studyName, studyId, studyLeads),
    #     by = c("projectId" = "studyId")
    #   ) %>%
    #   dplyr::select(-projectId)
    # 
    # data3 <- files_table %>%
    #   dplyr::filter(!id %in% c(data1$id, data2$id)) %>% 
    #   dplyr::select(id, name, benefactorId) %>%
    #   dplyr::inner_join(
    #     dplyr::select(studies_table, studyName, studyId, studyLeads),
    #     by = c("benefactorId" = "studyId")
    #   ) %>% 
    #   dplyr::select(-benefactorId)
    # 
    # data4 <- files_table %>%
    #   dplyr::filter(!id %in% c(data1$id, data2$id, data3$id)) %>% 
    #   dplyr::select("id", "name")
    
    # files_table <- files %>% 
    #   dplyr::select("id", "name", "studyName", "createdOn", "projectId", "benefactorId")
    # 
    # studies_table <- studies %>% 
    #   dplyr::select("studyName", "studyId", "studyLeads")
    # 
    # data1 <- files_table %>% 
    #   dplyr::filter(!is.na(studyName)) %>% 
    #   dplyr::select(id, name, studyName) %>% 
    #   dplyr::inner_join(
    #     dplyr::select(studies_table, studyName, studyLeads), 
    #     by = "studyName"
    #   )
    # 
    # data2 <- files_table %>% 
    #   dplyr::filter(!is.na(studyName), !id %in% data1$id) %>% 
    #   dplyr::select(id, name, projectId) %>% 
    #   dplyr::inner_join(
    #     dplyr::select(studies_table, studyName, studyId, studyLeads),
    #     by = c("projectId" = "studyId")
    #   ) %>% 
    #   dplyr::select(-projectId)
    # 
    # data3 <- files_table %>% 
    #   dplyr::filter(
    #     !id %in% c(data1$id, data2$id), 
    #     !is.na(studyName) 
    #   ) %>%
    #   dplyr::select(id, name, studyName) 
    # 
    # data4 <- files_table %>% 
    #   dplyr::filter(is.na(studyName))%>%
    #   dplyr::select(id, name, projectId) %>%
    #   dplyr::inner_join( 
    #     dplyr::select(studies_table, studyName, studyId, studyLeads),
    #     by = c("projectId" = "studyId")
    #   ) %>% 
    #   dplyr::select(-projectId)
    # 
    # data5 <- files_table %>% 
    #   dplyr::filter(
    #     !id %in% data4$id, 
    #     is.na(studyName), 
    #     !projectId == "syn4940963"
    #   ) %>% 
    #   dplyr::select(id, name, benefactorId) %>%
    #   dplyr::inner_join( 
    #     dplyr::select(studies_table, studyName, studyId, studyLeads),
    #     by = c("benefactorId" = "studyId") 
    #   )
    # 
    # data6 <- files_table %>% 
    #   dplyr::filter(!id %in% c(data4$id, data5$id), is.na(studyName)) %>% 
    #   dplyr::select(id, name, studyName, projectId, benefactorId)
  })
  
  output$new_files_dt <- DT::renderDataTable(
    new_files_table(),
    server = TRUE, 
    selection = 'single'
  )

}
