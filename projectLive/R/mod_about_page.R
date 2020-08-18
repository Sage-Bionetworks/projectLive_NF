# Module UI
  
#' @title   mod_about_page_ui and mod_about_page_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_about_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
      dashboardHeader(disable = T),
      dashboardSidebar(disable = T),
      
      dashboardBody(
        fluidPage(
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
          
          # box(title = "About",
          #     width = 12,
          #     solidHeader = T,
          #     status = "primary",
          #     shiny::htmlOutput(ns('about'))
          #     #DT::dataTableOutput(ns('study_table'))
          # ),
          
          # box(title = "About",
          #     status = "primary",
          #     solidHeader = F,
          #     width = 12,
          #     collapsible = FALSE,
              shinydashboard::infoBoxOutput(ns('about'), width = 12),
          #),
          
          box(title = "Funding Partner",
              width = 12,
              solidHeader = T,
              status = "primary",
              shiny::uiOutput(ns("group_selection_ui")),
              shiny::textOutput(ns('group'))
              #DT::dataTableOutput(ns('study_table'))
          )
  
))))
}
    
# Module Server
    
#' @rdname mod_about_page
#' @export
#' @keywords internal
    
mod_about_page_server <- function(input, output, session, syn, data_config){
  
  ns <- session$ns
  
  current_user_synapse_id <- shiny::reactive({
    # code to get the synapse id of the current user here
    # This user has permisions to CTF and NTAP
    return(3389310)
  })
  
  groups_allowed <- shiny::reactive({
    req(syn, data_config, current_user_synapse_id())
    get_allowed_groups_from_synapse_user_id(
      syn, data_config, current_user_synapse_id()
    ) 
  })
  
  output$group_selection_ui <- shiny::renderUI({
    shiny::req(groups_allowed())
    shiny::selectizeInput(
      ns("selected_group"), 
      label = "", 
      choices = groups_allowed(),
      multiple = F
    )
  })
  
  output$group <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies funded by {input$selected_group}. 
    Navigate to the tabs at the top of the page to get more information about the funded investigators and the various resources that they have generated."
    ))
  })
  
 
  output$about <- shinydashboard::renderInfoBox({

    shinydashboard::infoBox(
      " ",
      print("projectLive: Track the progress and impact of our funding partners in real time"),
      icon = shiny::icon("university", "fa-1x"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
    )
  })
  
  selected_group <- shiny::reactive(input$selected_group)
  
  files_table <- shiny::reactive({
    shiny::req(syn, data_config)
    tbl <-
      read_rds_file_from_synapse(
        syn,
        purrr::pluck(data_config, "data_files", "files", "synapse_id")
      ) %>% 
      dplyr::mutate(
        "year" = synapse_dates_to_year(.data$createdOn),
        "month" = synapse_dates_to_month(.data$createdOn)
      ) 
  })
  
  filtered_files_table <- shiny::reactive({
    shiny::req(files_table(), selected_group())
    dplyr::filter(files_table(), selected_group() == .data$fundingAgency) 
  })
  
  publications_table <- shiny::reactive({
    shiny::req(syn, data_config)
    read_rds_file_from_synapse(
      syn,
      purrr::pluck(data_config, "data_files", "publications", "synapse_id")
    ) 
  })
  
  filtered_publications_table <- shiny::reactive({
    shiny::req(publications_table(), selected_group())
    dplyr::filter(
      publications_table(),
      purrr::map_lgl(.data$fundingAgency, ~selected_group() %in% .x)
    )
  })
  
  studies_table <- shiny::reactive({
    shiny::req(syn, data_config)
    read_rds_file_from_synapse(
      syn,
      purrr::pluck(data_config, "data_files", "studies", "synapse_id")
    ) 
  })
  
  filtered_studies_table <- shiny::reactive({
    shiny::req(studies_table(), selected_group())
    tbl <- dplyr::filter( 
      studies_table(),
      purrr::map_lgl(.data$fundingAgency, ~selected_group() %in% .x)
    ) 
  })
  
  tools_table <- shiny::reactive({
    shiny::req(syn, data_config)
    read_rds_file_from_synapse(
      syn,
      purrr::pluck(data_config, "data_files", "tools", "synapse_id")
    ) 
  })
  
  filtered_tools_table <- shiny::reactive({
    shiny::req(tools_table(), selected_group()) 
    dplyr::filter(tools_table(), selected_group() == .data$fundingAgency) 
  })
  
  group_object <- shiny::reactive({
    list(
      "selected_group"     = selected_group(),
      "files_table"        = filtered_files_table(),
      "publications_table" = filtered_publications_table(),
      "studies_table"      = filtered_studies_table(),
      "tools_table"        = filtered_tools_table()
    )
  })
  
  return(group_object)
}
    
## To be copied in the UI
# mod_about_page_ui("about_page_ui_1")
    
## To be copied in the server
# callModule(mod_about_page_server, "about_page_ui_1")
 
