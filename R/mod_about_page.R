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
          shinydashboard::box(
            title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
            shiny::uiOutput(ns("group_selection_ui")),
            shiny::textOutput(ns('group'))
            #DT::dataTableOutput(ns('study_table'))
          ),
          shiny::actionButton(inputId='back_to_portal', label="Back to the NF Data Portal", 
                              icon = icon("map-marker-alt"), lib = "font-awesome",
                              class="btn btn-primary btn-lg btn-block",
                              onclick ="window.open('https://nf.synapse.org/', '_blank')")
          
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
    user <- syn$getUserProfile()[['ownerId']]
    return(user)
  })
  
  output$about <- shinydashboard::renderInfoBox({
    
    shinydashboard::infoBox(
      " ",
      print("projectLive: Track the progress and impact of your funding initiatives in real time"),
      icon = shiny::icon("university", "fa-1x"),
      color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      fill = TRUE
    )
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
      "You are now viewing studies moderated by {input$selected_group}. 
    Navigate to the tabs at the top of the page to get more information about the participating investigators and the various resources that they have generated."
    ))
  })
  
  tables <- shiny::reactive({
    shiny::req(syn, data_config)
    synapse_ids <- data_config %>%
      purrr::pluck("data_files") %>%
      purrr::map_chr("synapse_id") %>%
      purrr::map(read_rds_file_from_synapse, syn) %>%
      purrr::map(format_date_columns)
  })

  filtered_tables <- shiny::reactive({
    shiny::req(tables(), input$selected_group)
    purrr::map(
      tables(),
      filter_list_column,
      data_config$team_filter_column,
      input$selected_group
    )
  })

  group_object <- shiny::reactive({
    shiny::req(filtered_tables(), input$selected_group)
    c("selected_group" = input$selected_group, filtered_tables())
  })
}
    
## To be copied in the UI
# mod_about_page_ui("about_page_ui_1")
    
## To be copied in the server
# callModule(mod_about_page_server, "about_page_ui_1")
 
