# Module UI

#' @title   mod_study_lead_ui and mod_study_lead_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_study_lead
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import ggplot2
#' @import plotly
mod_study_lead_ui <- function(id){
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
        
        box(title = "Funding Partner",
            width = 12,
            solidHeader = T,
            status = "primary",
<<<<<<< HEAD
            shiny::textOutput(ns('funding_agency'))
          ),
          shinydashboard::box(
            title = "File Upload Timeline", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            height = 800,
            collapsible = FALSE,
            plotly::plotlyOutput(ns('file_upload_timeline'))
          ),
          shinydashboard::box(
            title = "Annotation Activity", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            shiny::uiOutput(ns("study_lead_ui")),
              # shiny::selectInput(ns("time"), 
              #                    label = "Select a time window", 
              #                    choices = c("year", "month"),
              #                    selected = "year", 
              #                    multiple = F),
            plotly::plotlyOutput(ns('annotation_activity'))
          )
          
        )
=======
            shiny::textOutput(ns('funding_agency')),
            #DT::dataTableOutput(ns('study_table'))
        ),
      
      
      box(title = "File Upload Timeline", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('upload_status'))
      ),
      
      box(title = "Annotation Activity", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          shiny::uiOutput(ns("study_lead_ui")),
          # shiny::selectInput(ns("time"), 
          #                    label = "Select a time window", 
          #                    choices = c("year", "month"),
          #                    selected = "year", 
          #                    multiple = F),
          plotly::plotlyOutput(ns('anno_status'))
>>>>>>> master
      )
      
    )
  )
  
  ))
}

# Module Server

#' @rdname mod_study_lead
#' @export
#' @keywords internal

mod_study_lead_server <- function(input, output, session, funding_partner){
  ns <- session$ns
  
<<<<<<< HEAD
  output$funding_agency <- shiny::renderText({
    print(glue::glue(
      "You are now viewing studies funded by {group_object()$selected_group}.
      Please hover your cursor over the plots to view more information.
      You can also zoom into parts of the plot."
    ))
  })
  
  merged_table <- shiny::reactive({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "merged_table"
    )
    
    group_object() %>% 
      .[unlist(param_list$tables)] %>% 
      purrr::reduce(dplyr::left_join, by = param_list$join_column)
  })
  
  output$file_upload_timeline <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "file_upload_timeline"
    )
    
    data <- merged_table() %>% 
      format_plot_data_with_param_list(param_list) %>% 
      dplyr::mutate("Study Leads" = forcats::as_factor(`Study Leads`)) %>% 
      tidyr::drop_na() %>% 
      dplyr::count(`Study Leads`, `Resource Type`, `Year`, name = "Count") %>% 
      tidyr::complete(`Study Leads`, `Resource Type`, `Year`, fill = list("Count" = 0)) %>% 
      dplyr::mutate("Resource Type" = dplyr::if_else(
        .data$Count == 0,
        NA_character_,
        .data$`Resource Type`
      )) %>% 
      dplyr::distinct()
    
    validate(need(nrow(data) > 0, param_list$empty_table_message))
    
    create_plot_with_param_list(
      data, param_list, "create_file_upload_timeline_plot", height = 700
    ) %>%
    plotly::layout(autosize = T)
  })
  
  output$study_lead_ui <- shiny::renderUI({
    
    shiny::req(group_object(), data_config)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "study_lead_ui"
    )
    
    choices <- merged_table() %>% 
      dplyr::pull(param_list$column) %>% 
      unlist(.) %>% 
=======
  # filter the data
  plotdata1 <- reactive({
    projectLive::studies %>% 
      dplyr::filter(fundingAgency == funding_partner())
  })
  
  plotdata2 <- reactive({
    projectLive::files %>% 
      dplyr::filter(fundingAgency == funding_partner())
  })
  
  output$study_lead_ui <- shiny::renderUI({
    choices <- 
      plotdata1() %>% 
      dplyr::pull("studyLeads") %>% 
      purrr::flatten_chr(.) %>% 
>>>>>>> master
      unique() %>% 
      sort() 
      
    shiny::selectInput(
      ns("studylead"),
      label = "Select a principal investigator",
      choices = choices
    )
  })

  
<<<<<<< HEAD
  output$annotation_activity <- plotly::renderPlotly({
    
    shiny::req(merged_table(), data_config, input$studylead)
    
    param_list <- purrr::pluck(
      data_config,
      "modules",
      "study_lead",
      "outputs",
      "annotation_activity"
    )
    
    data <- merged_table() %>% 
      dplyr::filter(purrr::map_lgl(
        .data$studyLeads, 
        ~input$studylead %in% .x
      )) %>% 
      format_plot_data_with_param_list(param_list) %>% 
      dplyr::mutate("Study Leads" = forcats::as_factor(`Study Leads`)) %>% 
      tidyr::drop_na() %>% 
      dplyr::count(`Study Leads`, `Assay`, `Year`, name = "Count") %>% 
      tidyr::complete(`Study Leads`, `Assay`, `Year`, fill = list("Count" = 0)) %>% 
      dplyr::mutate("Assay" = dplyr::if_else(
        .data$Count == 0,
        NA_character_,
        .data$`Assay`
      )) %>% 
      dplyr::distinct()
=======
  # shiny::observeEvent(plotdata1(), {
  # 
  #   shiny::updateSelectInput(session = session, 
  #                            inputId = "studylead", 
  #                            choices = sort(unique(as.data.frame(plotdata1())$studyLeads)))
  # })
  
  anno_data <- reactive({
    data1 <- dplyr::select(plotdata1(), "studyLeads", "studyName")
    plotdata2() %>% 
      dplyr::inner_join(data1, by = "studyName") %>% 
      dplyr::filter(purrr::map_lgl(studyLeads, ~input$studylead %in% .x)) %>% 
      dplyr::mutate(
        "year" = synapse_dates_to_year(createdOn),
        "assay" = dplyr::if_else(
          is.na(assay),
          "Pending Annotation",
          "Annotated"
        ),
        "studyLeads" = purrr::map_chr(
          studyLeads, stringr::str_c, collapse = " | "
        ),
      ) %>% 
      dplyr::select("studyLeads", "assay", "year") 
  })
  
  
  
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {funding_partner()}. Please hover your cursor over the plots to view more information. You can also zoom into parts of the plot."))
  })
  
  output$upload_status <- plotly::renderPlotly({
    
    data1 <- dplyr::select(plotdata1(), "studyLeads", "studyName")
    data <- plotdata2() %>% 
      dplyr::inner_join(data1, by = "studyName") %>% 
      dplyr::mutate(
        "year" = synapse_dates_to_year(createdOn),
        "studyLeads" = purrr::map_chr(
          studyLeads, stringr::str_c, collapse = " | "
        ),
        "studyName" = dplyr::if_else(
          is.na(studyName),
          "Pending Annotation",
          studyName
        )
      ) %>% 
      dplyr::select("studyName", "studyLeads", "resourceType", "year") 
>>>>>>> master

    
<<<<<<< HEAD
    create_plot_with_param_list(
      data, param_list, "create_annotation_activity_plot"
    )
=======
    validate(need(nrow(data) > 0 , 
                  "The investigators have not uploaded any files yet. Please check back later."))
    
    #make plot
    ggplot(data, aes(x=studyLeads, fill=resourceType, color=resourceType)) + 
      geom_bar(stat= "count", alpha=0.8, position="stack") +
      coord_flip() +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of files uploaded") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10, angle = 45),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="right",
            panel.grid.major.y = element_blank(),
            panel.background = element_rect(fill = "grey95")) +
      facet_grid(. ~ year, scales="free")
>>>>>>> master
  })
  
  #
  
  
  output$anno_status <- plotly::renderPlotly({
    
    validate(need(nrow(anno_data()) > 0 , 
                  "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
    
    #make plot
    ggplot(anno_data(), aes(x=studyLeads, fill=assay, color=assay)) + 
      geom_bar(stat= "count", alpha=0.8, position="stack") +
      coord_flip() +
      viridis::scale_color_viridis(discrete=TRUE) +
      viridis::scale_fill_viridis(discrete=TRUE) +
      labs(title="", y = "Number of experimental data files") +
      #ylim(0, 5) +
      theme_bw() +
      theme(legend.text = element_blank(), #element_text(size=8), 
            axis.text.x  = element_text(size=10),
            axis.text.y = element_text(size=10),
            text = element_text(size=10),
            strip.text.x = element_text(size = 10),
            legend.position="right",
            panel.grid.major.y = element_blank(),
            panel.background = element_rect(fill = "grey95")) +
      facet_grid(. ~ year, scales="fixed")
    
  })
  
}

## To be copied in the UI
# mod_study_lead_ui("study_lead_ui")

## To be copied in the server
# callModule(mod_study_lead_server, "study_lead_ui")

