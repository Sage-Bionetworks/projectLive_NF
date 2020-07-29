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
            shiny::textOutput(ns('funding_agency')),
            #DT::dataTableOutput(ns('study_table'))
        ),
      
      
      box(title = "Yearly Upload Status", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotly::plotlyOutput(ns('upload_status'))
      ),
      
      box(title = "Yearly Annotation Status", 
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
      unique() %>% 
      sort() 
      
    shiny::selectInput(
      ns("studylead"),
      label = "Select a principal investigator",
      choices = choices
    )
  })

  
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
          "Not Annotated",
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
          "Not Annotated",
          studyName
        )
      ) %>% 
      dplyr::select("studyName", "studyLeads", "resourceType", "year") 

    
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
      labs(title="", y = "Number of files annotated") +
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

