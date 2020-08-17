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
          # 
          
          box(title = "Funding Partner",
              width = 12,
              solidHeader = T,
              status = "primary",
              shiny::textOutput(ns('funding_agency')),
              ),
          
          
          box(title = "Participating Studies",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              DT::dataTableOutput(ns('study_table')),
          ),
          
          box(title = "",
              status = "primary",
              solidHeader = F,
              width = 12,
              collapsible = FALSE,
              shinydashboard::infoBoxOutput(ns('study'), width = 12)
          ),
          
          box(title = "Data Focus",
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
              plotly::plotlyOutput(ns('study_data'))
          ),
              
          
          box(title = "Study Timeline",
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

          box(title = "Study Summary",
              status = "primary",
              solidHeader = T,
              width = 12,
              collapsible = FALSE,
              shiny::htmlOutput(ns('study_details'))
          )
          
        )
      )))
}

# Module Server

#' @rdname mod_study_summary
#' @export
#' @keywords internal

mod_study_summary_server <- function(input, output, session, funding_partner){
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
  
  plotdata3 <- reactive({
    projectLive::tools %>% 
      dplyr::filter(fundingAgency == funding_partner())
  })
  
  merged_dataset <- reactive({
    data1 <- plotdata1()
    data2 <- plotdata2()
    data3 <- plotdata3()
    data2 <- data2 %>% 
      dplyr::mutate(
        year = synapse_dates_to_year(createdOn),
        month = synapse_dates_to_month(createdOn)
      ) 
    
    data <- dplyr::full_join(data1[,c("studyName", "studyStatus", "dataStatus", "studyLeads", "diseaseFocus", "summary", "consortium")], data2, by= "studyName")
    table_data <- dplyr::left_join(data, data3[,c("studyName", "softwareName")], by= "studyName")
    table_data
  })
  
  table <- reactive({
    merged_dataset() %>% 
      dplyr::group_by(studyName) %>% 
      dplyr::mutate(Individuals= dplyr::n_distinct(individualID),
                    Specimens = dplyr::n_distinct(specimenID),
                    Assays = dplyr::n_distinct(assay),
                    Files = dplyr::n_distinct(id),
                    Tools = dplyr::n_distinct(softwareName)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(studyName, studyLeads, studyStatus, dataStatus, diseaseFocus, Individuals, Specimens, Assays, Files, Tools) %>% 
      dplyr::distinct()
  })
  
  ##start making outputs
  output$funding_agency <- shiny::renderText({
    print(glue::glue("You are now viewing studies funded by {funding_partner()}. Please select a study from the table below to view the details."))
  })
  
  output$study_table <- DT::renderDataTable({
    
    base::as.data.frame(table())
      
  }, server = TRUE, selection = 'single')
  
  
  shiny::observeEvent(input$study_table_rows_selected, {
    
    
    #Remind user of selected studyName 
    output$study <- shinydashboard::renderInfoBox({
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- table()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      shinydashboard::infoBox(
        "You have selected",
        selected_studyName,
        icon = icon("file"),
        color = "light-blue", #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
        fill = FALSE
      )
    })
    
    output$study_data <- plotly::renderPlotly({
  
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- table()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      #Make the dataframe
      data <- merged_dataset() %>%
        dplyr::filter(studyName == selected_studyName) %>% 
        dplyr::add_count(assay, name = "Assays_used") %>% 
        dplyr::add_count(resourceType, name = "Resource_added") %>% 
        dplyr::add_count(species, name = "Species_used") %>% 
        dplyr::add_count(tumorType, name = "TumorTypes_investigated") %>% 
        dplyr::select(studyName, assay, Assays_used, resourceType, Resource_added, species, Species_used, tumorType, TumorTypes_investigated) 
      
      assay_df <- data %>% 
        dplyr::select(studyName, assay, Assays_used) %>% 
        base::unique() %>% 
        tidyr::drop_na()
      
      resource_df <- data %>% 
        dplyr::select(studyName, resourceType, Resource_added) %>% 
        base::unique() %>% 
        tidyr::drop_na()
      
      species_df <- data %>% 
        dplyr::select(studyName, species, Species_used) %>% 
        base::unique() %>% 
        tidyr::drop_na()
      
      tumortype_df <- data %>% 
        dplyr::select(studyName, tumorType, TumorTypes_investigated) %>% 
        base::unique() %>% 
        tidyr::drop_na()
      
      #Catch errors where no files are present
      validate(need(length(data$resourceType) > 0 , 
                    "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
      
      #Plot the results
      p1 <- ggplot(assay_df, aes(x=studyName, y=Assays_used, fill=assay, color=assay)) +
        geom_bar(stat= "identity", alpha=0.8, position="stack") +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded", x = "Assays Used") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="none",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) 
      
      p2 <- ggplot(resource_df, aes(x=studyName, y=Resource_added, fill=resourceType, color=resourceType)) +
        geom_bar(stat= "identity", alpha=0.8, position="stack") +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded", x = "Resources Added") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="none",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) 
      
      p3 <- ggplot(species_df, aes(x=studyName, y=Species_used, fill=species, color=species)) +
        geom_bar(stat= "identity", alpha=0.8, position="stack") +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded", x = "Species Used") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="none",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) 
      
      p4 <- ggplot(tumortype_df, aes(x=studyName, y=TumorTypes_investigated, fill=tumorType, color=tumorType)) +
        geom_bar(stat= "identity", alpha=0.8, position="stack") +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded", x = "TumorTypes Investigated") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="none",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) 
      
      plotly::subplot(p1, p2, p3, p4, titleX = TRUE)
      
    })
    
    
    output$study_timeline <- plotly::renderPlotly({
      
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- table()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      #Selected Study
      data <- merged_dataset() %>%
        base::as.data.frame() %>% 
        dplyr::filter(studyName == selected_studyName)
      
      #Catch errors where no files are present
      validate(need(length(data$resourceType) > 0 , 
                    "The investigator/investigators has/have not uploaded any files yet. Please check back later."))
      
      #Plot the results
      ggplot(data, aes(x=studyName, fill=resourceType, color=resourceType)) +
        geom_bar(stat= "count", alpha=0.8, position="stack") +
        #coord_flip() +
        viridis::scale_color_viridis(discrete=TRUE) +
        viridis::scale_fill_viridis(discrete=TRUE) +
        labs(title="", y = "Number of files uploaded") +
        #ylim(0, 5) +
        theme_bw() +
        theme(legend.text = element_blank(), #element_text(size=8),
              axis.text.x  = element_blank(), #, angle = 45),
              axis.text.y = element_text(size=10),
              text = element_text(size=10),
              strip.text.x = element_text(size = 10),
              legend.position="left",
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "grey95")) +
        facet_grid(. ~year+month, scales="fixed", labeller = label_value)
    
      })
    
    output$study_details <- shiny::renderText({
      
      #Extract the index of the row that was clicked by user
      selected_study <- input$study_table_rows_selected
      
      #Extract the studyName in the clicked row
      selected_data <- table()[selected_study, ]
      selected_studyName <- selected_data$studyName
      
      center_data_df <- merged_dataset() %>%
        base::as.data.frame() %>% 
        dplyr::filter(studyName == selected_studyName) %>%
        dplyr::select(projectId, studyStatus, dataStatus, summary, diseaseFocus) %>%
        base::unique() %>% 
        tidyr::gather(field, val) %>%
        dplyr::mutate(field = stringr::str_to_title(field),
                      field = stringr::str_c("<b>", field, "</b>")
        ) 
      
      knitr::kable(center_data_df, "html", escape = FALSE, col.names = NULL,
                   align = c('r', 'l')) %>%
        kableExtra::kable_styling("striped", full_width = T)
    })
      
  })

  
}

  
  


## To be copied in the UI
# mod_summary_snapshot_ui("study_summary_ui")

## To be copied in the server
# callModule(mod_study_summary_server, "study_summary_ui")

