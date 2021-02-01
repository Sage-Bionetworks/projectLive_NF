#' Create Plot With Param List
#' This function calla ploting function using a parameter list, and a tibble
#'
#' @param data A tibble
#' @param param_list A named list. The list must have the fields "plot" and 
#' tooltips". The "plot" field must be named list of the arguemnts of the 
#' plot_func. The "tooltips" field must be a list of strings that are either
#' names of columns in the data, or names of aesthetics in the plot_func
#' @param plot_func A string that is the name of a plot function
#' @param ... Arguments to plotly::ggplotly
#' @importFrom magrittr %>% 
#' @importFrom rlang !!!
#' @examples
#' 
#' data <- dplyr::tibble(
#'  "Study Leads" = c("s1", "s2", "s3"),
#'  "Resource Type" = c("r1", "r2", "r3"),
#'  "Year" = c(2000L, 2001L, 2002L),
#'  "Month" = factor("Jul", "Jul", "Jun"),
#'  "Count" = c(10, 30, 40)
#' )
#' param_list <- list(
#'   "plot" = list(
#'     "x" = "Study Leads",
#'     "y" = "Count",
#'     "fill" = "Resource Type",
#'     "facet" = list("Year", "Month")
#'   ),
#'   "tooltips" = list("count", "fill")
#' )
#' create_plot_with_param_list(
#' data, param_list, "create_file_upload_timeline_plot"
# ')
create_plot_with_param_list <- function(data, param_list, plot_func, ...){
  fig <-
    rlang::exec(plot_func, !!!param_list$plot, data = data) %>%
    plotly::ggplotly(
      tooltip = c(param_list$tooltips),
      ...
    )
}

#' Create Initiative Activity Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#' @param facet A list of string that are names of columns in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! !!
create_initiative_activity_plot <- function(data, x, fill, facet){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "count",
      position = "stack",
      alpha = 0.8, 
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(
      title = "", 
      y = "Number of files"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 8),
      axis.text.x  = ggplot2::element_blank(), 
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")) +
    ggplot2::facet_grid(cols = ggplot2::vars(!!!rlang::syms(unlist(facet))))
}

#' Create Resources Generated Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#' @param facet A list of string that are names of columns in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! !!
create_resources_generated_plot <- function(data, x, fill, facet){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "count",
      position = "stack",
      alpha = 1.0,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(
      title = "", 
      y = "Number of files per resource"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 8),
      axis.text.x  = ggplot2::element_blank(), 
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")) +
    ggplot2::facet_grid(cols = ggplot2::vars(!!!rlang::syms(unlist(facet))))
}

#' Create Publication Status Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!
create_publication_status_plot <- function(data, x, fill){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      alpha = 0.8,
      position = "stack"
    ) +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(title = "", y = "Number of publications") +
    ggplot2::ylim(0, 10) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(), 
      axis.text.x  = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")
    ) 
}

#' Create Publication Disease Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!
create_publication_disease_plot <- function(data, x, fill){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ), 
      alpha = 0.8,
      position = "stack"
    ) +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(title = "", y = "Number of publications") +
    ggplot2::ylim(0, 10) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(), 
      axis.text.x  = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")
    ) 
}

#' Create File Upload Timeline Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param y A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#' @param facet A list of string that are names of columns in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! !!
create_file_upload_timeline_plot <- function(data, x, y, fill, facet){
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "identity",
      alpha = 0.8, 
      position = "stack"
    ) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(title = "", y = "Number of files uploaded") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(size = 10, angle = 45),
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(!!!rlang::syms(unlist(facet))),
      scales = "free"
    )
}

#' Create Annotation Activity Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param y A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#' @param facet A list of string that are names of columns in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! !!
create_annotation_activity_plot <- function(data, x, y, fill, facet){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "identity",
      alpha = 0.8, 
      position = "stack"
    ) +
    ggplot2::coord_flip() +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(title = "", y = "Number of experimental data files") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(size = 10, angle = 90),
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(!!!rlang::syms(unlist(facet))),
      scales = "fixed"
    )
}

#' Create Data Focus Plots
#'
#' @param data_list A list of tibbles
#' @param param_list A named list that has the "plot" name. The "plot" value
#' must be a named list with the "x" name, which is a name in each tibble in the
#' data list, and a "fill" name with is a list of columns, ewual to the length
#' of the data list.
#'
#' @return
#' @export
#'
#' @examples
#' data_list <- list(
#'  "Assays" = dplyr::tribble(
#'   ~Study, ~Assays,
#'   "s1",   "a1",    
#'   "s1",   "a2"    
#' ),
#'  "Resources" = dplyr::tribble(
#'   ~Study, ~Resources,
#'   "s1",   "r1",       
#'   "s1",   "r1"
#'  )
#' )
#' 
#' param_list <- list(
#'   "plot" = list(
#'     "x" = "Study",
#'     "fill" = list(
#'       "Assay",
#'       "Resources"
#'     )
#'   )
#' )
# create_data_focus_plots(data_list, param_list)
create_data_focus_plots <- function(data_list, param_list){
  data_list %>% 
    purrr::imap(
      ~ create_data_focus_plot(
        data = .x, 
        x = param_list$plot$x, 
        fill = .y
      )
    ) %>% 
    purrr::map(plotly::ggplotly, tooltip = param_list$tooltips) %>% 
    plotly::subplot(titleX = TRUE)
}

#' Create Data Focus Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!
create_data_focus_plot <- function(data, x, fill){
  data %>%  
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "count", 
      alpha = 0.8, 
      position = "stack"
    ) +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(
      title = "", y = "Number of files uploaded", x = fill
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(), 
      axis.text.x  = ggplot2::element_blank(), 
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")
    ) 
}

#' Create Study Timeline Plot
#'
#' @param data A Tibble
#' @param x A string that is the name of a column in the data
#' @param fill A string that is the name of a column in the data
#' @param facet A list of string that are names of columns in the data
#'
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! !!
create_study_timeline_plot <- function(data, x, fill, facet){
  data %>%  
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      stat = "count",
      alpha = 0.8,
      position = "stack"
    ) +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(title = "", y = "Number of files uploaded") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(), 
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(), 
      axis.text.y = ggplot2::element_text(size = 10),
      text = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.position = "left",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(!!!rlang::syms(unlist(facet))))
}
