create_plot_with_param_list <- function(data, param_list, plot_func){
  fig <-
    rlang::exec(plot_func, !!!param_list$plot, data = data) %>%
    plotly::ggplotly(tooltip = c(param_list$tooltips))
}

create_consortium_activity_plot <- function(data, x, fill, facet){
  
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
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")) +
    ggplot2::facet_grid(cols = ggplot2::vars(!!!rlang::syms(unlist(facet))))
}



create_publication_status_plot <- function(data, x, fill){

  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      binwidth = 0.5,  
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

create_publication_disease_plot <- function(data, x, fill){
  
  data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = !!rlang::sym(x),
        fill = !!rlang::sym(fill),
        color = !!rlang::sym(fill)
      ),
      binwidth = 0.5,  
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
      axis.text.x  = ggplot2::element_text(size = 10),
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
