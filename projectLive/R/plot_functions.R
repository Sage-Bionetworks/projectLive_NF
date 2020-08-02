
create_study_summary_plot <- function(data, x, y, fill, color){
  
  data %>%  
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!rlang::ensym(x),
        y = !!rlang::ensym(y),
        fill = !!rlang::ensym(fill),
        color = !!rlang::ensym(color),
      ),
      stat = "identity", 
      alpha = 0.8, 
      position = "stack"
    ) +
    viridis::scale_color_viridis(discrete = TRUE) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(
      title = "", y = "Number of files uploaded", x = y
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

create_study_summary_grid_plot <- function(data, x, fill, color, ...){
  
  data %>%  
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{x}}, fill = {{fill}}, color = {{color}}),
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
      legend.position="left",
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey95")
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(...))
}