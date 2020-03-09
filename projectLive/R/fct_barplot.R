##Initial code shamelessly borrowed from the iAtlas Portal
#https://github.com/CRI-iAtlas/shiny-iatlas/blob/09da641f37e7acdc149853a6b818775328054189/functions/barplot.R

create_barplot <- function(
  df, 
  x_col = "x",
  y_col = "y",
  error_col = NA, 
  key_col = NA,
  color_col = NA, 
  label_col = NA,
  xlab = "",
  ylab = "", 
  title = "", 
  source_name = NULL, 
  bar_colors = NULL,
  sort = FALSE) {
  
  if(is.na(key_col)) key_col <- x_col
  if(is.na(color_col)) color_col <- x_col
  if(is.na(label_col)) label_col <- x_col
  if(is.na(error_col)){ 
    df <- df %>% dplyr::mutate(error = 0)
    error_col <- "error"
  }
  
  if(sort == "desc"){
    df <- df %>% 
      mutate(!!x_col := forcats::fct_reorder(!!sym(x_col), !!sym(y_col), .desc = T))
  }
  
  if (is.null(bar_colors)) {
    bar_colors <- viridis::viridis_pal(option = "D")(dplyr::n_distinct(df[[color_col]]))
  }
  
  wrapr::let(
    alias = c(
      X = x_col, 
      Y = y_col, 
      KEY = key_col,
      COLOR = color_col,
      ERROR = error_col,
      LABEL = label_col),
    plot_ly(
      df,
      x = ~X,
      y = ~Y,
      color = ~COLOR,
      text = ~LABEL,
      key = ~KEY,
      type = 'bar',
      source = source_name,
      colors = bar_colors,
      error_y = list(
        array = ~ERROR, 
        color = 'black', 
        thickness = 1),
      hoverinfo = 'text'
    )) %>% 
    layout(
      legend = list(orientation = 'h', x = 0, y = 1),
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    ) %>% 
    format_plotly()
  
}

