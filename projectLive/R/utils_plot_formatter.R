##Initial code shamelessly borrowed from the iAtlas Portal
##https://github.com/CRI-iAtlas/shiny-iatlas/blob/09da641f37e7acdc149853a6b818775328054189/functions/format.R


remove_legend <- function(p) {
  p %>% 
    plotly::layout(
      showlegend=F
      )
}


get_top_margin <- function(p) {
  t <- 0
  if ("layoutAttrs" %in% names(p$x)) {
    p_layout_data <- p$x$layoutAttrs[[length(p$x$layoutAttrs)]]
    if (!is.null(p_layout_data$title)) {
      t <- p_layout_data$font$size * 2 %>% 
        ceiling()
    }
  }
  t
}


get_margins_plotly <- function(p, font_size = 12) {
  
  xlabbuffer <- 0
  ylabbuffer <- 0
  if ("layoutAttrs" %in% names(p$x)) {
    p_layout_data <- p$x$layoutAttrs[[1]]
    if (length(p_layout_data$xaxis$title) > 0) {
      xlabbuffer <- font_size * 3  %>% 
        ceiling()
    }
    if (length(p_layout_data$yaxis$title) > 0) {
      ylabbuffer <- font_size * 3  %>% 
        ceiling()
    }
  }
  
  p_data <- p$x$attrs[[length(p$x$attrs)]]
  
  xlabangle <- 0
  xlabmax <- 1
  if (class(p_data$x) == "character") {
    xlabs <- p_data$x
    xlabangle <-  purrr::map(
      p$x$layoutAttrs, 
      ~ .[["xaxis"]][["tickangle"]]
    ) %>% 
      purrr::discard(is.null) %>% 
      unlist()
    xlabmax <- xlabs %>% 
      purrr::map_int(stringr::str_length) %>% 
      max(na.rm = TRUE)
  }
  xmultiplier <- abs(sin(xlabangle * pi/180))
  
  
  if (p_data$type %in% c("heatmap")) {
    ylabs <- p_data$y
    ylabmax <- ylabs %>% 
      purrr::map_int(stringr::str_length) %>% 
      max(na.rm = TRUE)
  } else {
    ylabmax <- 1
  }
  
  list(
    b = xlabbuffer %>% 
      magrittr::add(
        max(font_size - 1, (font_size - 1) * (xlabmax + 1 * xmultiplier) - xlabmax)
      ) %>% 
      ceiling(),
    l = ylabbuffer %>% 
      magrittr::add(
        max(font_size - 3, (font_size - 3) * ylabmax - ylabmax)
      ) %>% 
      ceiling(),
    t = get_top_margin(p)
  )
}


get_margins <- function(p, font_size = 12) {
  if (!("xaxis" %in% names(p$x$layout))) {
    return(get_margins_plotly(p, font_size))
  }
  if (stringr::str_length(p$x$layout$xaxis$title) > 0) {
    xlabbuffer <- (p$x$layout$xaxis$titlefont$size - 6) * 3  %>% 
      ceiling()
  } else {
    xlabbuffer <- 0
  }
  
  xlabs <- p$x$layout$xaxis$categoryarray
  xlabangle <- p$x$layout$xaxis$tickangle
  xlabmax <- xlabs %>% 
    purrr::map_int(stringr::str_length) %>% 
    max(na.rm = TRUE)
  xlabfontsize <- dplyr::if_else(
    !is.null(font_size), 
    min(font_size, p$x$layout$xaxis$tickfont$size),
    p$x$layout$xaxis$tickfont$size
  ) %>% 
    ceiling()
  xmultiplier <- abs(sin(xlabangle * pi/180))
  
  if (stringr::str_length(p$x$layout$yaxis$title) > 0) {
    ylabbuffer <- (p$x$layout$yaxis$titlefont$size - 6) * 3  %>% 
      ceiling()
  } else {
    ylabbuffer <- 0
  }
  
  ylabs <- p$x$layout$yaxis$categoryarray
  
  ylabangle <- p$x$layout$yaxis$tickangle
  ylabmax <- ylabs %>% 
    purrr::map_int(stringr::str_length) %>% 
    max(na.rm = TRUE)
  ylabfontsize <- dplyr::if_else(
    !is.null(font_size), 
    min(font_size, p$x$layout$yaxis$tickfont$size),
    p$x$layout$yaxis$tickfont$size
  ) %>% 
    ceiling()
  ymultiplier <- abs(cos(ylabangle * pi/180))
  list(
    b = xlabbuffer %>% 
      magrittr::add(
        max(xlabfontsize, (xlabfontsize - 2) * (xlabmax + 1 * xmultiplier))
      ) %>% 
      ceiling(),
    l = ylabbuffer %>% 
      magrittr::add(
        max(ylabfontsize, ylabfontsize * (ylabmax * ymultiplier))
      ) %>% 
      ceiling(),
    t = get_top_margin(p)
  )
}

format_plotly <- function(p) {
  font_size <- 13
  p %>% 
    plotly::layout(
      font = list(
        family = "Roboto, Open Sans, sans-serif",
        size = font_size),
      margin = get_margins(p, font_size)
    ) %>% 
    plotly::config(displayModeBar = T)
  

}