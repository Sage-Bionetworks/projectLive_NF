##Initial code shamelessly borrowed from the iAtlas Portal
#https://github.com/CRI-iAtlas/shiny-iatlas/blob/09da641f37e7acdc149853a6b818775328054189/functions/heatmap.R

create_heatmap <- function(corr_mat, source_name, scale_colors = F){
  zmin <- NULL
  zmax <- NULL
  if(scale_colors){
    extreme <- max(abs(min(corr_mat)),
                   abs(max(corr_mat)))
    zmax <- extreme
    zmin <- -extreme
  }
  
  p <- 
    plotly::plot_ly(
      z = corr_mat,
      x = colnames(corr_mat),
      y = rownames(corr_mat),
      type = "heatmap",
      source = source_name,
      colors = rev(RColorBrewer::brewer.pal(8, "RdBu")),
      zmin = zmin,
      zmax = zmax
    ) %>% 
    plotly::layout(xaxis = list(tickangle = 90)) %>% 
    format_plotly() %>%
    I
}