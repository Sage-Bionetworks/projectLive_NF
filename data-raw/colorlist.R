## Make colors for plots
library(RColorBrewer)
library(wesanderson)
#fabcolors = RColorBrewer::brewer.pal(n = 11,name = 'RdGy')
col2 = RColorBrewer::brewer.pal(n = 10,name = 'PRGn')
col1 = RColorBrewer::brewer.pal(n = 10,name = 'Spectral')
col3 = RColorBrewer::brewer.pal(n = 10,name = 'BrBG')
col4 = RColorBrewer::brewer.pal(n = 10,name = 'PiYG')
col5 = RColorBrewer::brewer.pal(n = 10,name = 'PuOr')
col6 = RColorBrewer::brewer.pal(n = 10,name = 'RdBu')

allcolors <- c(col1,col2,col3, col4, col5, col6)
allcolors <- list(allcolors)

morecolors1 <- wes_palette("Darjeeling1", n=4, type = "discrete")
morecolors1 <- list(morecolors1)

morecolors2 <- wes_palette("Moonrise2", n=3, type = "discrete")
morecolors2 <- list(morecolors2)

cooler <- list(c("#413144", "#B185A7", "#D37D72", "#A9A090", "#FFCE9E", 
                 "#f6511d", "#ffb400", "#00a6ed", "#7fb800", "#0d2c54",
                 "#6da34d", "#56445d", "#548687", "#8fbc94", "#c5e99b",
                 "#d36135", "#7fb069", "#ece4b7", "#e6aa68", "#02020b",
                 "#ca054d", "#3b1c32", "#a4d4b4", "#ffcf9c", "#b96d40",
                 "#adbca5", "#e8b9ab", "#e09891", "#cb769e", "#8c5f66",
                 "#795c5f", "#a69658", "#d9b26f", "#fadf7f", "#f2e29f"))

color_list <- c(allcolors, morecolors1, morecolors2, cooler)

usethis::use_data(color_list, overwrite = TRUE)