# Function that plot all the casts in 1 catalogue
plot_all <- function(df){
  cowplot::plot_grid(plotlist = df$p, ncol = 1, label_size = 4, scale = 0.95)
}