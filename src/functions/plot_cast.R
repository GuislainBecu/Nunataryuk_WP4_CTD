# Function that create a 4 plots set per file (on 1 line)
plot_cast <- function(pID, df_res, df_smt, df_fls) {
  cat(paste0("file #", pID, "/218", "\n"))
  # create a second df that will be filtered on flags
  # and tidyed up on the Temp and Sal , key = "type", value = "values"
  df_res2 <- df_res %>%
    #filter(Sal != 0.0) %>% 
    select(- Time_ms, - flag) %>% 
    rename(Temp.raw = Temp) %>% 
    as_tibble() %>% 
    gather(key = Temp.type, value = Temp.value, Temp.raw, Temp.med) %>% 
    rename(Sal.raw = Sal) %>% 
    gather(key = Sal.type, value = Sal.value, Sal.raw, Sal.med)
  #retrieve the xlim for Temp and Sal from df_res
  Tmin <- as.numeric(unique(df_res$Temp_min))
  Tmax <- as.numeric(unique(df_res$Temp_max))
  Smin <- as.numeric(unique(df_res$Sal_min))
  Smax <- as.numeric(unique(df_res$Sal_max))
  Tlim <- c(Tmin, Tmax)
  Slim <- c(Smin, Smax)
  if (!is.null(df_fls)) {
    p21 <- ggplot(data = df_res,
                  mapping = aes(x = Time_ms / 1000.00,
                                y = Pres_dbar,
                                colour = flag)) +
      geom_point(size = 1.75) +
      geom_point(data = df_fls,
                 aes(x = Time_ms / 1000.00,
                     y = Pres_dbar,
                     colour = flag),
                 size = 0.25,
                 inherit.aes = F) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      ylab("Depth [m]") +
      xlab("Time since start of profile [s]") +
      scale_colour_manual(name = "",
                          values = c("FALSE" = "grey", "TRUE" = "black"),
                          labels = c("FALSE" = "discarded", "TRUE" = "kept"))
  } else {
    p21 <- ggplot(data = df_res,
                  mapping = aes(x = Time_ms / 1000.00,
                                y = Pres_dbar,
                                colour = flag)) +
      geom_point(size = 1) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      ylab("Depth [m]") +
      xlab("Time since start of profile [s]") +
      scale_colour_manual(name = "",
                          values = c("TRUE" = "black"),
                          labels = c("TRUE" = "kept"))
  }
  
  if (!is.null(df_fls)) {
    p22 <- ggplot(data = df_fls,
                  mapping = aes(x = Temp,
                                y = Pres_dbar,
                                colour = "grey")) +
      geom_point(size = 0.3) +
      geom_point(data = df_res2 %>% filter(Temp.type == "Temp.raw"),
                 mapping = aes(x = Temp.value,
                               y = Pres_dbar,
                               colour = Temp.type),
                 size = 4) +
      geom_point(data = df_res2 %>% filter(Temp.type == "Temp.med"),
                 mapping = aes(x = Temp.value,
                               y = Pres_dbar,
                               colour = Temp.type),
                 size = 2) +
      geom_point(data = df_smt,
                 mapping = aes(x = Temp.smt,
                               y = depth.grid,
                               colour = "red"),
                 inherit.aes = F,
                 size = 0.75) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      xlab("Temp[C]") +
      ylab("Depth [m]") +
      coord_cartesian(xlim = Tlim) +
      scale_colour_manual(name = "",
                          breaks = c("Temp.raw",
                            "Temp.med",
                            "red",
                            "grey"),
                          values = c("Temp.raw" = "black",
                                     "Temp.med" = "purple",
                                     "red" = "orange",
                                     "grey" = "grey"),
                          labels = c("Temp.raw" = "raw data",
                                     "Temp.med" = "running median",
                                     "red" = "smoothed",
                                     "grey" = "discarded"),
                          guide = guide_legend(override.aes = list(size = c(4, 2, 0.3, 0.75))))
  } else {
    p22 <- ggplot(data = df_res2 %>% filter(Temp.type == "Temp.raw"),
                  mapping = aes(x = Temp.value,
                                y = Pres_dbar,
                                colour = Temp.type),
                  size = 4) +
      geom_point() +
      geom_point(data = df_res2 %>% filter(Temp.type == "Temp.med"),
                 mapping = aes(x = Temp.value,
                               y = Pres_dbar,
                               colour = Temp.type),
                 size = 2) +
      geom_point(data = df_smt,
                 mapping = aes(x = Temp.smt,
                               y = depth.grid,
                               colour = "red"),
                 inherit.aes = F,
                 size = 0.75) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      xlab("Temp[C]") +
      ylab("Depth [m]") +
      coord_cartesian(xlim = Tlim) +
      scale_colour_manual(name = "",
                          breaks = c("Temp.raw",
                                     "Temp.med",
                                     "red"),
                          values = c("Temp.raw" = "black",
                                     "Temp.med" = "purple",
                                     "red" = "orange"),
                          labels = c("Temp.raw" = "raw data",
                                     "Temp.med" = "running median",
                                     "red" = "smoothed"),
                          guide = guide_legend(override.aes = list(size = c(4, 2, 0.75))))
  }
  
  if (!is.null(df_fls)) {
    p23 <- ggplot(data = df_fls,
                  mapping = aes(x = Sal,
                                y = Pres_dbar,
                                colour = "grey")) +
      geom_point(size = 0.3) +
      geom_point(data = df_res2 %>% filter(Sal.type == "Sal.raw"),
                 mapping = aes(x = Sal.value,
                               y = Pres_dbar,
                               colour = Sal.type),
                 size = 4) +
      geom_point(data = df_res2 %>% filter(Sal.type == "Sal.med"),
                 mapping = aes(x = Sal.value,
                               y = Pres_dbar,
                               colour = Sal.type),
                 size = 2) +
      geom_point(data = df_smt,
                 mapping = aes(x = Sal.smt,
                               y = depth.grid,
                               colour = "red"),
                 inherit.aes = F,
                 size = 0.75) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      xlab("Sal[psu]") +
      ylab("Depth [m]") +
      coord_cartesian(xlim = Slim) +
      scale_colour_manual(name = "",
                          breaks = c("Sal.raw",
                                     "Sal.med",
                                     "red",
                                     "grey"),
                          values = c("Sal.raw" = "black",
                                     "Sal.med" = "lightblue",
                                     "red" = "red",
                                     "grey" = "grey"),
                          labels = c("Sal.raw" = "raw data",
                                     "Sal.med" = "running median",
                                     "red" = "smoothed",
                                     "grey" = "discarded"),
                          guide = guide_legend(override.aes = list(size = c(4, 2, 0.3, 0.75))))
  } else {
    p23 <- ggplot(data =df_res2 %>% filter(Sal.type == "Sal.raw"),
                  mapping = aes(x = Sal.value,
                                y = Pres_dbar,
                                colour = Sal.type),
                  size = 4) +
      geom_point() +
      geom_point(data = df_res2 %>% filter(Sal.type == "Sal.med"),
                 mapping = aes(x = Sal.value,
                               y = Pres_dbar,
                               colour = Sal.type),
                 size = 2) +
      geom_point(data = df_smt,
                 mapping = aes(x = Sal.smt,
                               y = depth.grid,
                               colour = "red"),
                 inherit.aes = F,
                 size = 0.75) +
      theme_bw() +
      theme_bw(base_size = 7) +
      scale_y_reverse() +
      xlab("Sal[psu]") +
      ylab("Depth [m]") +
      coord_cartesian(xlim = Slim) +
      scale_colour_manual(name = "",
                          breaks = c("Sal.raw",
                                     "Sal.med",
                                     "red"),
                          values = c("Sal.raw" = "black",
                                     "Sal.med" = "lightblue",
                                     "red" = "red"),
                          labels = c("Sal.raw" = "raw data",
                                     "Sal.med" = "running median",
                                     "red" = "smoothed"),
                          guide = guide_legend(override.aes = list(size = c(4, 2, 0.75))))
  }
  
  p24 <- ggplot(data = df_smt,
                mapping = aes(x = Sal.smt, y = Temp.smt, colour = "red")) +
    geom_point(size = 1) +
    geom_point(data = df_res2 %>%
                 filter(Temp.type == "Temp.raw") %>% 
                 filter(Sal.type == "Sal.raw"),
               mapping = aes(x = Sal.value, y = Temp.value, colour = "black"),
               size = 1) +
    xlab("Sal [psu]") +
    ylab("Temp [C]") +
    theme_bw() +
    theme_bw(base_size = 7) +
    coord_cartesian(xlim = Slim, ylim = Tlim) +
    scale_colour_manual(name = "",
                        values = c("black" = alpha("black", 0.3), "red" = "red"),
                        labels = c("red" = "smoothed", "black" = "kept raw data"))
  
  if (unique(df_res2$flag_QC)) {
    title <- ggdraw() +
    draw_label(paste(pID, ": ", df_res$filename, sep = ""),
               fontface = 'bold',colour = 'black',
               x = 0.5,
               hjust = 0.5)
  } else {
    title <- ggdraw() +
      draw_label(paste(pID, ": ", df_res$filename, sep = ""),
                 fontface = 'bold', colour = 'red',
                 x = 0.5,
                 hjust = 0.5)
  }
  p00 <- cowplot::plot_grid(p21, p22, p23, p24, ncol = 4) # get the 4 plots in 1 row
  p01 <- cowplot::plot_grid(title, p00, nrow = 2, rel_heights = c(0.1, 1)) # add a title to these 4 plots
}