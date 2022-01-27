# Function that performs a loess smoothing on a defined depth grid
# do that for Temp and Sal
# we define the grid step to 2 cm
#loess_smoothing <- function(df, filename, myspan) {
smoothing <- function(df) {
  df <- df %>% 
    filter(flag == "TRUE") #%>% 
    #filter(Sal != 0.000)
  myspan <- unique(df$loess_span)
  fct <- unique(df$fct)
  depth.grid <- seq(floor(100 * min(df$Pres_dbar, na.rm = TRUE)) / 100.00,
                    ceiling(100 * max(df$Pres_dbar, na.rm = TRUE)) / 100.00,
                    by = 0.01)
  if (!all(is.na(df$Temp) & is.na(df$Sal))) {
    if (fct == "loess") {
      my_smtT <- loess(df$Temp.med ~ df$Pres_dbar, span = myspan)
      my_smtS <- loess(df$Sal.med ~ df$Pres_dbar, span = myspan)
    } else if (fct == "spline") {
      my_smtT <- spline(y = df$Temp.med, x = df$Pres_dbar, xout = depth.grid, method = "natural")
      my_smtS <- spline(y = df$Sal.med, x = df$Pres_dbar, xout = depth.grid, method = "natural")
    } else if (fct == "median") {
      my_smtT <- median(df$Temp.med, na.rm = T)
      my_smtS <- median(df$Sal.med, na.rm = T)
    }
    if (fct == "loess") {
      Temp.smt <- predict(my_smtT, depth.grid)
      Sal.smt  <- predict(my_smtS, depth.grid)
    } else if (fct == "spline") {
      Temp.smt <- my_smtT$y
      Sal.smt  <- my_smtS$y
    } else if (fct == "median"){
      Temp.smt <- rep(my_smtT, length(depth.grid))
      Sal.smt  <- rep(my_smtS, length(depth.grid))
    }
    res <- data.frame(profile_ID = rep(unique(df$profile_ID), length(depth.grid)),
                      depth.grid,
                      Temp.smt,
                      Sal.smt)
  } else {
    res <- data.frame(profile_ID = rep(unique(df$profile_ID), length(depth.grid)),
                      depth.grid,
                      Temp.smt = rep(NA, length(depth.grid)),
                      Sal.smt = rep(NA, length(depth.grid)))
  }
}