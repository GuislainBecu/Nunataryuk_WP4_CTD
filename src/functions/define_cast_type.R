# Function that gets the cast type (i.e. upward or downward) 
define_cast_type <- function(df){
  df <- df %>% 
    filter(flag == "TRUE")
  mean.start <- mean(df$Pres_dbar[1:5], na.rm = T)
  mean.end   <- mean(df$Pres_dbar[-5:-1], na.rm = T)
  if (mean.start < mean.end){
    df$cast.type <- "D"
  } else {
    cast.type <- "U"
  }
}