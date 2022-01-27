# Function that performs a median computation on a running window
# myrange is the span of the running window
# do that for the Temp and for the Sal values
my_runmed <- function(df, filename, myrange) {
  myrange = unique(df$median_range)
  if (sum(!is.na(df$Temp)) > length(df$Temp)/10) { # i.e. we need at least 10% of the profile being not NA
    ok <- which(!is.na(df$Temp) & !is.na(df$Sal))
    res <- data.frame(Temp.med  = runmed(x = df$Temp[ok], k = myrange, endrule = "median"),
                      Sal.med   = runmed(x = df$Sal[ok], k = myrange, endrule = "median"))
  } else {
    res <- data.frame(Temp.med = rep(NA, length(df$Pres_dbar)),
                      Sal.med = rep(NA, length(df$Pres_dbar)))
  }
}
