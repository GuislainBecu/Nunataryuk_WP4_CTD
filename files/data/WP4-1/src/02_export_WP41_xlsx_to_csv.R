
#################################################################################
###                                                                           ###
###  Program that reads the RBR CTD files from the WP4 first cruise           ###
###  as they were acquired differently, and especially with the RBR           ###
###  proprietary file format and then converted into Excel format including   ###
###  several tabs. The data are stored in the 4th tab called "Data".          ###
###                                                                           ###
###  Guislain.Becu@takuvik.ulaval.ca - December 2019                          ###
###                                                                           ###
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--


# CLEAN ALL THE ENVIRONMENT -----------------------------------------------
rm(list = ls())


# LOAD THE LIBRARIES ------------------------------------------------------
library(tidyverse)
library(readxl)


# DEFINE THE FUNCTIONS ----------------------------------------------------
# Functions that will read the XL workbook, and especially the "Data" tab
readCTDfile <- function(myfile) {
  mypath <- "../03_new_filenames_xlsx/"
  fout <-  paste0("../04_new_filenames_csv_not_cleaned/", strsplit(myfile, ".xlsx")[[1]], ".csv")
  df <- readxl::read_excel(paste0(mypath, myfile),
                           skip = 1,
                           sheet = "Data",
                           col_names = T) %>%
    janitor::clean_names() %>% 
    select(1:3, 5:7, 9, 10, 14) %>% 
    rename(Pres_dbar = depth, # just to be consistent with the other data, but it is a depth!
           DateTime = time,
           Cond = conductivity,
           Temp = temperature,
           Sal = salinity,
           Chla = chlorophyll_a,
           pH = p_h,
           DOsat = dissolved_o2_saturation,
           DOconc = dissolved_o2_concentration) %>% 
    mutate(Time_ms = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%0S4")) %>% 
    mutate(Time_ms = round((Time_ms - Time_ms[1]) * 1000.0)) %>% 
    select(DateTime, Time_ms, Pres_dbar, Temp, Sal, Cond, Chla, pH, DOconc, DOsat) %>% 
    write.table(x = .,
              file = paste0("../04_new_filenames_csv_not_cleaned/", strsplit(myfile, ".xlsx")[[1]], ".csv"),
              row.names = F,
              sep = "\t")
  return(df)
}

# LIST ALL THE XL FILES ---------------------------------------------------
mypath <- "../03_new_filenames_xlsx/"
files.in <- list.files(path = mypath, pattern = "*.xlsx")


# READ THEIR CONTENT AND OUTPUT IN A TXT FILE -----------------------------
# case where we output each file in a txt file
df.in <- lapply(X = files.in,
                FUN = function(CTDfile) readCTDfile(CTDfile))
