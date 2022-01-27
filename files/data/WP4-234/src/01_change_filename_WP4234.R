rm(list = ls())
library(fs)
library(data.table)
library(tidyverse)
library(janitor)
df <- fread("./filenames_change_log.csv") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(new_filename = paste0("../03_new_filenames_cleaned/", new_filename)) %>% 
  mutate(original_filename = paste0("../02_original_filenames_parsed_and_cleaned/", original_filename)) %>% 
  mutate(ID = row_number()) %>% 
  group_by(ID) %>% 
  #map(.x = ., .f = function(df_tmp) file_copy(path = df_tmp$original_filename, new_path = df_tmp$new_filename, overwrite = T))
  nest() %>% 
  mutate(res = map(data, function(df_tmp) file_copy(path = df_tmp$original_filename, new_path = df_tmp$new_filename, overwrite = T)))
