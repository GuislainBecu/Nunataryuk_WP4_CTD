
             #################################################################################
             ###                                                                           ###
             ###  Program to process only the individual CTD files                         ###
             ###  we suppose here that the files have been cleaned with the Shiny app.,    ###
             ###  and we work from the flagged ones, not the cleaned ones, so that we can  ###
             ###  visualize which data have been discarded.                                ###
             ###                                                                           ###
             ###  v1.03 includes the depth_tare correction. Depth tares have been computed ###
             ###  from a linear regression between EC atmospheric pressure values and      ###
             ###  in-air CTD measurements. See "02_compute_corr_depth_tare.R" as well      ###
             ###  as the "../res/temporary_results/IOP_metadata_initial.csv" file that     ###
             ###  has been output from this code.                                          ###
             ###                                                                           ###
             ###  Guislain.Becu@takuvik.ulaval.ca - February 2019  (rev. Jan 2022)         ###
             ###                                                                           ###
             #--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--

             
# CLEAN ALL THE ENVIRONMENT -----------------------------------------------
rm(list = ls())
# and get current time
s1 <- Sys.time()

# set the working directory as the source file one
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD THE LIBRARIES ------------------------------------------------------
library(tidyverse)
library(cowplot)
library(gridGraphics)
library(leaflet)
library(htmlwidgets)


# SOURCE THE FUNCTIONS ----------------------------------------------------
source(file = "./functions/define_cast_type.R", verbose = F, echo = F) # upward vs. downward cast?
source(file = "./functions/my_runmed.R", verbose = F, echo = F)        # apply the running median
source(file = "./functions/smoothing.R", verbose = F, echo = F)        # apply the selected smoothing
source(file = "./functions/plot_cast.R", verbose = F, echo = F)        # plot 1 cast (several plots)
source(file = "./functions/plot_all.R", verbose = F, echo = F)         # plot all the casts
             

# READ ALL THE INPUT FILES ------------------------------------------------
# get the files path and the files list
mypath <- "../files/data/WP4-1234/"
files.in <- list.files(path = mypath, pattern = "*.csv")# [1:8] # to test the code on the first 8 files, quicker!

# read the metadata file containing the depth tare values and join it to the later df.info
df.meta <- data.table::fread(file = "../files/metadata_and_log/03_IOP_metadata_initial.csv", sep = ";", header = T) %>% 
  as_tibble() %>% 
  select(- c(flag_qc))
# read the "01_iop_info.csv" file which contains individual median ranges
# and loess span for each profile. This has been manually set and remains user tunable!
df.info <- data.table::fread(file = "../files/metadata_and_log/01_iop_info.csv", sep = ";", header = T) %>% 
  left_join(df.meta, by = "filename")

# read all the files and put them all in a single data frame
# then assign each file a profile_ID, gets the cast type (upward vs. downward)
# and get the associated spans and ranges
df.in <- lapply(X = paste0(mypath, files.in),
             FUN = data.table::fread) %>% 
  bind_rows(.id = "profile_ID") %>% 
  mutate(profile_ID = parse_number(profile_ID)) %>% 
  mutate(filename = files.in[profile_ID]) %>% 
  select(- DateTime) %>% 
  select(profile_ID, filename, everything()) %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(cast.type = map(data, define_cast_type)) %>% 
  mutate(cast.type = unlist(cast.type)) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  full_join(df.info, by = "filename")  %>% 
  group_by(filename) %>% 
  mutate(Pres_dbar = Pres_dbar - unique(tare_depth))


# CLEAN THE DATA ----------------------------------------------------------
# clean the data, i.e. Perform the running median as well as the loess smoothing
# df.res will contain the raw values as well as the median ones,
# but as the smoothing is performed on a depth grid, we push these smoothed values
# in another df, df.smt, as they don't have the same number of lines
# in df.smt, we just keep the depth.grid and the smoothed Temp and Sal values

df.res <- df.in %>%
  group_by(filename) %>% 
  filter(flag == "TRUE") %>% # flag here comes from the shiny app (depth timeseries cleanup)
  group_by(filename) %>% 
  nest() %>% 
  mutate(pred = map2(data, filename, myrange = 5, my_runmed)) %>% 
  unnest(cols = c(data, pred)) %>% 
  ungroup()

df.fls <- df.in %>% 
  filter(flag == "FALSE") %>% # keep the discarded points, for plotting
  group_by(filename) %>% 
  nest() %>% 
  mutate(pred = map2(data, filename, myrange = 35, my_runmed)) %>% 
  unnest(cols = c(data, pred)) %>% 
  ungroup()

df.smt <- df.res %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(pred = map(data, smoothing)) %>% 
  unnest(cols = c(pred)) %>% 
  select(- data) %>% 
  ungroup()


# ORGANIZE THE DF FOR PLOTTING --------------------------------------------
# create a general df containing filenames and the 2 others df, i.e.
# df.res and df.smt. As they don't match ref. nb rows, we have
# to nest them, so before that, create 2 nested versions of them
df.res.nst <- df.res %>%
  group_by(profile_ID) %>% 
  nest() %>% 
  rename(data.res = data)

df.smt.nst <- df.smt %>%
  group_by(profile_ID) %>% 
  nest() %>% 
  rename(data.smt = data)

df.fls.nst <- df.fls %>%
  group_by(profile_ID) %>% 
  nest() %>% 
  rename(data.fls = data)

# now create the global one, in which we add 1 column called "page"
# that is used to group 2 files on each of the final catalogue page
# change the modulo value if this has to change,
# e.g. %/% 6 for 6 pages per page, etc.
df.all <- full_join(df.res.nst, df.smt.nst, by = "profile_ID") %>% 
  full_join(df.fls.nst, by = "profile_ID") %>% 
  mutate(page = (profile_ID - 1) %/% 2) %>% 
  mutate(p = pmap(.l = list(profile_ID, data.res, data.smt, data.fls), .f = plot_cast)) %>% 
  group_by(page) %>% 
  nest()


# PLOTTING A CATALOG ------------------------------------------------------
# the catalog displays all the data from each profile, even the discarded
# ones (the discarded ones have a red title)
pdf("../res/temporary_results/catalog/Nunataryuk_WP4_CTD_catalog.pdf", width = 20, height = 10)
# 1 line version, longer somehow!? put the "cowplot" command in a function....
# map(df.all$data, ~cowplot::plot_grid(plotlist = .$p, ncol = 1))
map(df.all$data, plot_all)
dev.off()


# OUTPUT INDIVIDUAL ASCII FILES -------------------------------------------
# output only smoothed T and S value on the depth grid where they are defined
# ATTENTION, output filenames have the same as the input filenames!
# ATTENTION, these are not the final deliverables, only temporary/working ones!
df.output <- df.smt %>% 
  drop_na() %>% 
  group_by(profile_ID) %>% 
  nest() %>% 
  mutate(junk = map(data, function(df) {
    write.csv(file = paste0("../res/temporary_results/ASCII_initial/", unique(df$filename)),
              x = df %>% select(- filename),
              row.names = F)
    }))

# print processing time
s2 <- Sys.time()
print(paste0("processing time: ", round(difftime(s2, s1, units = "secs"), digits = 2), " secs"))
