
             #################################################################################
             ###                                                                           ###
             ###  program that starts from the output of "03_process_CTD_v1.03.R",         ###
             ###  i.e. from the individual CTD profiles that have been smoothed.           ###
             ###  It then discards the bad ones and averages the kept ones (based          ###
             ###  on the flag_QC field), and gather the data and metadata into a           ###
             ###  single master file.                                                      ###
             ###                                                                           ###
             ###  An additionnal profile selection has been made to remove unconsistent    ###
             ###  profiles for each station/date group. This filtering has been made       ###
             ###  visually, see section called "PROFILE SELECTION" below.                  ###
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


# LOAD THE INDIVIDUAL ASCII FILES -----------------------------------------

# list the files
mypath <- "../res/temporary_results/ASCII_initial/"
files.in <- list.files(path = mypath, pattern = "*.csv")

# import the metedata in a df
df.meta <- data.table::fread("../files/metadata_and_log/03_IOP_metadata_initial.csv")

# read them, join the metadata and store all in a single df
df.in <- lapply(X = paste0(mypath, files.in),
                FUN = data.table::fread) %>% 
  bind_rows(.id = "profileID") %>% 
  mutate(profileID = parse_number(profileID)) %>% 
  mutate(filename = files.in[profileID]) %>% 
  left_join(df.meta, by = "filename") %>% 
  select(- profileID) %>%  # another one already exists ("Profile_ID")
  filter(flag_qc == 1) %>% 
  mutate(date_UTC = as.POSIXct(date_heure_UTC, format = "%Y-%m-%d")) %>% 
  as_tibble() %>% 
  drop_na(c(Temp.smt, Sal.smt)) # drop Temp and Sal missing values (from smoothing)


# AVERAGE PROFILES --------------------------------------------------------

# define the function used to average. Use the median(n) (which is mean when n is even)
median_profiles <- function(df){
  df.tmp <- df %>% 
    select(filename, Profile_ID, depth.grid, Temp.smt, Sal.smt) %>% 
    group_by(depth.grid) %>%
    # remove "d0x" from filenames
    mutate(fn = sub(filename, pattern = regex("CTD-d\\d{2}.csv"), replacement = "CTD.csv")) %>% 
    summarize(Temp.med = median(Temp.smt, na.rm = T),
              Temp.sde = sd(Temp.smt, na.rm = T),
              Sal.med = median(Sal.smt, na.rm = T),
              Sal.sde = sd(Sal.smt, na.rm = T),
              filename = unique(fn)) # has to reduce dim to 1, to be consistent
  }

# average the profiles for each group of station/date
# (avoid averaging same station name over different cruises)
# put that in df.out (which is not a definitive output!)
df.out <- df.in %>% 
  group_by(station_name, date_UTC) %>% 
  nest() %>% 
  mutate(avg_profile = map(.x = data, .f = median_profiles)) %>% 
  tibble::rowid_to_column("group_ID") %>% 
  unnest(cols = c(avg_profile)) %>% 
  select(- data) %>%
  ungroup() %>% 
  rename(avg_filename = filename)

# temporary df containing only 3 columns: "group_ID", "avg_filename"
# and "lab" which is a short version of avg_filename (label for facet titles)
# lab contains cruise, station and date only
df.tmp <- df.out %>% 
  select(group_ID, avg_filename) %>% 
  mutate(lab = sapply(stringr::str_split(avg_filename, "_"),
                      function(x) {
                        paste(x[2], x[3], strsplit(x[4], "I")[[1]][2], sep = " / ")
                      })) %>% 
  unique()

# join df.tmp to df.out
df.out <- df.out%>% 
  left_join(df.tmp %>% select(- avg_filename), by = "group_ID")

# quick plots for visual checking
# first, re-arrange df.in to "stations and date" groups,
# name that df "df.plot" ; both df.out and df.plot now have
# a common group_ID variable that is used to group profiles
# per station and per date

# define a function that reset profile number to 1 for each group
# (instead of cumulative profile number aver the whole cruises)
reset_Profile_ID <- function(df){
  n <- df$Profile_ID - df$Profile_ID[1] + 1
  return(n)
}

# define the df.plot
df.plot <- df.in %>% 
  select(filename, Profile_ID, depth.grid, station_name, date_UTC, Temp.smt, Sal.smt) %>% 
  group_by(station_name, date_UTC) %>% 
  nest() %>% 
  mutate(n = map(data, reset_Profile_ID)) %>% 
  tibble::rowid_to_column("group_ID") %>% 
  unnest(cols = c(data, n)) %>% 
  left_join(df.tmp, by = "group_ID") # to add the lab field

# define the rainbow color scale for the legend (profile number within each group)
mycolour <- rainbow((max(df.plot$n, na.rm = T)))

# plots (T then S, call that "initial" as it contains all the profiles)
p.t <- ggplot(data = df.plot, aes(x = Temp.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = F, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out, aes(x = Temp.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.8) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour) +
  xlab("Temperature [C]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/01_Temp_avg_initial.pdf", plot = p.t, device = "pdf", width = 25, height = 20)

p.s <- ggplot(data = df.plot, aes(x = Sal.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = F, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out, aes(x = Sal.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.8) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour) +
  xlab("Salinity [p.s.u.]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/05_Sal_avg_initial.pdf", plot = p.s, device = "pdf", width = 25, height = 20)


# PROFILE SELECTION -------------------------------------------------------

# To be able to visually select the profiles that have to be kept/discarded in each group,
# plot the casts having only mulitple profiles (i.e. do not show single profile ones)
# plot T and S profile including a message saying which have been kept
# and which have been discarded (visually selected, many exchanges with Bennet)

# select multiple profiles casts (also include another one, see below)
df.myfilter <- df.plot %>% 
  ungroup() %>% 
  filter(n > 1 & group_ID != 101) %>% # group_ID 101: st125  WP4-4, 2 half profiles but really is 1 only
  select(group_ID) %>% 
  unique()

# tmp format for the filter, to create 2 downscaled versions of df.out and df.plot...
# call them df.out.2 and df.plot.2
myfilter <- df.myfilter$group_ID
df.plot.2 <- df.plot %>% filter(group_ID %in% myfilter)
df.out.2  <- df.out %>% filter(group_ID %in% myfilter)

# same for the colorpalette, downscale it
mycolour.2 <- rainbow((max(df.plot.2$n, na.rm = T)))

# below are the texts that will appear in the plot,
# these have been decided once the plot have been temporarily plotted (!!!)
my.expl <- c("discard #1       ",
             "discard #1 and #2",
             "discard #1 and #2",
             "keep only #2     ",
             "discard #1       ",
             "discard #1       ",
             "average all      ",
             "discard #1 and #2",
             "discard #1       ",
             "discard #1 and #2",
             "discard #1       ",
             "discard #1       ",
             "discard #1       ",
             "average all      ",
             "average all      ",
             "discard #1       ",
             "average all      ")

# define df.void, i.e. a df that contains the text labels
df.void <- data.frame(lab = unique(df.plot.2$lab),
                      expl = my.expl,
                      x = rep(0.4, length(my.expl)),
                      y = rep(1, length(my.expl)))

# and plot, call the output pdf files "initial_multiple_casts_profiles"
p.t.2 <- ggplot(data = df.plot.2, aes(x = Temp.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = T, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.2, aes(x = Temp.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.4) +
  facet_wrap(~lab, scales = "free") +
  theme_bw() +
  scale_color_manual(values = mycolour.2) +
  xlab("Temperature [C]") +
  ylab("Depth [m]") +
  geom_label(data = df.void, label = df.void$expl, x = -Inf, y = Inf, cex = 3, hjust = 0, vjust = 1, inherit.aes = F)
ggsave("../res/temporary_results/profiles_averaging/02_Temp_avg_initial_multiple_casts_profiles.pdf", plot = p.t.2, device = "pdf", width = 15, height = 10)

p.s.2 <- ggplot(data = df.plot.2, aes(x = Sal.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = T, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.2, aes(x = Sal.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.4) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour.2) +
  xlab("Salinity [p.s.u.]") +
  ylab("Depth [m]") +
  geom_label(data = df.void, label = df.void$expl, x = -Inf, y = Inf, cex = 3, hjust = 0, vjust = 1, inherit.aes = F)
ggsave("../res/temporary_results/profiles_averaging/06_Sal_avg_initial_multiple_casts_profiles.pdf", plot = p.s.2, device = "pdf", width = 15, height = 10)


# FINAL AVERAGING ---------------------------------------------------------

# in this last section, we filter according to what has been decided previously
# and average the remaining profiles.
bad.filename <- df.plot %>%
  filter(group_ID %in% df.myfilter$group_ID) %>% 
  ungroup() %>% 
  filter((group_ID == 2) & (n == 1) |
           (group_ID == 3) & ((n == 1) | (n ==2)) |
           (group_ID == 6) & ((n == 1) | (n ==2)) |
           (group_ID == 7) & ((n == 1) | (n ==3)) |
           (group_ID == 8) & (n == 1) |
           (group_ID == 9) & (n == 1) |
           #(group_ID == 10) | # keep all profiles here
           (group_ID == 11) & ((n == 1) | (n ==2)) |
           (group_ID == 12) & (n == 1) |
           (group_ID == 13) & ((n == 1) | (n ==2)) |
           (group_ID == 14) & (n == 1) |
           (group_ID == 15) & (n == 1) |
           (group_ID == 16) & (n == 1) |
           #(group_ID == 31) | # keep all profiles here
           #(group_ID == 32) | # keep all profiles here
           (group_ID == 34) & (n == 1)) %>% 
           #(group_ID == 45)) # keep all profiles here
  select(filename) %>% 
  unique()

# define the function used to average. Use the average this time, instead of median, as all the 
# kept profiles are close to each other (might also smoothen up the remaining spikes)
mean_profiles <- function(df){
  df.tmp <- df %>% 
    select(filename, Profile_ID, depth.grid, Temp.smt, Sal.smt) %>% 
    group_by(depth.grid) %>%
    mutate(fn = sub(filename, pattern = regex("CTD-d\\d{2}.csv"), replacement = "CTD.csv")) %>% 
    summarize(Temp.med = mean(Temp.smt, na.rm = T, trim = 0),
              Temp.sde = sd(Temp.smt, na.rm = T),
              Sal.med = mean(Sal.smt, na.rm = T, trim = 0),
              Sal.sde = sd(Sal.smt, na.rm = T),
              filename = unique(fn)) # has to reduce dim to 1, to be consistent
}

# badf <- bad.filename$filename contains the filenames that have to be
# removed before we apply again the avergaring. So: Do the averaging again then,
# but first discard these files. call the output df.out.final instead of df.out
badf <- bad.filename$filename
df.out.final <- df.in %>% 
  filter(!filename %in% badf) %>% 
  group_by(station_name, date_UTC) %>% 
  nest() %>% 
  mutate(avg_profile = map(.x = data, .f = mean_profiles)) %>% 
  tibble::rowid_to_column("group_ID") %>% 
  unnest(cols = c(avg_profile)) %>% 
  select(- data) %>%
  ungroup() %>% 
  rename(avg_filename = filename) %>% 
  left_join(df.tmp %>% select(- avg_filename), by = "group_ID")

df.plot.final <- df.in %>% 
  filter(!filename %in% badf) %>% 
  select(filename, Profile_ID, depth.grid, station_name, date_UTC, Temp.smt, Sal.smt) %>% 
  group_by(station_name, date_UTC) %>% 
  nest() %>% 
  mutate(n = map(data, reset_Profile_ID)) %>% 
  tibble::rowid_to_column("group_ID") %>% 
  unnest(cols = c(data, n)) %>% 
  left_join(df.tmp, by = "group_ID") # to add the lab field

# and plot the final pdf
p.t.final <- ggplot(data = df.plot.final, aes(x = Temp.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = F, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.final, aes(x = Temp.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.8) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour) +
  xlab("Temperature [C]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/03_Temp_avg_final.pdf", plot = p.t.final, device = "pdf", width = 25, height = 20)

p.s.final <- ggplot(data = df.plot.final, aes(x = Sal.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = F, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.final, aes(x = Sal.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.8) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour) +
  xlab("Salinity [p.s.u.]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/07_Sal_avg_final.pdf", plot = p.s.final, device = "pdf", width = 25, height = 20)

# do the selection again on the multiple casts stations. Call these df and plots "final_multiple_casts_profiles"
# select multiple profiles casts (also include another one, see below)
df.myfilter.final <- df.plot.final %>% 
  ungroup() %>% 
  filter(n > 1 & group_ID != 101) %>% # group_ID 101: st125  WP4-4, 2 half profiles but really is 1 only
  select(group_ID) %>% 
  unique()

# tmp format for the filter, to create 2 downscale versions of df.out and df.plot...
# call them df.out.2 and df.plot.2
myfilter.final <- df.myfilter.final$group_ID
df.plot.final.multiple <- df.plot.final %>% filter(group_ID %in% myfilter)
df.out.final.multiple  <- df.out.final %>% filter(group_ID %in% myfilter)

# same for the colorpalette, downscale it
mycolour.multiple <- rainbow((max(df.plot.final.multiple$n, na.rm = T)))

# and plot
p.t.final.multiple <- ggplot(data = df.plot.final.multiple, aes(x = Temp.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = T, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.final.multiple, aes(x = Temp.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.4) +
  facet_wrap(~lab, scales = "free") +
  theme_bw() +
  scale_color_manual(values = mycolour.multiple) +
  xlab("Temperature [C]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/04_Temp_avg_final_multiple_casts_profiles.pdf", plot = p.t.final.multiple, device = "pdf", width = 15, height = 10)

p.s.final.multiple <- ggplot(data = df.plot.final.multiple, aes(x = Sal.smt, y = depth.grid, col = factor(n))) +
  geom_point(show.legend = T, size = 0.3) +
  scale_y_reverse() +
  geom_point(data = df.out.final.multiple, aes(x = Sal.med, y = depth.grid), col = "black", inherit.aes = F, size = 0.4) +
  facet_wrap(~lab, scales = "free") +
  theme_bw()  +
  scale_color_manual(values = mycolour.multiple) +
  xlab("Salinity [p.s.u.]") +
  ylab("Depth [m]")
ggsave("../res/temporary_results/profiles_averaging/08_Sal_avg_final_multiple_casts_profiles.pdf", plot = p.s.final.multiple, device = "pdf", width = 15, height = 10)


# OUTPUT IN ASCII FILES ---------------------------------------------------

# well, 1st get a general master file, unique, from which we can
# extract whatever we need, like individuals files, etc if needed

# requests from Bennet:
# 1: empty stations (where CTD casts have been discarded, for ice chunks for example) included, but set to NA
# 2: add a field giving the station names with the following format: X_stnYYY where X is the sub-cruise number and YYY the station name

# df.out.final already contains all that we need, well, almost...
# we want to output all the data in a single file, including metadata
# so, get an updated (manually!! :o/) metadata constructor df
df.meta.final <- data.table::fread("../files/metadata_and_log/04_IOP_for_meta_final.csv") %>% 
  select(-flag_QC)

# and join that metadata df with the final general output
# also, modify avg filename so that they are consistent
# with Bennet's Gdrive document
df.final <- df.meta.final %>% 
  left_join(df.out.final %>% 
              rename(filename = avg_filename), by = "filename") %>% 
  select(- station_name.y) %>% 
  rename(station_name = station_name.x) %>% 
  mutate(filename = sub(x = filename, pattern = "_st", replacement = "_STN")) %>%                 # replace "st" by "STN"
  mutate(filename = sub(x = filename, pattern = "-alt", replacement = "alt")) %>%                 # replace "-alt" by "alt"
  mutate(filename = sub(x = filename, pattern = "alt-", replacement = "alt")) %>%                 # replace "alt-" by "alt"
  mutate(filename = sub(x = filename, pattern = "STN380altb", replacement = "STN380alt")) %>%     # replace "altb" by "alt"
  mutate(filename = sub(x = filename, pattern = "TEST-shingle", replacement = "ShingleTest")) %>% # replace "TEST-shingle" by "ShingleTest"
  mutate(filename = sub(x = filename, pattern = "alts", replacement = "alt2"))                    # replace "-alts" by "alt2"

# df.final then contains the data and the metadata, we can therefore
# get anything out of it, e.g. output individual ascii files, metadata files, produce plots etc.
# output the main masterfile, remove all fields for plotting, mainly
# and add 2 fields for the handheld measurements of leg1
df.masterfile <- df.final %>% 
  select(- c(Temp_min, Temp_max, Sal_min, Sal_max, EC_Press_dbar_val, tare_depth, closest_EC_sector, group_ID, lab, date_UTC)) %>% 
  unique() %>% 
  rename("filename [NA]" = filename) %>% 
  rename("station [NA]" = station_name) %>% 
  rename("cruise_station [NA]" = cruise_station) %>% 
  rename("latitude [decimal degree]" = lat_dec_deg) %>% 
  rename("longitude [decimal degree]" = lon_dec_deg) %>% 
  rename("utc_date_time [yyyy-mm-dd hh:mm]" = date_heure_UTC) %>% 
  rename("comment [NA]" = comment) %>% 
  rename("depth [m]" = depth.grid) %>% 
  rename("temperature [C]" = Temp.med) %>% 
  rename("temperature_sdev [C]" = Temp.sde) %>% 
  rename("salinity [NA]" = Sal.med) %>% 
  rename("salinity_sdev [NA]" = Sal.sde) %>% 
  mutate(`temperature_surface_handheldmeter [C]` = NA) %>% 
  mutate(`salinity_surface_handheldmeter [NA]` = NA) %>% 
  select("filename [NA]":"temperature_sdev [C]", "temperature_surface_handheldmeter [C]", everything()) %>% 
  as_tibble()

# replace NaN by NA, to be homogeneous in the entire df / file
df.masterfile[is.na(df.masterfile)] <- NA

# remove negative depths. Negative depths more likely occurred
# during the depth tare correction (e.g., overcorrection)
# !!! keep the stations with no data, i.e. for which depth is NA only
# we don't want to remove them, so keep NA, just remove negative values
df.masterfile <- df.masterfile %>%
  filter((`depth [m]` >= 0.0)
         %>% replace_na(TRUE))

# final step for df.masterfile: add data that have been taken with a hand held device (leg 1)
# and add stations where no data have been acquired with the IOP package at all
# (from https://docs.google.com/spreadsheets/d/1sWq0qyCd0B4hyjyruQJsV68fDZ_5etinS6wpAbn4rn8/edit#gid=0)
# i.e. add stations 1_STN540alt, "2_STNxxx", "2_XX2", "2_XX3", "3_STNxxx", "3_STNR13", "4_STNXX4" 
# (we do not include "3_STN130_5m", "3_STNR02_5m", "3_STNR09_20m", as they are already
# included in the whole IOP profiles)
# 1) start with stations with no existing IOP profile
df.masterfile <- df.masterfile %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-1_STN540alt_I20190422_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "STN540alt",
          "cruise_station [NA]" = "1_STN540alt",
          "latitude [decimal degree]" = 69.28916,
          "longitude [decimal degree]" = -136.32489,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-04-22 21:25",
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = 0.1,
          "salinity_surface_handheldmeter [NA]" = 0.18) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-2_STNxxx_I20190615_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "STNxxx",
          "cruise_station [NA]" = "2_STNxxx",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-06-15 18:00", # set at 12:00 local time
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-2_XX2_I20190625_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "XX2",
          "cruise_station [NA]" = "2_XX2",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-06-25 20:00",
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-2_XX3_I20190629_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "XX3",
          "cruise_station [NA]" = "2_XX3",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-06-29 04:00",
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-3_STNxxx_I20190725_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "xxx",
          "cruise_station [NA]" = "3_STNxxx",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-07-25 18:00", # set at 12:00 local time
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-3_STNR13_I20190729_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "R13",
          "cruise_station [NA]" = "3_STNR13",
          "latitude [decimal degree]" = 68.35953,
          "longitude [decimal degree]" = -133.719921,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-07-29 18:00", # set at 12:00 local time
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-4_STNXX4_I20190828_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "XX4",
          "cruise_station [NA]" = "4_STNXX4",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-08-28 18:00", # set at 12:00 local time
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA) %>% 
  add_row("filename [NA]" = "Nunataryuk_WP4-4_STNXX4-2_I20190828_R20200109_BECU_IOP-CTD.csv",
          "station [NA]" = "XX4-2",
          "cruise_station [NA]" = "4_STNXX4-2",
          "latitude [decimal degree]" = 68.35713,
          "longitude [decimal degree]" = -133.7384,
          "utc_date_time [yyyy-mm-dd hh:mm]" = "2019-08-28 18:00", # set at 12:00 local time
          "flag_qc" = 0,
          "comment [NA]" = NA,
          "depth [m]" = NA,
          "temperature [C]" = NA,
          "temperature_sdev [C]" = NA,
          "salinity [NA]" = NA,
          "salinity_sdev [NA]" = NA,
          "temperature_surface_handheldmeter [C]" = NA,
          "salinity_surface_handheldmeter [NA]" = NA)
# and 2) add the handheld measurements of leg1 for existing IOP profiles, see
# https://docs.google.com/spreadsheets/d/16x2brcYqCW9LrnoH0MG01JUYWKv2MTj1mxbU9JSVjZE/edit#gid=0
# stations with discarded IOP measurements: 150alt, 140alot, 340alt, 360, 380alt (380alt from 22 April)
# station 1_STN150alt T and S
df.masterfile$`temperature_surface_handheldmeter [C]`[df.masterfile$`cruise_station [NA]` == "1_STN150alt"] = 0.10
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN150alt"] = 0.23
# station 1_STN140alt T and S
df.masterfile$`temperature_surface_handheldmeter [C]`[df.masterfile$`cruise_station [NA]` == "1_STN140alt"] = -0.10
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN140alt"] = 0.25
# station 1_STN340alt
df.masterfile$`temperature_surface_handheldmeter [C]`[df.masterfile$`cruise_station [NA]` == "1_STN340alt"] = 0.20
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN340alt"] = 0.17
# station 1_STN360, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN360"] = 0.01
# station 1_STN380alt 2019-04-22 22:50
df.masterfile$`temperature_surface_handheldmeter [C]`[(df.masterfile$`cruise_station [NA]` == "1_STN380alt") &
                                                        (df.masterfile$`utc_date_time [yyyy-mm-dd hh:mm]` == "2019-04-22 22:50")] = -0.40
df.masterfile$`salinity_surface_handheldmeter [NA]`[(df.masterfile$`cruise_station [NA]` == "1_STN380alt") &
                                                      (df.masterfile$`utc_date_time [yyyy-mm-dd hh:mm]` == "2019-04-22 22:50")] = 0.04
# and why not stations where IOP profiles were performed:
# 0a, 0b, 550, 01, 370alt, 870, 860, 350, 380alt(25/4), 830, 20, 810, 840, 740 and 40
# station 1_STN0a, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN0a"] = 0.01
# station 1_STN0b, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN0b"] = 0.01
# station 1_STN550
df.masterfile$`temperature_surface_handheldmeter [C]`[df.masterfile$`cruise_station [NA]` == "1_STN550"] = -0.01
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN550"] = 0.20
# station 1_STN01, S only, as T is not sure (log says "2.1?")
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN01"] = 0.15
# station 1_STN370alt, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN370alt"] = 0.09
# station 1_STN870, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN870"] = 0.16
# station 1_STN860, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN860"] = 0.12
# station 1_STN350, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN350"] = 0.16
# station 1_380alt 2019-04-25 23:45, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[(df.masterfile$`cruise_station [NA]` == "1_STN380alt") & 
                                                      (df.masterfile$`utc_date_time [yyyy-mm-dd hh:mm]` == "2019-04-25 23:45")] = 0.17
# station 1_STN830, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN830"] = 0.16
# station 1_STN020, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN020"] = 0.16
# station 1_STN810, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN810"] = 0.16
# station 1_STN840, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN840"] = 0.15
# station 1_STN740, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN740"] = 0.16
# station 1_STN40, S only, no T
df.masterfile$`salinity_surface_handheldmeter [NA]`[df.masterfile$`cruise_station [NA]` == "1_STN040"] = 0.15
  
# finally, output a csv MasterFile, i.e. for all the stations together...
data.table::fwrite(x = df.masterfile,
                   file = "../res/ASCII_final/unique_Masterfile/Nunataryuk_WP4_Masterfile_v1-01.csv",
                   append = F,
                   sep = ";",
                   na = "NA",
                   row.names = F)

# ... and generate individual ASCII files, i.e. 1 per cruise/station/date element
df.ascii <- df.masterfile %>% 
  mutate(filename = `filename [NA]`) %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(void = map(data, parse_ind <- function(df) {
    write.csv(file = paste0("../res/ASCII_final/individual_files/", unique(df$`filename [NA]`)),
              x = df,
              row.names = F)
  })) %>% 
  unnest(cols = c(data))

# plot T and S profiles including std-dev when available
# start from df.final, and apply similar processing than for df.masterfile
# (remove NA and negative depths)
df.final[is.na(df.final)] <- NA
df.final <- df.final %>%
  filter((depth.grid >= 0.0)
         %>% replace_na(TRUE))

# create a df called df.pdf (slightly different format)
df.pdf <- df.final %>% 
  filter(flag_qc == 1) %>% 
  select(- c(flag_qc, comment)) %>% 
  select(- station_name) %>% 
  separate(col = cruise_station, into = c("cruise", "station"), sep = "_STN") %>% 
  group_by(filename) %>% 
  mutate(group_ID = group_indices()) %>% 
  mutate(page = ((group_ID - 1) %/% 4) + 1)

# I wanted to plot T and S on the same plot, but as ggplot's philosophy is
# against this approach, I'musing the good old plot command :-)
pdf(file = "../res/PDF_final/Nunataryuk_WP4_Masterfile_v1-01.pdf", width = 30, height = 30)
par(bg = "black")
par(mfrow=c(12,11),
    oma = c(5,5,5,0) + 0.1,
    mar=c(5, 2, 2, 3) + 0.1)
for (i in unique(df.pdf$group_ID)) {
  df <- df.pdf %>% filter(group_ID == i)

  # plot temperature on 1st x-axis
  plot(x = df$Temp.med, y = df$depth.grid,
       xlim = c(unique(df$Temp_min), unique(df$Temp_max)),
       ylim = c(max(df$depth.grid, na.rm = TRUE), 0.0),
       pch = 20, col = "darkolivegreen2", cex = 0.5,
       xaxt = "n", xlab = "", ylab = "", 
       type = "b", col.axis = "white", bty = "n")
  box(col = 'white')
  axis(1, xlim = c(unique(df$Temp_min), unique(df$Temp_max)),
       col = "darkolivegreen2",
       col.axis = "darkolivegreen2",
       las = 1)  ## las=1 makes horizontal labels

  # allow 2nd plot on the same graph
  par(new = TRUE)
  
  # plot salinity on 2nd x-axis
  plot(x = df$Sal.med, y = df$depth.grid,
       xlim = c(unique(df$Sal_min), unique(df$Sal_max)),
       ylim = c(max(df$depth.grid, na.rm = TRUE), 0.0),
       pch = 18, col = "darkturquoise", cex = 0.5,
       xaxt = "n", xlab = "", ylab = "", 
       type = "b", bty = "n")
  
  # some aesthetics
  axis(3, xlim = c(unique(df$Sal_min), unique(df$Sal_max)),
       col = "darkturquoise",
       col.axis = "darkturquoise",
       las = 1)
  axis(2, col="white", col.axis = "white")
  mtext(unique(df$lab), side = 3, col = "white", line = 2, cex = 0.75)
  
  # print the overall labels
  mtext("Temperature [C]", side = 1, outer = TRUE, line = 2, col = "darkolivegreen2", cex = 2)
  mtext("Depth [m]", side = 2, outer = TRUE, line = 2, col = "white", cex = 2)
  mtext("Salinity [n.a.]", side = 3, outer = TRUE, line = 2, col = "darkturquoise", cex = 2)
}
dev.off()


# OUTPUT FINAL METADATA FILE ----------------------------------------------
# homogeneize the metadata df "filename" column so that the station
# names fit the "station_name" and "cruise_station" columns
# ex: mv Nunataryuk_WP4-1_st01_I20190423_R20200109_BECU_IOP-CTD.csv to
#     to Nunataryuk_WP4-1_STN01_I20190423_R20200109_BECU_IOP-CTD.csv
df.meta.output <- df.meta.final %>% 
  as_tibble() %>% 
  select(-c(Temp_min, Temp_max, Sal_min, Sal_max)) %>% 
  select(filename:flag_qc, comment, closest_EC_sector, everything()) %>% 
  separate(filename, into = c("f1", "f2", "f3", "f4", "f5", "f6", "f7"), sep = "_") %>% 
  select(-f3) %>% 
  mutate(f3 = station_name) %>% 
  unite("filename", c(f1, f2, f3, f4, f5, f6, f7), sep = "_")
  
data.table::fwrite(x = df.meta.output,
                   file = "../res/ASCII_final/Nunataryuk_WP4_Metadata_v1-01.csv",
                   append = F,
                   sep = ";",
                   dateTimeAs = "write.csv",
                   na = "NA",
                   row.names = F,
                   col.names = T, quote = F)

# print processing time
s2 <- Sys.time()
print(paste0("processing time: ", round(difftime(s2, s1, units = "secs"), digits = 2), " secs"))
