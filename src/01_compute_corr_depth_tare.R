

             #################################################################################
             ###                                                                           ###
             ###  program that gets the in-air values of the pressure of the Nunataryuk    ###
             ###  CTD casts, so that we can get the Atmospheric Pressure measured          ### 
             ###  at Shingle Point, Tuktoyaktuk, Aklavik and Inuvik from EC web site       ###
             ### (https://climat.meteo.gc.ca/historical_data/search_historic_data_f.html)  ###
             ###                                                                           ###
             ###  Guislain.Becu@takuvik.ulaval.ca - February 2019 (rev. Jan 2022)          ###
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
library(ggplot2)
library(geosphere)


# GET ENV. CANADA ATM. PRESS. TIMESERIES ----------------------------------

# From April to Septempber 2019, we fetch the atmospheric pressure values from the EC weather stations
# and will get date and time of each CTD profile from metadata. Also, we'll compute the shortest ground station
# to identify which EC sector we'll compare the CTD cast to, in terms of atmospheric pressure.
# now get EC values time series at the 4 locations
mypath <- "../files/EC_AtmPress/"
files.shp <- list.files("../files/EC_AtmPress/", pattern = "ShinglePoint")
files.tuk <- list.files("../files/EC_AtmPress/", pattern = "Tuktoyaktuk")
files.akl <- list.files("../files/EC_AtmPress/", pattern = "Aklavik")
files.ink <- list.files("../files/EC_AtmPress/", pattern = "Inuvik")

# collect individual location data
#   Shingle Point
df.shp <- lapply(X = paste0(mypath, files.shp),
                 FUN = data.table::fread, fill=TRUE, na.strings= "") %>% 
  bind_rows(.id = "original_file_ID")

# Tuktoyaktuk
df.tuk <- lapply(X = paste0(mypath, files.tuk),
                 FUN = data.table::fread, fill=TRUE, na.strings= "") %>% 
  bind_rows(.id = "original_file_ID")

# Aklavik
df.akl <- lapply(X = paste0(mypath, files.akl),
                 FUN = data.table::fread, fill=TRUE, na.strings= "") %>% 
  bind_rows(.id = "original_file_ID")

# Inuvik
df.ink <- lapply(X = paste0(mypath, files.ink),
                 FUN = data.table::fread, fill=TRUE, na.strings= "") %>% 
  bind_rows(.id = "original_file_ID")

# and bind them together, then re-arrange them slightly
df.ec <- rbind(df.shp, df.tuk, df.akl,df.ink) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(original_file_id, longitude_x, latitude_y, nom_de_la_station, 
         #date_heure, annee, mois, jour, heure, pression_a_la_station_k_pa)%>% 
         date_heure, pression_a_la_station_k_pa)%>% 
  mutate(pression_a_la_station_k_pa = parse_number(pression_a_la_station_k_pa, locale = locale(decimal_mark = ","))) %>% 
  mutate(date_heure = as.POSIXct(date_heure, format = "%Y-%m-%d %H:%M")) %>% 
  mutate(original_file_id = parse_number(original_file_id)) %>% 
  mutate(longitude_x = parse_number(longitude_x, locale = locale(decimal_mark = ","))) %>% 
  mutate(latitude_y = parse_number(latitude_y, locale = locale(decimal_mark = ",")))

# get the lat/lon for the 4 EC locations
lat_shp <- unique(df.ec$latitude_y[df.ec$nom_de_la_station == "SHINGLE POINT A"])
lon_shp <- unique(df.ec$longitude_x[df.ec$nom_de_la_station == "SHINGLE POINT A"])

lat_tuk <- unique(df.ec$latitude_y[df.ec$nom_de_la_station == "TUKTOYAKTUK A"])
lon_tuk <- unique(df.ec$longitude_x[df.ec$nom_de_la_station == "TUKTOYAKTUK A"])

lat_akl <- unique(df.ec$latitude_y[df.ec$nom_de_la_station == "AKLAVIK A"])
lon_akl <- unique(df.ec$longitude_x[df.ec$nom_de_la_station == "AKLAVIK A"])

lat_ink <- unique(df.ec$latitude_y[df.ec$nom_de_la_station == "INUVIK CLIMATE"])
lon_ink <- unique(df.ec$longitude_x[df.ec$nom_de_la_station == "INUVIK CLIMATE"])


# WORK ON IN-SITU DATA: METADATA, MEAN PRESSURE IN AIR ETC. ---------------

# construct a temporary metadata df from existing log and working files
info.iop <- data.table::fread("../files/metadata_and_log/01_iop_info.csv")
pre.meta <- data.table::fread("../files/metadata_and_log/02_for_metadata.csv")

# define a function to compute distance [km] from the 4 EC locations, given a CTD cast lat/lon
get_dist <- function(df){
  lat <- df$lat_dec_deg
  lon <- df$lon_dec_deg
  df$dist_shp <- distHaversine(p1 = c(lon, lat), p2 = c(lon_shp, lat_shp)) / 1000.0
  df$dist_tuk <- distHaversine(p1 = c(lon, lat), p2 = c(lon_tuk, lat_tuk)) / 1000.0
  df$dist_akl <- distHaversine(p1 = c(lon, lat), p2 = c(lon_akl, lat_akl)) / 1000.0
  df$dist_ink <- distHaversine(p1 = c(lon, lat), p2 = c(lon_ink, lat_ink)) / 1000.0
  idx <- which.min(c(df$dist_shp, df$dist_tuk, df$dist_akl, df$dist_ink))
  sector_list <- c("SHINGLE POINT A", "TUKTOYAKTUK A", "AKLAVIK A", "INUVIK CLIMATE")
  df$sector <- sector_list[idx]
  dist <- data.frame(df$dist_shp, df$dist_tuk, df$dist_akl, df$dist_ink, df$sector)
}

# now for each filename in metadata, compute the distance to each of the 4 locations
# and find the closest. put the answer in a parameter called "sector", saved in the metadata df
df.meta.tmp <- info.iop %>% 
  left_join(pre.meta, by = "filename") %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(dist = map(data, get_dist)) %>% 
  unnest(cols = c(data, dist))

# Define a function that will get EC atm. pres. for each CTD cast (time based)
get_EC_Press_value <- function(df){
  df.tmp <- df.ec %>% filter((nom_de_la_station == unique(df$df.sector)) & 
                               (date_heure == unique(lubridate::round_date(df$date_heure_MDT, unit = "hour"))))
  df$EC_Press_dbar_val <- rep(unique(df.tmp$pression_a_la_station_k_pa), length(df$filename))
}

# now work on the files that have been kept (manually) for possible measures in air
tare.files.list <- list.files("../files/data/files_for_tare_corr/", pattern = "*csv")
df.tareall   <- lapply(X = paste0("../files/data/files_for_tare_corr/", tare.files.list),
                       FUN = data.table::fread) %>% 
  bind_rows(.id = "profile_ID") %>% 
  mutate(profile_ID = parse_number(profile_ID)) %>% 
  group_by(profile_ID) %>% 
  mutate(filename = tare.files.list[profile_ID]) %>% 
  ungroup() %>% 
  mutate(page = ((profile_ID - 1) %/% 16) + 1)

# Get the pressure in air, when available (i.e. when salinity == 0.0)
df.tarekeep <- df.tareall %>% 
  select(- c(Cond, Chla, pH, DOconc, DOsat, datetime, DateTime)) %>% 
  filter(Sal <= 0.00) %>% 
  left_join(df.meta.tmp, by = "filename") %>% 
  select(- c(Temp, median_range, fct, loess_span, temp_min,
             temp_max, sal_min, sal_max, comment)) %>% 
  mutate(date_heure_MDT = as.POSIXct(paste(date_mdt, time_start_mdt, sep = " "), format = "%Y-%m-%d %H:%M")) %>% 
  group_by(profile_ID) %>% 
  mutate(CTD_Press_dbar_avg = median(Pres_dbar, na.rm = T)) %>% 
  #select(- c(Pres_dbar, Sal)) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(profile_ID) %>% 
  nest() %>% 
  mutate(Pres_dbar_EC = map(data, get_EC_Press_value)) %>%
  unnest(cols = c(data, Pres_dbar_EC))
  
# linear regression of EC and in-air measurements
mylm <- lm(df.tarekeep$CTD_Press_dbar_avg ~ df.tarekeep$Pres_dbar_EC)
a   <- mylm$coefficients[2] # slope
b   <- mylm$coefficients[1] # intercept
rsq <- summary(mylm)$r.squared # r squared
df.lm <- data.frame(x = c(90, 110), y = c(a * 90 + b, a * 110 + b))


# FEW PLOTS ---------------------------------------------------------------

# define custom colors (reverse rainbow)
mycolour <- rainbow(length(unique(df.tarekeep$station_name)))

# plot of EC vs casts for each sector
p.tarekeep <- ggplot(data = df.tarekeep,
                     aes(x = Pres_dbar_EC, y = CTD_Press_dbar_avg, colour = station_name, shape = df.sector)) +
  geom_point(size = 5) +
  coord_cartesian(xlim = c(99, 105), ylim = c (-0.20, 0.25)) +
  geom_line(data = df.lm, aes(x = x, y = y), inherit.aes = F) +
  xlab("Environment Canada atmospheric pressure [kPa]") +
  ylab("In-Situ CTD based depth measured in air [m]") +
  scale_colour_manual(name = "Stations",
                      values = mycolour) +
  scale_shape_manual(name = "Env. Canada weather station localization",
                     values = c("SHINGLE POINT A" = 3,
                                "TUKTOYAKTUK A" = 15,
                                "AKLAVIK A" = 16,
                                "INUVIK CLIMATE" = 17)) +
  theme(axis.text = element_text(size=16),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")) +
  annotate(x = 99.00, y = 0.22, geom = "text", label = bquote("y = "*.(signif(a, digits = 2))~" * x -"~.(signif(-b, digits = 4))), hjust = 0) +
  annotate(x = 99.00, y = 0.20, geom = "text", label = bquote("r"^2~"="~.(signif(rsq, digits = 4))), hjust = 0)
#p.tarekeep
ggsave(p.tarekeep, file = "../res/temporary_results/depth_tare_corr/comp_pressure_in_air.pdf", width = 8.5, height = 6, units = "in")

# check which points have been kept "in-air", change "page" 
# to 1, 2 or 3, otherwise there are too many plots per facet wrap page
for (i.page in 1:3) {
  p.all <- ggplot(df.tareall %>% filter(page == i.page),
                  aes(x = Time_ms,
                      y = Pres_dbar)) +
    geom_point(shape = 42, col = "grey", size = 0.4) +
    geom_point(data = df.tarekeep %>% filter(page == i.page),
               aes(x = Time_ms,
                   y = Pres_dbar),
               inherit.aes = F,
               col = "black",
               size = 0.4,
               shape = 42) +
    scale_y_reverse() + 
    theme(axis.text.x = element_text(size = 3)) +
    theme(axis.text.y = element_text(size = 3)) +
    facet_wrap(~ filename, scales = "free", ncol = 4) +
    theme(strip.text.x = element_text(size = 5)) +
    theme(strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))
  ggsave(plot = p.all, filename = paste0("../res/temporary_results/depth_tare_corr/check.tare.files_", as.character(i.page), ".pdf"),
         device = "pdf", width = 14, height = 9, units = "in")
}

# time series plots of Environnement Canada database
p.ec <- ggplot(data = df.ec,
               aes(x = date_heure, y = pression_a_la_station_k_pa, colour = nom_de_la_station)) +
  geom_line(size = 0.3) +
  coord_cartesian(ylim = c(98, 104)) +
  xlab("Date") +
  ylab("Atmospheric Pressure [kPa]") +
  theme_bw() +
  scale_colour_manual(name = "Env. Canada station name",
                      values = c("SHINGLE POINT A" = "black",
                                 "TUKTOYAKTUK A" = "grey",
                                 "AKLAVIK A" = "blue",
                                 "INUVIK CLIMATE" = "red"),
                      labels = c("SHINGLE POINT A" = "Shingle Point",
                                 "TUKTOYAKTUK A" = "Tuktoyaktuk",
                                 "AKLAVIK A" = "Aklavik",
                                 "INUVIK CLIMATE" = "Inuvik"))
#p.ec
ggsave(p.ec, file = "../res/temporary_results/depth_tare_corr/EC_Pres_timeseries.pdf", height = 8, width = 20, units = "in")

# draw a map to show the CTD stations and the EC stations, see Clark Richard's web
# post on the OCE package(see https://clarkrichards.org/page2/)
library(oce)
library(ocedata) #for the coastlineWorldFine data
library(rgdal)
library(marmap)
library(sf)
data(coastlineWorldFine)

mp <- function() {
  mapPlot(coastlineWorldFine, projection="+proj=stere +lon_0=-130 +lat_0=90",
          longitudelim = c(-137, -133),
          latitudelim = c(68.3, 69.7), col='grey')
}
# get bathymetry file, store it automatically in
# "./marmap_coord_-150;66;-115;74_res_4.csv"
c <- as.topo(getNOAA.bathy(-150, -115, 66, 74, keep=TRUE))
pdf(file = "../res/temporary_results/depth_tare_corr/Nunataryuk_map.pdf", width = 14, height = 9) 
mp()
mapImage(c, col=oceColorsGebco, breaks = seq(-4000, 0, 500))
mapPolygon(coastlineWorldFine, col='grey')
mapText(df.tarekeep$lon_dec_deg, df.tarekeep$lat_dec_deg, df.tarekeep$station_name,
        col = mycolour, pos = 4, offset = 0.75)
mapPoints(df.tarekeep$lon_dec_deg, df.tarekeep$lat_dec_deg,
          col = mycolour, pch = 16)
# simplify the EC sectors df to add them on the map
df.ec.map <- df.ec %>% select(longitude_x, latitude_y, nom_de_la_station) %>% unique()
mapText(df.ec.map$longitude_x, df.ec.map$latitude_y, df.ec.map$nom_de_la_station,
        col = "black", pos = 4, offset = 0.75)
mapPoints(df.ec.map$longitude_x, df.ec.map$latitude_y,
          col = "black", pch = "+", cex = 2)
dev.off()


# COMPUTE THE CORRECTING DEPTH TARE VALUE FOR ALL THE CTD CASTS WITH THE LM FIT ------------------

# 1st thing, we need to interpolate EC atm pressure measurements on 2019-08-29 at 14:00 as it is missing
# at the Shingle Point station, and as it is needed during the WP4-4 station 125 casts (2 casts)
# the easy way: 13h pressure = 101.68 kPa, 15h pressure = 101.66 kPA, so put 14h pressure value to 101.67 kPa!
df.ec$pression_a_la_station_k_pa[df.ec$nom_de_la_station == "SHINGLE POINT A" & df.ec$date_heure == "2019-08-29 14:00:00"] <- 101.67

# then we retrieve the date/time from the tmp metadata df and get the EC value out of it. When we have it, we use the lm coeffs
# to compute the correcting depth tare with the equation tare_depth = (a * EC_press) + b, where a and b are the lm coeffs
df.meta.final <- df.meta.tmp %>% 
  tibble::rowid_to_column("Profile_ID") %>% 
  mutate(date_heure_UTC = as.POSIXct(paste(date_utc, time_start_utc, sep = " "), format = "%Y-%m-%d %H:%M")) %>% 
  mutate(date_heure_MDT = as.POSIXct(paste(date_mdt, time_start_mdt, sep = " "), format = "%Y-%m-%d %H:%M")) %>%
  group_by(Profile_ID) %>%
  nest() %>% 
  mutate(EC_Press_dbar_val = map(data, get_EC_Press_value)) %>% 
  unnest(col = c(data, EC_Press_dbar_val)) %>% 
  mutate(tare_depth = ((a * EC_Press_dbar_val) + b)) %>%
  select(- c("median_range", "fct", "loess_span", "temp_min", "temp_max",
             "sal_min", "sal_max", "df.dist_shp", "df.dist_tuk",
             "df.dist_ink", "df.dist_akl", "date_utc", "time_start_utc",
             "date_mdt", "time_start_mdt")) %>% 
  rename(closest_EC_sector = df.sector) %>% 
  select(Profile_ID, filename, station_name, lat_dec_deg, lon_dec_deg, date_heure_UTC,
         date_heure_MDT, flag_qc, EC_Press_dbar_val, tare_depth, comment, closest_EC_sector, everything())

# output the metadata in a csv file. Note that df.meta.final is final only for this code, not for
# the project!
data.table::fwrite(x = df.meta.final,
                   file = "../files/metadata_and_log/03_IOP_metadata_initial.csv",
                   append = F,
                   sep = ";",
                   na = "NA",
                   row.names = F,
                   col.names = T,
                   dateTimeAs = "write.csv")

# print processing time
s2 <- Sys.time()
print(paste0("processing time: ", round(difftime(s2, s1, units = "secs"), digits = 2), " secs"))
