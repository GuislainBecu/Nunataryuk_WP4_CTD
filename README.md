---
title: "Nunataryuk WP4 CTD profiles"
output: html_document
---

### 1  Summary

This repo contains the CTD profiles of legs 1, 2, 3 and 4 of the Nunataryuk project, work package 4 (WP4). This includes raw data files, temporary/working data and meta files, as well as results (text and graphic files).
Other parameters will be added later (e.g., particle size distribution, backscattering coefficient, etc.).
<br>

### 2  Main output

The main results that will be of interest for the users are the following:

  - `./res/ASCII_final/individual_files/*.csv` - this folder contains one file per station/leg/date set (i.e., multiple casts that have been recorded for some stations have been averaged). These files contain depth, temperature and salinity (values that have been smoothed on a 0.01 m depth grid), as well as metadata,
  - `./res/ASCII_final/unique_Masterfile/Nunataryuk_WP4_Masterfile_v1-01.csv` - this file contains the same parameters as above, but gathered in a unique file,
  - `./res/ASCII_final/Nunataryuk_WP4_Metadata_v1-01.csv` - this file contains all the metadata for each station/leg/date set,
  - `./res/PDF_final/Nunataryuk_WP4_Masterfile_v1-01.pdf` - this PDF file displays a 1-pager overall view ofthe complete set of temperature and salinity profiles per station/leg/date set.

The parameters provided in all the ASCII files follow the format "parameter [unit]" and are the following:

  - `filename [NA]`: standardized file name of the file,
  - `station [NA]`: name of the station,
  - `cruise_station [NA]`: name of the leg followed by the name of the station,
  - `latitude [decimal degree]`: latitude in decimal degree,
  - `longitude [decimal degree]`: longitude in decimal degree,
  - `utc_date_time [yyyy-mm-dd hh:mm]`: UTC date and time with the specified format (time of the 1st profile when several acquired),
  - `flag_qc [NA]`: quality flag. 1 = kept profile ; 0 = discarded profile,
  - `comment [NA]`: field comment,
  - `depth [m]`: depth of the provided data (the grid has been set every 0.01 m),
  - `temperature [C]`: smoothed temperature value in Celsius degrees,
  - `temperature_sdev [C]`: smoothed temperature standard deviation (when several profiles),
  - `temperature_surface_handheld [C]`: temperature measured with the handheld meter at the surface,
  - `salinity [NA]`: smoothed salinity value,
  - `salinity_sdev [NA]`: smoothed salinity standard deviation (when several profiles),
  - `salinity_surface_handheld [NA]`: salinity measured with the handheld meter at the surface.

Other non-final / temporary results have been stored in `./res/temporary_results` and will be explained below.
<br>

### 3  Data processing notes

This section gives details on the successive processing steps that have been applied to the raw data, as well as on the file tree organization.
<br>

##### 3.1  WP4-1 files renaming

The data acquired during the leg 1 have been acquired with a different sensor compared to the data acquired on the subsequent legs (2, 3 and 4). For homogenization purposes (including with all the other operations that generated data files), the names of the WP4-1 files had to be modified.

Also, many WP4-1 files contain several casts, they were therefore split (manually, as no obvious pattern was found!). To do so, they have been copied and pasted as many times as there were casts, and each occurrence has been renamed with the cast number and its content has been manually selected, to include only this cast (a catalog illustrating that is provided, see section 3.5.3 below).

The file naming convention is the following:
`Nunataryuk_WP4-L_stXYZ_Iyyyymmdd_RYYYYMMDD_OPER_VAR-0c.csv`, where:
  - L is the leg ID,
  - XYZ is the station name,
  - yyyymmdd is the date of the initial submission (i.e., the date of the data acquisition),
  - YYYYMMDD is the date of the data last revision (i.e., the date of the last processing),
  - OPER is the operator name,
  - VAR is the variable / sensor / operation code name,
  - c is the cast ID.
(Example: `Nunataryuk_WP4-1_st0a_I20190420_R20191223_BECU_IOP-CTD-01.csv`)

The code file `./files/data/WP4-1/src/01_change_filename_WP41.R` has been used, along with the input file `./files/data/WP4-1/src/filenames_change_log.csv` (that was manually created) to copy the original files into new files with appropriate file names (without changing the files content). Note: they still have an .xlsx extension, which will be changed, see next paragraph. The files with the original file names are located in `./files/data/WP4-1/02_original_filenames_raw/`, while the files with the new file names are located in `./files/data/WP4-1/03_new_filenames_xlsx/`.

The code file `./files/data/WP4-1/src/02_export_WP41_xlsx_to_csv.R` has been used to parse these .xlsx files into .csv files and to store the output files in the directory called `./files/data/WP4-1/04_new_filenames_csv_not_cleaned/`. See section 3.3 for the "cleaning" that can be guessed form the directory name.
<br>

##### 3.2  WP4-2, WP4-3 and WP4-4 files renaming

Again, for homogenization purposes with other data files generated by other field activities, the data files of the last 3 legs had to be renamed.
The code file `./files/data/WP4-234/src/01_change_filename_WP4234.R` has been used along with the (manually edited) input file `./files/data/WP4-234/src/filenames_change_log.csv` to rename all the files contained in `./files/data/WP4-234/02_original_filenames_parsed_and_cleaned/` and store them in `./files/data/WP4-234/03_new_filenames_cleaned/`.

Note that for the leg 1, the cleaning of the data has been performed before the renaming of the files, whereas for the legs 2, 3 and 4, the cleaning has been performed on the original files. The original files of the legs 2, 3 and 4 are located in `./files/data/WP4-234/01_original_filenames_raw/` (see the `README.txt` in that directory).
<br>

##### 3.3  "Shiny" cleaning

As already mentionned above, all the profiles have been cleaned using a so-called "shiny" R app (see https://github.com/GuislainBecu/02.IOP.CLEAN.INPUT.FILES), which allows an interactive mouse-based “zoom and select” point selection on the depth time-series of a loaded profile. This app achieves 2 goals:

  - Remove data points acquired during surface waiting time or wrongly acquired points (on deck etc.),
  - Split a multiple casts data file into several files, containing each a single cast. Actually, the app outputs 2 types of results, the 1st one removes all the lines corresponding to the discarded points, while the 2nd one appends a `flag` parameter as a last and additional column, `TRUE` meaning that the point has been kept, `FALSE` meaning that the point has been discarded. This 2nd option has been used here.

<br>

##### 3.4  Overall input files

The whole set of the homogeneously named files that has been cleaned with the Shiny app (see section 3.3) have been all gathered (i.e., copied and pasted) in `./files/data/all.cleaned.NewFileNames/` for all the legs. The main code (see section 3.5) will use these last files as input files.
<br>

##### 3.5  Processing

All the code files are stored in the `./src/` directory, as well as in its subdirectory `./src/functions/`. The following sub-sections provide details on each step.
<br>

<u>*3.5.1  Files renaming*</u>

See sections 3.1 and 3.2 for the necessary renaming of the files.
<br>

<u>*3.5.2  Depth tare correction*</u>

The code used in this part, namely `./src/01_compute_corr_depth_tare.R`, has been used to compute and apply a depth tare. Indeed, the CTD probes that have been used do not provide a depth tare feature to correct for any recent change in atmospheric pressure. For this Nunataryuk field, as most of the stations display a very shallow water depth, a good pressure measurement is paramount. The probes that were used have a very low depth rating (for a better accuracy, which is usually a percentage of the depth rating), but to improve further the data quality, a correction for this atmospheric pressure change has been performed. To do so, all the "in-air" CTD measurements that were acquired over the span of the whole cruise (sometimes by chance!) have been compared against Environment-Canada ("EC") values recorded in the area (from 4 sites, see (https://climat.meteo.gc.ca/historical_data/search_historic_data_f.html ; each in-air CTD value has been compared to the closest site value at the same date and time). This second code file gathers all the necessary data (from the in situ CTD and the EC archives) and computes a corrective depth value to be applied from a linear regression. The correction is achieved by applying the linear model coefficients (slope and offset) to the atmospheric pressure values found in the EC archive at the date and time of each CTD cast, at the closest site in terms of straight line distance.

Temporary result files illustrating this depth tare estimation are located in `./res/temporary_results/depth_tare_corr/`: `check.tare.files_[1|2|3]` show for each cast which (in air) point has been kept (larger black points) or discarded (smaller grey points) for the comparison with the EC values, `EC_Pres_timeseries.pdf` displays a time-series of the EC values for the 4 selected close-by sites, `Nunataryuk_map.pdf` shows a map of the area, including all the stations and the 4 EC sites, and finally, `comp_pressure_in_air.pdf` shows the comparison and gives the linear model statistics (r$^2$ = 0.9173).
<br>

<u>*3.5.3  Profiles processing (running median and smoothing)*</u>

The code used in this part, namely `./src/02_process_CTD_v1.03.R`, applies the following steps:

  - Applies a running median (over a 5 points wide moving window), to remove any spike,
  - Applies a smoothing function, i.e., a local polynomial regression ("loess"), or, for few profiles, a spline (as the loess was diverging, whatever the selected span). The loess span has been manually set iteratively. The file `./files/metadata-and_log/iop_info.csv` gives the function that has been used (loess / spline) as well as the span (this file also gives the temperature and salinity axis extremes values for the plotting). The main goal of the smoothing is to provide results on a consistent depth grid (set to 0.01 m),
  - Plots a catalog of all the profiles (see `./res/temporary_results/catalog/Nunataryuk_WP4_CTD_catalog.pdf`), including the discarded ones (each plot title, which corresponds to each cast’s file name, is in red font when a profile has been discarded, in black otherwise). For each profile, 4 plots are produced. The 1st one displays a depth time-series, with the kept points in black and the discarded ones in grey (this also applies for the profiles including multiple casts, mostly during the leg 1, but not only). The 2nd and 3rd plots display respectively the temperature and the salinity profiles (the black points are the kept measurement, the purple (for the temperature) and turquoise (for the salinity) points are the running median outputs, the orange points are the smoothed values, and the grey points are the discarded ones). Finally, the 4th plot displays a T = f(S) plot,
  - Outputs individual ASCII files for each cast, providing the smoothed values on the 0.01 m steps depth grid, see files located in `./res/temporary_results/ASCII_initial/`. Note that these files are not the final output, as they need to be averaged (see section 3.5.4).

<br>

<u>*3.5.4  Profiles averaging*</u>

The code used here is `./src/03_average_CTD_profiles.R` and performs the following:

  - Averages the profiles for which multiple casts have been performed. In some cases, 1 cast out of 7 or 8 was obviously an outlier. This code has therefore been run twice to manually select which cast had to be discarded and includes a log of the discarded/kept profiles. Also, temporary plots illustrate this cast selection; See in `./res/temporary_results/profiles_averaging/`: `01_Temp_avg_initial.pdf` displays all the salinity casts, grouped by station and by date, `02_Temp_avg_initial_multiple_casts_profiles.pdf` displays only multiple casts profiles (with annotations telling which casts has been selected to be removed),  and the final versions of these same files (i.e., `03_Temp_avg_final.pdf` and `04_Temp_avg_final_multiple_casts_profiles.pdf`) displays the same casts after the manual selection. 4 other files display the same results for the salinity,
  - Generates a global PDF file that contains all the temperature and salinity profiles on 1 page (`./res/PDF_final/Nunataryuk_WP4_Masterfile_v1-01.pdf`),
  - Generates individual ASCII files (normal form and including metadata) for each station/date set (i.e., multiple casts that have been kept have been averaged), see `./res/ASCII_final/individual_files/`,
  - Generates a unique ASCII Master file (normal form and including metadata also), see `./res/ASCII_final/unique_Masterfile/Nunataryuk_WP4_Masterfile_v1-01.csv`,
  - Generates a metadata file, giving all details on each station/date file, see `./res/ASCII_final/Nunataryuk_WP4_Metadata_v1-01.csv`.
<br>

##### 3.6  Additional notes

During the processing, it was obvious that the upward casts were dragging water masses from the bottom of the water column toward the surface (see for example page 46 of the catalog (profiles #91 and #92 of `./res/temporary_results/catalog/Nunataryuk_WP4_CTD_catalog.pdf`). All the upward profiles have then been discarded (this is anyway consistent with the IOP package user's guide).

For some station/leg/date set, no IOP based CTD profiles were available (e.g., the sensor was obstructed with ice chunks). For these casts, and whenever available, data from a hand held CTD device were used and these data have been included in the results files provided here (manually, see in the code `./src/03_average_CTD_profiles.R`).

Guislain.Becu@takuvik.ulaval.ca

27 Jan. 2022

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
