These files are the raw one (from the directory called "01_original_filenames_raw") that have been parsed with the dedicated WetLabs software ("WAP"), to display the CTD fields of interest.

These files have also been through a cleaning "Shiny app" (https://github.com/GuislainBecu/02.IOP.CLEAN.INPUT.FILES).

This cleaning includes:
1) a removal of all the useless points (waiting/warming-up time on deck, wrongly started profiles, etc.)
2) a multiple-casts splitting tool: whenever a data file contains more than 1 cast (mostly happened during the leg 4-1, but not only), the app has been used to manually select each cast part among the several copies of the unique original file. e.g., if the file "file1.csv" contains 3 downward casts, it is copied 3 times with 3 different names (like "file1-d01.csv", "file1-d02.csv" and "file1-d03.csv"). Inside each of these identical files, the app allows to manually select a subpart corresponding to the actual individual casts. A "flag" column is appended as a last column in each file, the value being "TRUE" for the manually selected point, "FALSE" otherwise.