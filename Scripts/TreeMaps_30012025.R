# Tree Map 
# 30 January 2025
# Author: Jenny Cribbs
# Inputs: plot data from the event ID script, tree data and plot data read in from the Google Sheets
# Code Description: (1) spatial data wrangling to use plot beginning waypoints in utms to calculate plot end waypoints in utms based on azimuth and transect length in meters (2) visualize points (3) choose calculated or gps waypoint to get one end point per plot (4) visualize result (5) convert all points to decimal degrees. (6) calculate utms for associated trees based on dOut and dSide (7) calculate utms for pila based on dOut and dSide (8) map all trees and pila (from gps and xy)
# Output: (1) a csv file with lat/long coordinates for each plot beginning and end point. (2) a csv file with lat/long coordinates for each tree.

library(tidyverse)
library(terra)
library(tigris)
library(tidycensus)
library(rmapshaper)
library(sf)
library(tmap)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")
#setwd("/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances")

# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/RawData/YPE_Data"
#datadir <- "/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/Data/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty dataframe 
pilas <- data.frame()

# loop through each folder corresponding to each plot
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "PILA") # choose PILA data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
      dplyr::select(plotID, plot_type, date, crew, trans_length, width, 
                    slope, aspect, plot_azimuth,
                    plot_beg_UTM_N, plot_beg_UTM_E, waypoint_beg,
                    plot_end_UTM_N, plot_end_UTM_E, waypoint_end,
                    plot_notes, plot_elevation_ft, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    seedlings_50m, seedlings_100m, seedlings_150m, seedlings_200m,
                    treeNum, species, 
                    DBH_cm, est_DBH_cm, height_m, est_height_m,
                    pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker, 
                    flags, DTOP,
                    percentLive, fire_scar, resistance, 
                    damageCodes, notes,
                    PILA_UTM_E, PILA_UTM_N, 
                    GPSdevice, PILA_waypoint, estimatedAccuracy_PILA_ft,
                    dOut_m, dSideR_m, dSideL_m,
                    est_dOut_m, est_dSideR, est_dSideL
      )
    pilas <- rbind(pilas, xlsfile)
    
  }
}
# check data types
summary(pilas)

# create a unique occurrenceID (for PILAs) 
pilas <- pilas %>% 
  mutate(occurrenceID = paste(occurrenceID = paste("E", plotID, "-", "PILA", treeNum, sep = "")))
# create a unique kml_id for waypoints
pilas <- pilas %>% mutate(kml_id = paste(GPSdevice, PILA_waypoint))

# remove row with no data except "bearing above" dSideR column
pilas <- pilas %>% filter(!is.na(plotID))

# Calculated relative tree position for plot 75 tree 19 in TreeMaps2.R
# impute data to replace notes with calculated utms
pilas <- pilas %>%
  mutate(
    dOut_m = ifelse(occurrenceID == "E75-PILA19", NA, dOut_m),
    dSideR_m = ifelse(occurrenceID == "E75-PILA19", NA, dSideR_m),
    PILA_UTM_E = ifelse(occurrenceID == "E75-PILA19", 283333, PILA_UTM_E),
    PILA_UTM_N = ifelse(occurrenceID == "E75-PILA19", 4201923, PILA_UTM_N)                )   
# trees 16 and 17 have the following notes 
#[895] "30m down and towards trail from tree 10"
#[896] "25m up and 10m towards stand"  
# Imputation would require eyeballing a point in QGIS or other heroics
# Will coerce these to numeric with the resulting NAs and fill in later if possible
# Change dOut_m to numeric
pilas$dOut_m <- as.numeric(pilas$dOut_m)
# convert dSide Right to numeric (no NAs after imputation above)
pilas$dSideR_m <- as.numeric(pilas$dSideR_m)
# combine dSideR_m and dSideL_m into a single dSide column with checks
pilas <- pilas %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))

summary(pilas)
# Why is PILA_UTM_E character?
# Probably because of preserving the leading zeros in Excel
# Coerce PILA_UTM_E to numeric (no NAs added)
pilas$PILA_UTM_E <- as.numeric(pilas$PILA_UTM_E)

# bring in kml files from gps units
kml_66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml") %>% 
  mutate(Source = "66i",
         kml_id = paste(Source, Name))
kml_66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml") %>% 
  mutate(Source = "66sr", 
         kml_id = paste(Source, Name))
# create one kml from both units
kml_sf <- rbind(kml_66i, kml_66sr)
st_crs(kml_sf) # already lat/long WGS84 decimal degrees

# remove rows with name Coordinates (not from YOSE project and no Z axis)
filter(kml_sf, Name == "Coordinates")
kml_sf = filter(kml_sf, Name != "Coordinates")

# replace geometry column of kml with lat and long
kml = bind_cols(st_drop_geometry(kml_sf), st_coordinates(kml_sf))

# join pila and kml data
# not joining correctly--mostly NAs
#test <- left_join(pilas, kml, by = c("PILA_waypoint" = "Name", "GPSdevice" ="Source")) %>% select (plotID, treeNum, kml_id, X, Y, Z)
test2 <- left_join(pilas, kml, by = "kml_id") %>% select (plotID, treeNum, kml_id, X, Y, Z)
# lots of NAs check the kml_ids
table(pilas$kml_id %in% kml$kml_id)
table(kml$kml_id %in% pilas$kml_id)
# Convert both to lowercase and trim any whitespace
pilas$GPSdevice <- trimws(tolower(pilas$GPSdevice))
kml$Source <- trimws(tolower(kml$Source))
i <- filter(pilas, GPSdevice == "66i")
sr <- filter(pilas, GPSdevice == "66sr")
si <- filter(pilas, GPSdevice == "66si")
si_check <- si %>% select(occurrenceID, GPSdevice, dOut_m, dSide, PILA_waypoint, PILA_UTM_E, PILA_UTM_N, crew, date, plotID)
# 30 points were taken on the inreach that was returned to UCD
# fortunately they are entered as utms, so hopefully no errors 
# We will not be able increase precision by using the device directly, but shouldn't be a big deal
inreach <- filter(pilas, GPSdevice == "inreach")

# Perform a left join
joined_data <- left_join(pilas, gps_data, by = "occurrenceID")

# Normalize waypoint names to ensure they have the same length
# Remove any rows where the waypoint name contains letters

# This keeps only numeric waypoint names
kml_filtered <- kml[!grepl("[a-zA-Z]", kml$Name), ]

# Normalize the 66i waypoints (5 digits)
kml_filtered$Name[gsub("^", "0", kml_filtered$Name, fixed = TRUE)] <- 
  formatC(as.numeric(kml_filtered$Name), width = 5, flag = "0")
pilas$GPSdevice[gsub("^", "0", pilas$GPSdevice, fixed = TRUE)] <- format(as.numeric(pilas$GPSdevice), width = 5, flag = "0")

# Normalize the 66sr waypoints (4 digits)
kml_filtered$Name[gsub("^", "0", kml_filtered$Name, fixed = TRUE)] <- 
  formatC(as.numeric(kml_filtered$Name), width = 4, flag = "0")
pilas$GPSdevice[gsub("^", "0", pilas$GPSdevice, fixed = TRUE)] <- format(as.numeric(pilas$GPSdevice), width = 4, flag = "0")

# Step 2: Normalize based on the device source
# For 66i: Ensure 5 digits
pilas_clean$PILA_waypoint[pilas_clean$source == "66i"] <- 
  formatC(as.numeric(pilas_clean$PILA_waypoint[pilas_clean$source == "66i"]), width = 5, flag = "0")

# For 66sr: Ensure 4 digits
pilas_clean$PILA_waypoint[pilas_clean$source == "66sr"] <- 
  formatC(as.numeric(pilas_clean$PILA_waypoint[pilas_clean$source == "66sr"]), width = 4, flag = "0")

# Step 3: Merge with the kml data using normalized waypoints
merged_data <- merge(pilas_clean, gps_data, by.x = "PILA_waypoint", by.y = "Name", all.x = TRUE)

# Now, you can merge based on the cleaned, normalized waypoint names
joined_data <- merge(pilas, gps_data_filtered, by.x = "PILA_waypoint", by.y = "Name", all.x = TRUE)


# Assuming 'PILA_waypoint' is in pilas and 'Name' is in gps_data
pilas$normalized_waypoint <- sprintf("%05d", pilas$PILA_waypoint)  # Adjust length as necessary
gps_data$normalized_name <- sprintf("%05d", gps_data$Name)

# Now try the join using the normalized names
joined_data <- merge(pilas, gps_data, by.x = "normalized_waypoint", by.y = "normalized_name", all.x = TRUE)

# Replace UTM coordinates with GPS coordinates where available
joined_data$utm_x <- ifelse(!is.na(joined_data$gps_utm_x), joined_data$gps_utm_x, joined_data$utm_x)
joined_data$utm_y <- ifelse(!is.na(joined_data$gps_utm_y), joined_data$gps_utm_y, joined_data$utm_y)


test_subset <- filter(pilas, !is.na(PILA_waypoint)) %>% select(plotID, treeNum, waypoint_beg, waypoint_end, PILA_waypoint, dOut_m)


                  