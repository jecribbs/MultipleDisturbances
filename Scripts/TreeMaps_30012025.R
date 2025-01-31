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

# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/RawData/YPE_Data"
#datadir <- "/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/Data/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
pila_list <- data.frame()

# loop through each folder
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
                    plot_beg_UTM_N, plot_beg_UTM_E, 
                    plot_end_UTM_N, plot_end_UTM_E, 
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
    pila_list <- rbind(pila_list, xlsfile)
    
  }
}

# check data types
summary(pila_list)

# bring in kml files from gps units
kml_66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml") %>% 
  mutate(Source = "66i",
         kml_id = paste(Source, Name))
kml_66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml") %>% 
  mutate(Source = "66sr", 
         kml_id = paste(Source, Name))
# create one kml from both units
kml_sf <- rbind(kml_66i, kml_66sr)
st_crs(kml_sf) # also WGS84

# remove rows with name Coordinates
Coordinates = filter(kml_sf, Name == "Coordinates")
kml_sf = filter(kml_sf, Name != "Coordinates")

# replace geometry column of kml with lat and long
kml = bind_cols(st_drop_geometry(kml_sf), st_coordinates(kml_sf))

# join pila and kml data
test <- left_join(pila_list, kml, by = c("PILA_waypoint" = "Name", "GPSdevice" ="Source"))

                  