# Plot and Tree Maps
# 26 January 2025
# Author: Jenny Cribbs
# Inputs: list of utms entered for each plot and utms and xy coordinates for trees.
# Code Description: (1) spatial data wrangling to use plot beginning waypoints in utms to calculate plot end waypoints in utms based on azimuth and transect length in meters (2) visualize points (3) choose calculated or gps waypoint to get one end point per plot (4) visualize result (5) convert all points to decimal degrees. (6) calculate utms for associated trees based on dOut and dSide (7) calculate utms for pila based on dOut and dSide (8) map all trees and pila (from gps and xy)
# Output: (1) a csv file with lat/long coordinates for each plot beginning and end point. (2) a csv file with lat/long coordinates for each tree.

library(tidyverse)
library(terra)
library(tigris)
library(tidycensus)
library(rmapshaper)
library(sf)
library(tmap)

## Part 1: Calculate plot ends

# read in clean plot data

#plotData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/PlotLevelData.csv")
plotData <- read.csv("C:/Users/tazli/Downloads/YOSE_SugarPine/DataClean/PlotLevelData.csv")

plotData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/PlotLevelData.csv")

# read in tree data
#treeData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/YOSE_cleanTreeList.csv")
treeData <- read.csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/YOSE_cleanTreeList.csv")
# read in pila data
#pilaData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/YOSE_cleanPILAdata.csv")
pilaData <- read.csv("C:/Users/tazli/Downloads/YOSE_SugarPine/MultipleDisturbances/YOSE_cleanPILAdata.csv")

# plot end point calculation 
# Convert field azimuth from magnetic to true and degrees to radians
plotData <- plotData %>% mutate(azimuth_rad = (plot_azimuth + 12.5) * pi / 180)

# Calculate the easting and northing offsets
plotData <- plotData %>% mutate(delta_easting = trans_length * sin(azimuth_rad), delta_northing = trans_length * cos(azimuth_rad))

# Calculate the ending UTM coordinates
plotData <- plotData %>% mutate(end_easting = plot_beg_UTM_E + delta_easting, end_northing = plot_beg_UTM_N + delta_northing)

# Create spatial points and convert to lat/long (WGS84)
# plot beginnings
plotBeg_sf <- plotData %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })
# plot ends (calculated)
plotEnds_sf <- plotData %>% 
  group_split(UTM_zone) %>% 
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("end_easting", "end_northing"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })
# plot ends (gps)
plotEndsGPS_sf <- plotData %>% 
  filter(!is.na(plot_end_UTM_N) & !is.na(plot_end_UTM_E)) %>%
  group_split(UTM_zone) %>% 
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("plot_end_UTM_E", "plot_end_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# Part 3: Choose calculated or field gps coordinates for end points 
# Manual review in progress


# Part 4: Look at result


# Need to flag trees as GPS trees or xy trees
# Need to label plots as transect, balloon, combination or weird 
# Then can plot all trees


# Plotting Trees ~~~~~~~~~~~~~~~~~~~~
# Tree and PILA data have different numbers of columns
# Join each separately to plot data to map as separate spatial objects
# start with trees since they are the simplest case

# join tree data with plotData
treeData <- left_join(treeData, select(plotData, -date, -crew), by = "plotID")
# need to check plot 74 tree 13--appears to have no position because dOut is NA--can this be estimated or does it need to be discarded
# Remove plot74, tree13 for now
#tree_positions <- tree_positions %>% filter(!is.na(tree_UTM_E) & !is.na(tree_UTM_N))
# dOut was missed in the field, but neighboring trees are 1m apart (48.2 and 49.2 dOut), so a dOut estimate of 48.7 seems more reasonable than deleting the full record
# impute 48.7 as best guess for the missing dOut value
treeData <- treeData %>% 
  mutate(dOut_m = if_else(plotID == 74 & treeNum == 13, 48.7, dOut_m))

# Create a function to calculate tree positions based on the plot beg and x,y
calculate_tree_positions <- function(data) {
  data %>%
    mutate(
      hypotenuse = sqrt(dOut_m^2 + dSide^2),  # Step 1: Calculate hypotenuse
      angle_dSide = atan2(dSide, dOut_m),     # Step 2: Calculate angle opposite Dside
      azimuth_rad = (plot_azimuth + 12.5)  * pi / 180,         # Convert azimuth from magnetic to true and degrees to radians
      adjusted_angle = azimuth_rad + angle_dSide, # Step 3: Adjust azimuth by angle_Dside
      x_offset = hypotenuse * sin(adjusted_angle), # Step 4: Calculate x offset
      y_offset = hypotenuse * cos(adjusted_angle), # Step 4: Calculate y offset
      tree_UTM_E = plot_beg_UTM_E + x_offset,  # Step 5: Final easting
      tree_UTM_N = plot_beg_UTM_N + y_offset   # Step 5: Final northing
    )
}

# Calculate tree positions
tree_positions <- calculate_tree_positions(treeData)

# Create spatial points for trees and convert to lat/long (WGS84)
tree_points <- tree_positions %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("tree_UTM_E", "tree_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# find non-numeric dOut values
# 271 NAs + 3 trees in plot 75 with relative positions
# NAs include pilas with estimated position and gps position
pilaData %>%
  filter(!grepl("^-?[0-9.]+$", dOut_m)) %>%
  select(occurrenceID, dOut_m) %>%
  distinct() %>%
  print()

# Change dOut_m to numeric
pilaData$dOut_m <- as.numeric(pilaData$dOut_m)

# combine dSideR_m and dSideL_m into a single dSide column with checks
pilaData <- pilaData %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(pilaData)

# Decide which join works best then continue plotting pila
# join pila data with plot data
pilaData <- left_join(select(pilaData, -plot_beg_UTM_N, -plot_beg_UTM_E, -plot_end_UTM_N, -plot_end_UTM_E), plotData, by = c("eventID" = "plotID"))

# Use the same function to calculate UTMs for pila
pila_positions <- calculate_tree_positions(pilaData)

# filter out records with missing positions (1066 left)
pila_positions_trimmed <- pila_positions %>% filter(!is.na(tree_UTM_E) & !is.na(tree_UTM_N))

# Create spatial points for pila and convert to lat/long (WGS84)
pila_points_xy <- pila_positions_trimmed %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("tree_UTM_E", "tree_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# Coerce UTM columns to numeric
pilaData <- pilaData %>%
  mutate(
    PILA_UTM_E = as.numeric(PILA_UTM_E),
    PILA_UTM_N = as.numeric(PILA_UTM_N)
  )
# remove NAs (only 193 remain)
pilaData_trimmed <- pilaData %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))
# check that no missing values remain
pilaData_trimmed %>%
  summarise(
    missing_e = sum(is.na(PILA_UTM_E)),
    missing_n = sum(is.na(PILA_UTM_N))
  )

# Create spatial points for gps pila and convert to lat/long (WGS84)
pila_points_gps <- pilaData_trimmed %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("PILA_UTM_E", "PILA_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })




# remove NAs to isolate estimated locations only (29)
pila_points_estimated <- pilaData %>%
  filter(!is.na(est_dOut_m) | !is.na(est_dSideL) | !is.na(est_dSideR))
select(pila_points_estimated, occurrenceID, est_dOut_m, est_dSideL, est_dSideR, dOut_m, dSide)
# impute positions for tree with relative position to WP29

# Coerce estimated positions to numeric
pila_points_estimated <- pila_points_estimated %>%
  mutate(
    est_dOut_m = as.numeric(est_dOut_m),
    est_dSideL = as.numeric(est_dSideL)
  )

# check that no missing values remain
pila_points_estimated %>%
  summarise(
    missing_e = sum(is.na(est_dOut_m)),
    missing_n = sum(is.na(est_dSideL))
  )
# add xy coordinates for E58-PILA16 dOut_m = 63.5, dSide = -47
filter(pila_points_estimated, eventID == "E58-PILA16")

# run calculate points function
pila_points_estimated <- calculate_tree_positions(pila_points_estimated)

# Adapt function to calculate tree positions based on point 345 (plot 75, tree 8)
relativeTreeCalculation <- function(start_E, start_N, bearing, distance) {
  hypotenuse = distance  # Calculate hypotenuse
# Convert azimuth from magnetic to true and degrees to radians
bearing_rad = (bearing + 12.5)  * pi / 180 
x_offset = hypotenuse * sin(bearing_rad) # Calculate x offset
y_offset = hypotenuse * cos(bearing_rad) # Calculate y offset
tree_UTM_E = start_E + x_offset  # Final easting
tree_UTM_N = start_N + y_offset   # Final northing
print(list(tree_UTM_E = tree_UTM_E, tree_UTM_N = tree_UTM_N))
} 


# Create spatial points for gps pila and convert to lat/long (WGS84)
pila_points_estimated <- pila_points_estimated %>%
  group_split(UTM_zone) %>%
  map_dfr(function(df) {
    st_as_sf(df, 
             coords = c("tree_UTM_E", "tree_UTM_N"), 
             crs = paste0("+proj=utm +zone=", unique(df$UTM_zone), " +datum=NAD83")) %>%
      st_transform(crs = 4326)  # Convert to lat/long
  })

# Part 2: Visualize the Plot Points

# Bring in the NPS boundary for YOSE
nps <- st_read("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/nps_boundary") %>% filter(UNIT_CODE == "YOSE")
st_crs(nps) # 6269
nps <- st_transform(nps, crs = 4326)
# Bring in county boundaries
counties <- st_read("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/tl_2024_us_county/tl_2024_us_county.shp") %>% filter(NAME == "Mariposa" | NAME == "Madera" | NAME == "Tuolumne")
# Reproject county boundaries to lat/long
counties <- st_transform(counties, crs = 4326)
# Read KML files to bring in gps points
kml_66i <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66i (Unit ID 3404379582).kml") %>% 
  mutate(Source = "66i",
         kml_id = paste(Source, Name))
kml_66sr <- st_read("/Users/jennifercribbs/Documents/YOSE/Waypoints/Recently Read from GPSMAP 66sr (Unit ID 3377332670).kml") %>% 
  mutate(Source = "66sr", 
         kml_id = paste(Source, Name))
# create one kml from both units
kml <- rbind(kml_66i, kml_66sr)
st_crs(kml) # also WGS84

# Map pila and non-pila trees with plot points and gps points
tmap_mode("view")
tm_shape(nps) +
  tm_polygons(col = "gray",
              title = "Yosemite") +
  tm_shape(kml) +
  tm_dots(fill = "black") +
  tm_shape(pila_points_gps) +
  tm_dots(fill = "magenta", popup.vars = "occurrenceID") +
  tm_shape(pila_points_xy) +
  tm_dots(fill = "purple", popup.vars = "occurrenceID") +
  tm_shape(tree_points) +
  tm_dots( fill = "darkgreen", popup.vars = c("treeNum", "species")) +
  tm_shape(plotBeg_sf) +
  tm_dots(fill = "green", popup.vars = "plotID") +
  tm_shape(plotEnds_sf)+
  tm_dots(fill = "red", popup.vars = "plotID") +
  tm_shape(plotEndsGPS_sf) +
  tm_dots(fill = "pink", popup.vars = "plotID") 
  
  

# Come up with an algorithm or review strategy for duplicate trees with gps and xy coordinates

# Last part: adding columns for NPS data products
# ggplot version use at the end with county designation code
ggplot() +
  geom_sf(data = nps, fill = "transparent", color = "darkgreen", lwd = 1) + # park boundary
  geom_sf(data = counties, fill = "transparent", color = "blue", lwd = 1) + # county boundaries boudaries
  geom_sf(data = YPEplots, color = "green", size = 2) + # converted points
  #geom_sf(data = YPEtrees, color = "green", size = 1) +
  labs(
    title = "Spatial Data Overlay",
    subtitle = "Park Boundary, YPE Points, and individual trees",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# use a spatial join to assign a county name to each point
#plots_with_county <- st_join(YPEplots, counties, join = st_intersects) %>% select(plotID, UTM_zone, geometry, NAME)

# Determine county of each plot
# Extract coordinates and keep the plotID column
#ype_coords <- plots_with_county %>%
  #st_coordinates() %>%
  #as.data.frame() %>%
  #bind_cols(plots_with_county %>% select(plotID, NAME))  # Keep the plotID column


# Write out final spatial data
#write_csv(ype_coords, "YPE_plot_coordinates.csv")

st_write(your_sf_object, "your_data.kml", driver = "KML")
st_write(your_sf_object, "your_data.shp", driver = "ESRI Shapefile")

