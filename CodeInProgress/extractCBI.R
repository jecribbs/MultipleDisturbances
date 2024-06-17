# Script to extract continuous fire severity metrics. 
# It loads plot gps locations as well as the Rim and Ferguson fire severity rasters, reprojects tht plot locations, then extracts the severity data. 

library(sf)
library(tidyverse)
library(terra)


# Load fire severity rasters 
rim_sev <- rast("./Data/Rim_2013-CASTF-002857_CBI_bc_v1.0_2013_extended.tif")
st_crs(rim_sev)
ferguson_sev <- rast("./Data/FERGUSON_2018-CASNF-000745_CBI_bc_v1.0_2018_extended.tif")
plot(ferguson_sev)

# merge fire rasters 
fire_sev <- merge(rim_sev, ferguson_sev)
plot(fire_sev)

# Load the point data
yose_pts <- read.csv("./Data/Plot_beg_UTMs_66sr22.csv", header = TRUE) 
head(yose_pts) 
yose_pts_11 <- dplyr::filter(yose_pts, utm_zone == 11) |>
  st_as_sf(coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = 26911)
yose_pts_10 <- dplyr::filter(yose_pts, utm_zone == 10) |>
  st_as_sf(coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = 26910)

# reproject points to raster crs
yose_pts_11 <- st_transform(yose_pts_11, crs = crs(rim_sev))
yose_pts_10 <- st_transform(yose_pts_10, crs = crs(rim_sev))
yose_pts_sf <- rbind(yose_pts_11, yose_pts_10)

# Plot the data to check 
plot(fire_sev)
points(yose_pts_sf, pch = 19, col = "red")

# Extract the values from the raster
pred_CBI <- extract(fire_sev, yose_pts_sf, method = "bilinear")
plot_CBI_data <- cbind(yose_pts, pred_CBI)
names(plot_CBI_data)[7] <- "predicted_CBI"
plot_CBI_data <- select(plot_CBI_data, -ID)
head(plot_CBI_data)
hist(plot_CBI_data$predicted_CBI)

# Save the data
write.csv(plot_CBI_data, "./Data/plot_CBI_data.csv", row.names = FALSE)
