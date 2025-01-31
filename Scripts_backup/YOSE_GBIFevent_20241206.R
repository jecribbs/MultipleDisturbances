# GBIF: Event Tab
# Authors: Jenny Cribbs
# Date: 09 September 2024

# Overall Input: Read in csv files for plot-level data, fire severity data, and PRISM data
# Overall Output: Clean csv with a row for each plot and columns matching GBIF columns, so the output can be pasted into NPS template Event tab

library(tidyverse)

# read in clean plot data
plotData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/PlotLevelData.csv")

# read in spatial data
ypeCoords <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/YPE_plot_coordinates.csv")

# bring in merged ground and MTBS fire severity data
fireData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/fireCompare_22052024.csv")

# bring in estimated effort
effortData <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/dataSandbox/CleanData/estimatedEffort.csv") %>% 
  rename(plotID = "YPE.Plot.ID", effort = "Estimated.Effort..in.hrs.",
         startDateTime = "Start.Date.time",
         endDateTime = "End.Date.time") %>% 
  select(plotID, effort, startDateTime, endDateTime)

# join plot data to fire data
plotFire <- left_join(plotData, fireData)
# add estimated effort data
combinedData <- left_join(plotFire, effortData)
# join coordinates from SpatialDataWorkflow.R
combinedData <- left_join(combinedData, ypeCoords)

# rename and add columns to match template
eventData <- combinedData %>% rename(eventID = plotID, 
                                     samplingProtocol = plot_type,
                                     samplingEffort = effort,
                                     verbatimLatitudeStart = X,
                                     verbatimLongitudeStart = Y,
                                     verbatimLatitudeStop = plot_end_UTM_N,
                                     verbatimLongitudeStop = plot_end_UTM_E,
                                     eventRemarks = plot_notes,
                                     county = NAME) %>% 
  mutate(parentEventID = "", 
         sampleSizeValue = trans_length * width_pila, 
         sampleSizeUnit = "square meters",
         waterFeatureName = "",
         countryCode = "US",
         stateProvince = "California",
         habitatDescription = "mixed-conifer forest",
         verbatimCoordinateSystem = "decimal degrees",
         verbatimSRS = "EPSG:4326", # 26910 for zone 10 plots or 4326 for lat long for all
         verbatimDatum = "WGS84",# WGS84 for lat/long
         coordinateUncertaintyinMeters = "", # maybe add GPS field uncertainty with metadata notes 
         coordinatePrecision = "", # this may be a feature of the GPS units
         publicDisplay = "yes",
         dataAccess = "unrestricted",
         fieldNumber = "",
         fieldNotes = "",
         verbatimElevation = (plot_elevation_ft * 3.28084), # convert to meters
         verbatimDepth = "",
         waterFeatureSubstrate = "",
         locationRemarks = "",
         footprintWKT = "",
         footprintSRS = "", 
         verbatimLatitude = verbatimLatitudeStart,
         verbatimLongitude = verbatimLongitudeStart,
         azimuth = plot_azimuth + 12.5,
         aspect = aspect + 12.5)

# select columns for NPS data product
eventExcelData <- eventData %>% select (eventID, parentEventID, samplingProtocol, sampleSizeValue, sampleSizeUnit, samplingEffort, date, startDateTime, endDateTime, waterFeatureName, countryCode, stateProvince, county, habitatDescription, verbatimLatitude, verbatimLongitude, verbatimLatitudeStart, verbatimLongitudeStart, verbatimLatitudeStop, verbatimLongitudeStop, verbatimCoordinateSystem, verbatimSRS, verbatimDatum, coordinateUncertaintyinMeters, coordinatePrecision, publicDisplay, dataAccess, fieldNumber, fieldNotes, eventRemarks, verbatimElevation, verbatimDepth, waterFeatureSubstrate, locationRemarks, footprintWKT, footprintSRS, slope, aspect, azimuth)

# cleanup columns to match report and NPS data product format conventions
eventExcelData <- eventExcelData %>% 
  mutate(
    samplingEffort = paste(samplingEffort, "hours"), # add units for effort
    startDateTime = ifelse(is.na(startDateTime) | startDateTime == "", paste(date, "0:00"),  startDateTime), # add 00:00 for unknown times
    endDateTime = ifelse(is.na(endDateTime) | endDateTime == "", paste(date, "0:00"), endDateTime), # add 00:00 for unknown times
    samplingProtocol = case_when(samplingProtocol == "highseverity" ~ "fire protocol",
                                 samplingProtocol == "random" ~ "primary protocol")
)

write.csv(eventExcelData, "gbifEvent.csv")

# Spatial data wrangling to fix up for December deadline
# For now, changing 2 zone 10 plots by hand in Excel sheet (YPE32, YPE50) to zone 10
# Changed plots 10 and 43 to Madera county by hand
# Changed Tuolume County plots based on eyeballing county line: 3 plots near Smith Mountain (199-201, 208-209, 202-206), 3 near Hetch Hetchy (1281-1301, 0941, 881-931), Cathedral Creek, Mount Hooper, 2 near Lake Eleanor (247-248, 137-138), 266-264 (plot44?), 0091-0092, 01751-01761 (return of the saplings?), 0093-0094, 0095-0096, 01871-01861, North Crane Creek, and Tuolume Grove, bald mountain YPE 5, Aspen Valley Area (0255-0254, 01311-01321, Yosemite Wilderness, need to be changed to Tuolume county