# Take the data from the YOSE understory and convert the species observations into a long-form data frame, with one row per location-species combination. 

# This function pulls out all the species and "association" observations and makes them into a single, long-form data frame. 
# This data frame could then be filtered if we just want the pin-location data, not the associations, or if there are some categories we want to remove (litter, rock etc.)

library(tidyverse)

# set working directory to project dir 
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances")

# Use one random plot as an example
plot60_data = read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/YPE_Understory_Plot_60.csv")
head(plot60_data)

# This function will take one of the plot-level data frames and turn it into a 4-column long-format data frame that contains the plot ID in column 1, the dOut_m pin location in column 2, whether the species was at the pin location (pin) or associated with it (assoc) and each species observation in column 4.

# To do this, we need 2 things: 1) count how many species are present at each location; 2) take all the species observations across all locations, get rid of the NAs and turn them into a long vector. 

convert_understory_data_to_long <- function(understory_data) {
  require(dplyr)
  # Get the relevant columns of the data frame, assuming the format is as is normal for the understory data frames (species1, species2, . . ., assoc1, assoc2, ...)
  understory_species <- dplyr::select(understory_data, matches("species"))
  understory_assoc <- dplyr::select(understory_data, matches("assoc"))
  
  # Count the species present at pin location and association in each row 
  n_species_by_row <- apply(understory_species, 1, f <- function(x) {return(length(na.omit(x)))}) 
  n_assoc_by_row <- apply(understory_assoc, 1, f <- function(x) {return(length(na.omit(x)))}) 
  
  # Turn the species and association info into long vectors with no NAs 
  species_vector <- unlist(apply(understory_species, 1, na.omit))
  assoc_vector <- unlist(apply(understory_assoc, 1, na.omit))
  n_species <- length(species_vector)
  n_assoc <- length(assoc_vector)
  total_number_of_obs <- n_species + n_assoc
  
  # Combine all the info into a long-form data frame with columns for plot id, location id, an indicator for "pin" vs "assoc", plus the long vectors of species and association ids in the last column. 
  species_understory_long <- data.frame(plotID = rep(understory_data$plotID[1], total_number_of_obs), dOut_m = c(rep(understory_data$dOut_m, n_species_by_row), rep(understory_data$dOut_m, n_assoc_by_row)), pin_vs_assoc = c(rep("pin", n_species), rep("assoc", n_assoc)), species = c(species_vector, assoc_vector))
  
  return(species_understory_long) 
} ## End function

# Test the function on our plot data 
testrun <- convert_understory_data_to_long(understory_data)
head(testrun)
dim(testrun)

# Get species richness for plot, including both pin touches and associations
testrun %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(species) %>%
  unique() %>% count() # count provides richness as a number, unique the full list

# bring in the rest of the data with the Understory read code
# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

# Load initial packages
library(readxl)

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# Initialize an empty list to store data for each plot
understory_list <- list()

# Loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "Understory", full.names = TRUE) # Understory data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    data <- read_excel(file, na = "NA")
    
    # Extract the plot number from the file name
    plot_number <- tools::file_path_sans_ext(basename(file))
    
    # Store the data along with plot number in the list
    understory_list[[plot_number]] <- data
  }
}
# We can "lapply()" this function to the list of data frames from all the plots. 
all_plots_understory <- lapply(understory_list, convert_understory_data_to_long)

# stick all dataframes in understory list into one df 
all_plots_understory <- do.call(rbind, all_plots_understory)

# save as a csv in case we want to start with this later
write.csv(all_plots_understory, "UnderstoryDataLong.csv") 

# Try to use test code for species richness for all plots
species_richness <- all_plots_understory %>% group_by(plotID) %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(plotID, species) %>%
  unique() %>% count()
class(species_richness) # grouped dataframe--ok?
head(species_richness)
# appears to work, but two plots dropped out--check if they should be zero
# add in missing plots with a zero or modify the filter statement

# Make the species richness into a function and apply to all plots--Don't need because of group by above
species_richness_fun <- function(data_source) {
  require(dplyr)
  species_richness <- data_source %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(plotID, species) %>%
  unique() %>% count()
  return(species_richness)
  }# count provides richness as a number, unique the full list

richness <- lapply(understory_list, species_richness_fun)
class(richness)
head(richness)
rich <- unlist(richness)
class(rich)

# This is getting close, but the order is weird should have added 01, 02 etc. for single digit plots
# Next goal is to bind the plot-level species richness numbers with other plot data, so the richness list needs to be in the right order or maybe better to join on plotID
# bring in saved plot level data (or all tree level data with plot data)
# combined_data <- cbind(plot_data, rich)

unique(all_plots_understory$species)
