# Take the data from the YOSE understory and convert the species observations into a long-form data frame, with one row per location-species combination. 

# This function pulls out all the species and "association" observations and makes them into a single, long-form data frame. This data frame could then be filtered if we just want the pin-location data, not the associations, or if there are some categories we want to remove (litter, rock etc.)

# We could "lapply()" this function to the list of data frames from all the plots. 

library(tidyverse)

# Use one random plot as an example
plot_data = read.csv("./Data/YPE_Understory_Plot_60 - Sheet1.csv")
head(plot_data)

# Test the function on our plot data 
testrun <- convert_plot_data_to_long(plot_data)
head(testrun)
dim(testrun)

# Get species richness for plot, including both pin touches and associations
testrun %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(species) %>%
  unique()
  



# This function will take one of the plot-level data frames and turn it into a 4-column long-format data frame that contains the plot ID in column 1, the dOut_m pin location in column 2, whether the species was at the pin location (pin) or associated with it (assoc) and each species observation in column 4.

# To do this, we need 2 things: 1) count how many species are present at each location; 2) take all the species observations across all locations, get rid of the NAs and turn them into a long vector. 

convert_plot_data_to_long <- function(plot_data) {
  require(dplyr)
  # Get the relevant columns of the data frame, assuming the format is as is normal for the understory data frames (species1, species2, . . ., assoc1, assoc2, ...)
  plot_data_species <- dplyr::select(plot_data, matches("species"))
  plot_data_assoc <- dplyr::select(plot_data, matches("assoc"))
  
  # Count the species present at pin location and association in each row 
  n_species_by_row <- apply(plot_data_species, 1, f <- function(x) {return(length(na.omit(x)))}) 
  n_assoc_by_row <- apply(plot_data_assoc, 1, f <- function(x) {return(length(na.omit(x)))}) 
  
  # Turn the species and association info into long vectors with no NAs 
  species_vector <- unlist(apply(plot_data_species, 1, na.omit))
  assoc_vector <- unlist(apply(plot_data_assoc, 1, na.omit))
  n_species <- length(species_vector)
  n_assoc <- length(assoc_vector)
  total_number_of_obs <- n_species + n_assoc
  
  # Combine all the info into a long-form data frame with columns for plot id, location id, an indicator for "pin" vs "assoc", plus the long vectors of species and association ids in the last column. 
  species_data_long <- data.frame(plotID = rep(plot_data$plotID[1], total_number_of_obs), dOut_m = c(rep(plot_data$dOut_m, n_species_by_row), rep(plot_data$dOut_m, n_assoc_by_row)), pin_vs_assoc = c(rep("pin", n_species), rep("assoc", n_assoc)), species = c(species_vector, assoc_vector))
  
  return(species_data_long) 
} ## End function


