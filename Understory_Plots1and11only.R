# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

# Load initial packages
library(tidyverse)
library(readxl)
library(VIM)

# setting the directory for data extraction
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# list all file names in the data directory
files <- list.files(datadir)
# provide path for files in datadir
folders <- list.dirs(datadir)[-c(1,4)]
# create a blank dataframe 
understory_list <- data.frame()
# bring in selected variables from excel files in datadir
for (folder in folders) {
  files = list.files(folder, pattern = "Understory") # Understory data only
  for(file in files) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>% 
      dplyr::select(everything())
    understory_list <- rbind(understory_list, xlsfile)
  }
}
# check data types
summary(understory_list)


write_csv(understory_list, "Data/CleanData/UnderstoryData.csv")

# Must run UnderstoryRead.R to create the understory_list object 

# Define a function to calculate species richness
calculate_species_richness <- function(data, species_columns) {
  # Extract intercepted species from the specified columns
  species <- unlist(data[species_columns], use.names = FALSE)
  
  # Remove NA values
  species <- species[!is.na(species)]
  
  # Calculate species richness
  species_richness <- length(unique(species))
  
  return(species_richness)
}

# Calculate species richness for intercepted species for each plot 
species_richness_per_plot <- numeric(length(understory_list))  # Initialize a numeric vector to store species richness for each plot

# Loop through each plot's data
for (i in seq_along(understory_list)) {
  # Specify the columns containing intercepted species data for this plot
  species_columns <- c("species1", "species2", "species3", "species4", "species5", 
                       "species6", "species7", "species8", "species9", "species10")  
  # Calculate species richness for intercepted species for this plot
  species_richness_per_plot[i] <- calculate_species_richness(understory_list[[i]], species_columns)
}

# Print or use species richness for each plot
print(species_richness_per_plot)