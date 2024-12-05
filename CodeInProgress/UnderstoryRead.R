

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

# Load initial packages
library(tidyverse)
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

# trying to make a list into a data frame
# joins won't take a list as a argument
#full_join(understory_list[1], understory_list[2])

read_csv()

# check data types
summary(understory_list)


#write_csv(understory_list, "Data/CleanData/UnderstoryData.csv")