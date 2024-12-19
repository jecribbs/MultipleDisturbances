##----------------------------------------------------------------------------------------------##
# Author: Olivia Ross
# Date: 18 December 2024

## This script creates a plant name reference sheet for Jenny Cribbs' Multiple Disturbances Project
## Based on vegetation data collected in SEKI Summer 2024

# packages
librarian::shelf(here, dplyr, readxl, writexl, data.table)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# read in Understory List Data for each plot and combine files

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# one xlsx file per survey plot 

# list all xlsx files in the folder
understory_plot_files <- list.files(path = here("UnderstoryListData"), #path for understory files
                    pattern = "\\.xlsx$",
                    full.names = TRUE)
            
# create an empty list to store the understory files for each plot
understory_list <- list()

# loop through the "UnderstoryListData" folder and read in each xlsx file
for (file in understory_plot_files) {
  
  # read in each excel xlsx file
  understory_plot <- read_excel(file) 
  
  # store each understory plot file in it's own data frame
  understory_list[[file]] <- understory_plot 
}

# combine understory data frames for all plots
understory_plot_combined <- do.call(rbind, understory_list)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create a list of unique plant names found in SEKI 2024 plots

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# select just species name for plant list
seki2024_species_list <- understory_plot_combined |>
  select(species)

# remove duplicates
seki2024_species_list <- seki2024_species_list[!duplicated(seki2024_species_list),] #230 when using understory data from 18 December 2024, by OLR
  
# sort alphabetically
seki2024_species_list <- seki2024_species_list[order(seki2024_species_list$species),]

# export as xlsx to "SEKI 2024" project folder
write_xlsx(seki2024_species_list, "G:\\.shortcut-targets-by-id\\1AvILjAM2I4sEqI94IechT7wlI0EFF81J\\SEKI_2024\\Data\\SEKI2024_plantlist.xlsx" )