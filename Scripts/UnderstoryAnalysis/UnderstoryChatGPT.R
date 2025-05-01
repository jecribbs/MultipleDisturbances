# Load required library
library(readxl)

# Directory containing Excel files for each plot
plot_files_directory <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# List files with understory in the name within the directory
plot_files <- list.files(path = plot_files_directory, pattern = "Understory")

# Initialize an empty data frame to store combined data
combined_data <- data.frame(matrix(nrow = 50, ncol = 0))  # Assuming 50 rows for each plot

# Loop through each Excel file and process data
for (file in plot_files) {
  # Read data from Excel file
  plot_data <- read_excel(file)
  
  # Assuming your Excel file has a column named "Species_Code" containing the species codes
  species_codes <- plot_data$Species_Code
  
  # Create a table of species counts for the current plot
  species_counts <- table(species_codes)
  
  # Add the species counts as a new column in the combined data frame
  combined_data <- cbind(combined_data, species_counts)
}

# Fill NA values with 0
combined_data[is.na(combined_data)] <- 0

# Print the combined data
print(combined_data)

# Example function to calculate species frequency (same as before)
calculate_species_frequency <- function(data) {
  species_count <- table(unlist(data))
  species_frequency <- prop.table(species_count)
  return(species_frequency)
}

# Calculate species frequency for each plot
plot_species_frequency <- calculate_species_frequency(combined_data)

# Print species frequency
print(plot_species_frequency)

# Plot species frequency
barplot(plot_species_frequency, main="Combined Species Frequency", xlab="Species", ylab="Frequency")
