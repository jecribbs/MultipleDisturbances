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
                       "species6", "species7", "species8", "species9", "species10",
                       "species11", "species12", "species13", "species14", "species15",
                       "species16", "species17", "species18", "species19", "species20",
                       "species21", "species22", "species23", "species24", "species25",
                       "species26", "species27", "species28", "species29")  
  # Calculate species richness for intercepted species for this plot
  species_richness_per_plot[i] <- calculate_species_richness(understory_list[[i]], species_columns)
}

# Print or use species richness for each plot
print(species_richness_per_plot)

# Calculate species richness for associated species for each plot separately
associated_richness_per_plot <- numeric(length(understory_list))  # Initialize a numeric vector to store species richness for each plot

# Loop through each plot's data
for (i in seq_along(understory_list)) {
  # Specify the columns containing intercepted species data for this plot
  species_columns <- c("assoc1", "assoc2", "assoc3", "assoc4", "assoc5", 
                       "assoc6", "assoc7", "assoc8", "assoc9", "assoc10",
                       "assoc11", "assoc12", "assoc13","assoc14", 
                       "assoc15", "assoc16", "assoc17", "assoc18",
                       "assoc19", "assoc20", "assoc21", "assoc22", "assoc23",
                       "assoc24", "assoc25", "assoc26", "assoc27", "assoc28",
                       "assoc29", "assoc30", "assoc31", "assoc32", "assoc33")  
  
  # Calculate species richness for intercepted species for this plot
  associated_richness_per_plot[i] <- calculate_species_richness(understory_list[[i]], species_columns)
}

# Print or use species richness for each plot
print(associated_richness_per_plot)

