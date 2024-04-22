
# Calculate Understory Species Richness -----------------------------------



# Get species richness for plot, including both pin touches and associations
testrun %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(species) %>%
  unique() %>% count() # count provides richness as a number, unique the full list

# Try to use test code for species richness for all plots
species_richness <- all_plots_understory %>% group_by(plotID) %>% dplyr::filter(!(species %in% c("litter", "rock", "wood", "bareground", "litterlitter"))) %>% 
  select(plotID, species) %>%
  unique() %>% count()
class(species_richness) # grouped dataframe--ok?
head(species_richness)
# appears to work, but two plots dropped out--check if they should be zero
# add in missing plots with a zero or modify the filter statement



# This is getting close, but the order is weird should have added 01, 02 etc. for single digit plots
# Next goal is to bind the plot-level species richness numbers with other plot data, so the richness list needs to be in the right order or maybe better to join on plotID
# bring in saved plot level data (or all tree level data with plot data)
# combined_data <- cbind(plot_data, rich)

