
# Calculate Understory Species Richness -----------------------------------
# Authors: Jenny Cribbs
# Date: 26 April 2024

library(dplyr)
library(tidyr)

# Ensure plotID is numeric
allLifeStages$plotID <- as.numeric(allLifeStages$plotID)

# Add leading zeros to plotID values for single-digit numbers
allLifeStages$plotID <- sprintf("%02d", allLifeStages$plotID)

# filter out substrates that shouldn't count for species richness
species_richness <- allLifeStages %>%
  filter(!(species %in% c("litter", "rock", "wood", "bareground"))) %>%
  group_by(plotID) %>% # organize by plot
  summarise(richness = n_distinct(species)) %>% # list unique occurences of species in each plot
  complete(plotID = unique(allLifeStages$plotID)) %>%
  replace_na(list(richness = 0)) # fill in zero for any plot with no plant species e.g. only litter

class(species_richness$plotID)

# chat GPT version--trying to retain plots with zero plants

allLifeStages %>%
  group_by(plotID) %>%
  summarise(unique_species_count = n_distinct(species)) %>%
  complete(plotID = unique(allLifeStages$plotID)) %>%
  replace_na(list(unique_species_count = 0)) %>%
  filter(!(plotID %in% c("litter", "rock", "wood", "bareground")))


# Next goal is to bind the plot-level species richness numbers with other plot data, so the richness list needs to be in the right order or maybe better to join on plotID
# bring in saved plot level data (or all tree level data with plot data)
# combined_data <- cbind(plot_data, rich)


