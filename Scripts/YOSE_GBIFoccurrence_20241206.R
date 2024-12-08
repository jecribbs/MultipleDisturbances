# GBIF: Occurence Tab for PILA and Associated Trees
# Authors: Jenny Cribbs, Tazlina Dentinger
# Date created: 06 December 2024
# Updated: 

# Overall Input: Read in PILA and Associated Tree data from Excel files downloaded from Google Sheets using for loop
# Code Description: clean data with a row for each tree with a unique occurrence ID and columns matching GBIF columns, so the output can be pasted into NPS template occurrence tab
# Overall Output:

# Part1: YOSE PILA Data Import -------------------------

# Load initial packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)
library(VIM)

# Set the working directory
setwd("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/")

# Bring in the PILA data for each plot in the project folder from Google Sheets
# setting the directory for data extraction--change to your local data directory
datadir <- "/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/RawData/YPE_Data"

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
pila_list <- data.frame()

# loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "PILA") # choose PILA data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
      dplyr::select(plotID, plot_type, date, crew, trans_length, width, 
                    slope, aspect, plot_azimuth,
                    plot_beg_UTM_N, plot_beg_UTM_E, 
                    plot_end_UTM_N, plot_end_UTM_E, 
                    plot_notes, plot_elevation_ft, 
                    ribes_50m, ribes_100m, ribes_150m, ribes_200m, 
                    seedlings_50m, seedlings_100m, seedlings_150m, seedlings_200m,
                    treeNum, species, DBH_cm, est_DBH_cm, height_m, est_height_m,
                    pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker, flags, DTOP,
                    percentLive, fire_scar, resistance, damageCodes, notes 
      )
    pila_list <- rbind(pila_list, xlsfile)
    
  }
}

# check data types
summary(pila_list)

# Part2: PILA Data Wrangling -------------------------

# remove row with no data
pila_list <- pila_list %>% filter(!is.na(plotID))

# rename columns to match template style
pila_list <- pila_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         diameter = DBH_cm,
         height = height_m,
         deadTop = DTOP, 
         boleChar = fire_scar)

# add GBIF columns to match occurrence tab template
pila_list <- pila_list %>% 
  mutate(occurrenceID = paste(eventID, treeNum, sep = "_"), 
         basisOfRecord = "HumanObservation",
         taxonID = "PILA",
         scientificName = "Pinus lambertiana (Douglas, 1827)",
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = ifelse(eventID %in% c(5, 7, 8, 25), "absent", "present"), # change to absent for YPE 5, 7, 8, and 25
         individualCount = ifelse(eventID %in% c(5, 7, 8, 25), 0, 1),
         organismQuantity = 1,
         organismQuantityType = "individual", 
         publicDisplay = "yes",
         dataAccess = "",
         lifeStage = "",
         sex = "",
         reproductiveCondition = if_else(str_detect(notes, "\\bcone(s)?\\b"), "cone-bearing", "no cones observed"),
         behavior = "",
         covariateSample = "",
         preparations = "",
         footprintWKT = "",
         footprintSRS = "", 
         associatedMedia = "",
         identifiedBy = recordedBy,
         dateIdentified = date,
         identificationReferences = "",
         identificationRemarks = "",
         identificationQualifier = "",
         identificationVerificationStatus = "",
         occurrenceRemarks = notes,
         materialSampleID = "",
         recordNumber = "",
         organismRemarks = "",
         identificationID = "")



# select columns for GBIF occurrence tab
cleanPILAdata <- pila_list %>% 
  select(occurrenceID, eventID, basisOfRecord, taxonID, scientificName, recordedBy, occurrenceStatus, individualCount, organismQuantity, organismQuantityType, publicDisplay, dataAccess, lifeStage, sex, reproductiveCondition, behavior, covariateSample, preparations, identifiedBy, dateIdentified, identificationReferences, identificationRemarks, identificationQualifier, identificationVerificationStatus, occurrenceRemarks, materialSampleID, recordNumber, organismRemarks, identificationID, diameter, height, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, activeBoleCanker, inactiveBoleCanker, deadTop, percentLive, boleChar)

# Part3: YOSE PILA Data Import -------------------------

# save as a csv file in the working directory
write.csv(cleanPILAdata, "YOSE_cleanPILAdata.csv", row.names = FALSE) # don't save first column

# Part4: Associated Tree Data Import -------------------------
# Bring in the tree data for each plot in the YPE_Data folder -----------

# provide path for files in datadir
folders <- list.dirs(datadir, full.names = TRUE)[-c(1,4)] # Ensure full path names are used

# initialize an empty list to store data for each plot
tree_list <- data.frame() #pila_list var changed to tree_list

# loop through each folder
for (folder in folders) {
  # list all files in the folder
  files <- list.files(folder, pattern = "Tree") # choose Tree data only
  
  # Loop through each file
  for(file in files) {
    # Read the Excel file
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% # keeps numbers numeric
      dplyr::select(plot, date, crew,
                    treeNum, species, DBH_cm, height_m,
                    dOut_m, dSideR_m, dSideL_m,
                    percentLive, damageCodes, notes
      )
    tree_list <- rbind(tree_list, xlsfile)
    
  }
}

# check data types
summary(tree_list)

# Part4: Associated Tree Data Wrangling -------------------------

# fix plotID naming
tree_list <- tree_list %>% 
  rename(plotID = plot) # plotID entered as plot for Trees

# convert dSide Left to numeric 
tree_list$dSideL_m <- as.numeric(tree_list$dSideL_m) # likely dash entry error

# combine dSideR_m and dSideL_m into a single dSide column with checks
tree_list <- tree_list %>%
  mutate(dSide = case_when(
    !is.na(dSideR_m) & dSideR_m >= 0 ~ dSideR_m,
    !is.na(dSideL_m) & dSideL_m <= 0 ~ dSideL_m,
    TRUE ~ NA_real_
  ))
summary(tree_list)

# check species field
unique(tree_list$species)

# clean species list
tree_list <- tree_list %>%
  mutate(species = case_when(
    species == "PYGE" ~ "PIJE", # correct misspelling in data entry
    species %in% c("unknown", "UNK", "UNKNOWN", "Charcol", "Unknown") ~ "UNKNOWN",
    TRUE ~ species
  ))

# check species field again
unique(tree_list$species)

# remove SALIX and CONU because they should be shrubs 
# although some are tree-like, these species were likely not assessed as trees uniformly across the plots
tree_list <- tree_list %>% 
  filter(species != "CONU") %>% 
  filter(species != "SALIX") # removes only 2 records total (3309 to 3307)

# check damage codes field
unique(tree_list$damageCodes)

#then need to concatenate with damageCodes column

# make damage codes uppercase
tree_list <- tree_list %>% 
  mutate(damageCodes = toupper(damageCodes))

#start with empty column and use separate if_else statements for each term 
tree_list <- tree_list %>% 
  mutate(fireDamage = "")
#ensure not case sensitive

#add FIRE code, maybe others, based on notes using 
tree_list <- tree_list %>% 
  mutate(fireDamage = if_else(str_detect(notes, "char"), "FIREch", "", missing = "")) # fireDamage = if_else(str_detect(notes, "crisp"), "FIRE", "", missing = ""))
# fireDamage = if_else(str_detect(notes, "fire"), "FIRE", "", missing = ""),
# fireDamage = if_else(str_detect(notes, "burn"), "FIRE", "", missing = ""),
# fireDamage = if_else(str_detect(notes, " fs"), "FIRE", "", missing = ""),
# fireDamage = if_else(str_detect(notes, "fs "), "FIRE", "", missing = ""),
# fireDamage = if_else(str_detect(notes, "cat face"), "FIRE", "", missing = ""))


#check fire damage column - also need to check for false positive FIRE, then remove
unique(tree_list$fireDamage)

treeFire <- filter(tree_list, fireDamage == "FIRE")

# transform damage codes list to and standardize break char and damage codes to FHW codes
tree_list$damageCodes <- gsub('BROKE', 'BROK',
                              gsub('BTOP', 'BROKE', # change to BROK after verifying BTOPs
                                   gsub('_', '|', 
                                        gsub(', ', '|', 
                                             gsub('CROOK', 'CROK', tree_list$damageCodes)))))

# Part 6: Write out Clean Tree LIst -------------------------

# write results to a csv in case GBIF format is not desirable
write.csv(tree_list, "CleanTreeList.csv")

# Part7: Reorganization for GBIF -------------------------

# rename columns to match occurrence tab template
treeOccurrenceData <- tree_list %>% 
  rename(eventID = plotID,
         recordedBy = crew,
         taxonID = species,
         diameter = DBH_cm,
         height = height_m)

# add GBIF columns to match occurrence tab template
treeOccurrenceData <- treeOccurrenceData %>% 
  mutate(occurrenceID = paste("E", eventID, "-", "Tree", treeNum, sep = ""),  #may need to update occurrenceID so the PILA and Trees don't overlap--> adding "Tree" for now
         basisOfRecord = "HumanObservation",
         scientificName = case_when(
           taxonID == "PIJE" ~ "Pinus jeffreyi (Balf. 1853)", # check re abbreviations
           taxonID == "ABCO" ~ "Abies concolor (Lindley, 1861)", # (Gordon & Glend.) Lindl. ex Hildebr., 1861
           taxonID == "ABMA" ~ "Abies magnifica (A.Murray bis, 1863)", # check with NPS to see if this should just be Murray, 1863
           taxonID == "CADE" ~ "Calocedrus decurrens ((Torr.) Florin, 1956)", # (Torr.) Florin, 1956 is most recent Torr., 1853 is original
           taxonID == "PIPO" ~ "Pinus ponderosa (Douglas, 1836)",
           taxonID == "QUKE" ~ "Quercus kelloggii (Newberry, 1857)",
           taxonID == "PSME" ~ "Pseudotsuga menziesii (Lipscomb, 1993)", # this doesn't seem right, but on NCBI??? 
           taxonID == "UNKNOWN" ~ "Pinales", # check that we're not mislabeling any oaks etc. 
           taxonID == "QUCH" ~ "Quercus chrysolepis",
           taxonID == "QUWI" ~ "Quercus wislizeni",
           taxonID == "ACMA" ~ "Acer macrophyllum",
           taxonID == "UMCA" ~ "Umbellularia californica",
           taxonID == "PICO" ~ "Pinus contorta",
           taxonID == "JUOC" ~ "Juniperus occidentalis",
           taxonID == "Abies_sp" ~ "Abies"), #needs dates and names (GBIF)
         recordedBy = gsub("_", "|", recordedBy),
         occurrenceStatus = "present",
         individualCount = 1,
         organismQuantity = 1,
         organismQuantityType = "individual", 
         publicDisplay = "yes",
         dataAccess = "",
         lifeStage = "",
         sex = "",
         reproductiveCondition = "",
         behavior = "",
         covariateSample = "",
         preparations = "",
         identifiedBy = recordedBy,
         dateIdentified = date,
         identificationReferences = "",
         identificationRemarks = "",
         identificationQualifier = "",
         identificationVerificationStatus = "",
         occurrenceRemarks = notes,
         materialSampleID = "",
         recordNumber = "",
         organismRemarks = "",
         identificationID = "",
         pitchTubes = "",
         exitHoles = "",
         activeBranchCanker = "",
         inactiveBranchCanker = "",
         activeBoleCanker = "",
         inactiveBoleCanker = "",
         deadTop = if_else((str_detect(damageCodes, "\\bDTOP\\b") | (str_detect(damageCodes, "\\bSD\\b"))), "Y", "N", missing = "N"),
         boleChar = if_else(str_detect(damageCodes, "\\bFIRE\\b"), "Y", "N", missing = "N"))


# select columns for GBIF occurrence tab
gbifTreeOccurrence <- treeOccurrenceData %>% 
  select(occurrenceID, eventID, 
         basisOfRecord, taxonID, scientificName, recordedBy, 
         occurrenceStatus, individualCount, organismQuantity, 
         organismQuantityType, publicDisplay, dataAccess, 
         lifeStage, sex, reproductiveCondition, behavior, 
         covariateSample, preparations, identifiedBy, dateIdentified, 
         identificationReferences, identificationRemarks, 
         identificationQualifier, identificationVerificationStatus, 
         occurrenceRemarks, materialSampleID, 
         recordNumber, organismRemarks, identificationID, 
         diameter, height, pitchTubes, exitHoles, 
         activeBranchCanker, inactiveBranchCanker, 
         activeBoleCanker, inactiveBoleCanker, 
         deadTop, percentLive, boleChar) # add damage codes

# Part8: Combine PILA and Tree Data -------------------------

gbifOccurrence <- rbind (cleanPILAdata, gbifTreeOccurrence)
# save as a csv file in the working directory
write.csv(gbifOccurrence, "YOSE_GBIFoccurrence.csv", row.names = FALSE) # don't save first column





