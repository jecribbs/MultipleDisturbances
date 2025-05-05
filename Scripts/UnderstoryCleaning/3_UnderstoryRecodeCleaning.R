
# (3) Recode And Clean Understory Data/Pre-processing for Richness----------------------------------------

# Authors: Jenny Cribbs & Tazlina Dentinger
# Date: 21 April 2024

# Input: all_plots_understory data frame with 4 columns from step 2

# Code description: This cleaning script recodes misspellings, redundancies/synonyms e.g. scientific code and common name or code and full name, and lumps unknowns and life stages. 

# Desired Output: (1) A clean dataframe with one consistent code for each species, substrate, or association. (2) For richness, we will then filter out the substrates, so that the richness calculation includes only unique understory plant species (hits + associations). Unknowns will have to be filtered out entirely or lumped...discuss! (3) For percent cover, filter out associations since we only want to know what was hit (species and substrate). Probably best to put 2 and 3 as part of their respective analysis scripts.

# -------------------------------------------------------------------------

library(tidyverse)

# Read in data (or run Understory scripts 1 and 2)
all_plots_understory <- read_csv("dataSandbox/RawData/UnderstoryDataLong.csv")

#remove duplicated associated species rows:
all_plots_understory <- all_plots_understory %>% 
  filter(pin_vs_assoc == "pin" | (pin_vs_assoc == "assoc" & dOut_m == 2))

#create unique ID for each plot & hit and number the hits to track the order
all_plots_understory <- all_plots_understory %>% mutate(id = paste0("pl", plotID, "_", "pt", dOut_m))
all_plots_understory <- all_plots_understory %>% group_by(id) %>% mutate(hitNum = row_number())
all_plots_understory <- all_plots_understory %>% mutate(hitNum = case_when(
  pin_vs_assoc == "pin" ~ hitNum,
  pin_vs_assoc == "assoc" ~ NA,
  TRUE ~ NA
)) %>% ungroup () %>% select(!id)

##---------------------- Part 1: Add identifications made post-field season ----------------------------
#Based on review documented in Unknown Veg Notes -----------------------------------------------------

#Still needs full check with full Unknown Veg Notes

#read in Unknown Veg csv file
unkVeg <- read_csv("Data/RawData/Test Unknown Veg Notes - UnknownPlants.csv")
names(unkVeg) <- gsub(" ", "_", names(unkVeg))
#keep only relevant columns and YOSE rows
unkVeg <- unkVeg %>% select(project, asEntered, code, bestGuess, nextSteps, plotID) %>% filter(project == "YOSE")

#Create unique columns by plotID and species in each df to merge
unkVeg <- unkVeg %>% mutate(
  uniqueID = paste0(asEntered, plotID))
all_plots_understory <- all_plots_understory %>% mutate(
  uniqueID = paste0(species, plotID))

#use merge to map codes in dictionary to codes in df - Something's off here
all_plots_wUnk <- merge(all_plots_understory, unkVeg, by.x = "uniqueID", by.y = "uniqueID" all.x = F) #CHANGE BACK TO TRUE 
all_plots_merged <- all_plots_wUnk %>% 
  mutate(species = case_when(
    nextSteps %in% c("ID'ed", "Not ID-able") ~ bestGuess, #need to clean up bestGuess column before using - or use Code?
    asEntered == NA_character_ ~ "ooooooo", #what the hell man - doesn't remotely work. Neither does regular NA or is.na
    #when there is no entry in unkVeg ~ species
    TRUE ~ species
  )) %>% select(plotID.x, dOut_m, pin_vs_assoc, species, nextSteps) %>% rename(plotID = plotID.x)

#change df name back to whatever feeds into Part 2

##-------------------------- Part 2: clean data "manually" in R ----------------------------
#Includes fixing misspellings and incorrect entries or common names-------------------------

# Look at all unique hits
unique(all_plots_understory$species) # 375 (reduced from 501 when there was incorrect incrementing)

# bar chart to look at frequency of unique hits
all_plots_understory %>% 
  group_by(species) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
  xlab("Hit") +
  ylab("Proportion") +
  coord_flip()

#Standardize species ending (currently no "sp", no _1 etc.)
all_plots_understory_trim <- all_plots_understory %>% 
  mutate(species = case_when(
    str_detect(species, " sp.") ~ unlist(str_split(species, " "))[1],
    str_detect(species, "Taraxacum sp") ~ "Taraxacum", #only case that doesn't include a "."
    str_detect(species, "_sp") ~ unlist(str_split(species, "_"))[1],
    str_detect(species, "_\\d+") ~ unlist(str_split(species, "_"))[1],
    TRUE ~ species
  ))

# Correct misspellings and misidentifications
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    species %in% c("littter", "ltter", "littler", "litterlitter", "lItter", "pinecone", "bark") ~ "litter", #removed "unknown_DD" from list - should be in wood
    species %in% c("bare", "dirt", "DG", "dg") ~ "bareground",
    species %in% c("rcok", "gravel") ~ "rock",
    str_detect(species, "_DD") ~ "wood",
    species == "Pranus" ~ "Prunus",
    species == "VECA" ~ "Fragaria vesca",
    species %in% c("PPREM", "PREM") ~ "Prunus emarginata",
    species == "cynocerous" ~ "Cynosurus echinatus",
    species %in% c("striptanthis", "streptanthus tortuosus") ~ "Streptanthus tortuosus",
    species %in% c("Saxifrogacea", "saxifrogacea", "Saxifrage", "unk_saxifrage") ~ "Saxifragaceae",
    species %in% c("castilleja", "castelleja") ~ "Castilleja",
    species == "enicamena" ~ "Ericameria",
    species == "Eriogonum sp." ~ "Eriogonum" ,
    species == "Eriogunum sp." ~ "Eriogonum",
    species == "Senicio glomerata" ~ "Senecio glomerata",
    species == "Achillea millefolia" ~ "Achillea millifolium",
    species == "AZOC" ~ "Rhododendron occidentale",
    species == "CAREXI" ~ "Carex",
    species == "Pseudoghaphalium californicum" ~ "Pseudognaphalium californicum",
    species == "HIAE" ~ "HIAL",
    species == "CIEN" ~ "CEIN",
    species == "GRER" ~ "GAER",
    species == "COMO" ~ "CAMO",
    species == "STFO" ~ "STTO",
    species %in% c("SYCO", "GAHI") ~ "SYMO", #paper data sheet said creeping snowberry; recoded to correct snowberry
    species == "PITO" ~ "Diplacus torreyi",
    species == "ERHU" ~ "ERNU",
    species == "DOOB" ~ "GOOB",
    species == "MYOD" ~ "Osmorhiza", #paper data sheet said sweet cicely; recoded to correct sweet cicely
    species == "GADS" ~ "GADI",
    species == "RALE" ~ "RULE",
    species == "QUDU" ~ "Quercus berberidifolia", #incorrect scrub oak
    species == "DEMI" ~ "DERI", #misspelling of species code
    species == "PUCA" ~ "ROCA",
    species == "SASC" ~ "Salix", #insufficient evidence of species-level ID
    species == "PEHE" & plotID %in% c("3", "4", "9") ~ "Penstemon", #paper data sheet said penstemon or blue penstemon
    grepl("^QUWE_", species, ignore.case = TRUE) ~ "QUWI",
    species == "TOPU" ~ "TODI", #incorrect poison oak entered
    species == "CAUM" ~ "Calyptridium", #paper said pussypaws
    species == "PENE" ~ "Penstemon", #paper said maybe newberrii
    species == "COCO_seed" ~ "COCO", #not a tree => seeedlings not recorded
    species == "BRCA" & plotID == 8 ~ "Poaceae", #paper data sheet said grass 1
    species %in% c("Juncus", "juncus") & plotID %in% c(49,73) ~ "Poales", #on paper as juncus/round carex
    species == "GAPRO" ~ "Pyrola", #on paper as wintergreen
    species == "LOIN" ~ "shrub_1", #based on note on datasheet 
    species == "LUMA" ~ "LULA", #L. macrophyllum doesn't exist
    species == "Juncus patens" ~ "Juncus", #insufficient evidence of species-level ID
    species == "Erythranthre" ~ "Erythranthe",
    species == "RINI" ~ "RIVI", #incorrectly entered from plot 19
    species == "epilobium" & plotID == 25 ~ "Epilobium brachycarpum",
    TRUE ~ species
  ))

# Look at all unique hits
unique(all_plots_understory$species) #was 483, down to 339 by removing incrementing, recoding some incorrect entries
 
# Convert common names to scientific and fix scientific names
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    #species %in% c("moss1", "moss2") ~ "moss", # do we want to count brophytes, lichens, etc???
    species == "cryptantha/plagiobothrys" ~ "Boraginaceae",
    species == "brome" ~ "Bromus",
    species == "fireweed" ~ "CHAN",
    species == "horsetail" ~ "Equisetum",
    species == "rush" ~ "juncus",
    species %in% c("unk_apeaceae", "unk_apiaceae") ~ "Apiaceae",
    species == "elgl" ~ "ELGL",
    species %in% c("whisker plant", "whisker brush", "whiskerbrush") ~ "LECI",
    species == "Taraxacum sp" ~ "Taraxacum",
    species == "goldenrod" ~ "Solidago",
    species == "vetch" ~ "Vicia",
    species == "fern" ~ "Polypodiopsida",
    species == "Dodder" ~ "Cuscuta",
    species == "aster" ~ "Asteraceae",
    species == "grass" ~ "Poaceae",
    species == "piperia" ~ "Platanthera", #genus Pipera subsumed
    species == "SACE" ~ "SAME", #Blue Elder moved to S. mexicana
    species == "CYOF" ~ "Adelinia grandis",
    
    TRUE ~ species
  ))

# # convert codes to scientific names - will be taken care of with dictionary
# known <- known %>% mutate(species == case_when(
#   species == "BRTE" ~ "Bromus tectorum",
#   species == "RIRO" ~ "Ribes roezlii",
#   species == "CEIN" ~ "Ceanothus integerrimus",
#   species == "CHFO" ~ "Chamaebatia foliosa",
#   species == "ARVI" ~ "Arctostaphylos viscida"
#   )
# )

# Look at all unique hits
unique(all_plots_understory$species) # 479 ->333

##----------- Part 3: lump various Poales, forbs, life stages for visualization ---------------------------

# lump all carex for now
carexcheck <- all_plots_understory %>% 
  mutate(species = case_when(
    str_detect(species, "(?i)carex_") ~ "Carex",
    str_detect(species, "(?i)carex sp") ~ "Carex",
    TRUE ~ species
  ))
# Look at all unique hits
unique(carexcheck$species) # 424 ->328

# lump all unknown grasses for now
grassgroup <- carexcheck %>% 
  mutate(species = case_when(
    grepl("^grass_", species, ignore.case = TRUE) ~ "Poaceae", #any common names w/ grass that aren't Poaceae?
    TRUE ~ species
  ))
# Look at all unique hits
unique(grassgroup$species) # 373 ->236

# lump all unknown forbs for now
forbfest <- grassgroup %>% 
  mutate(species = case_when(
    grepl("^forb_", species, ignore.case = TRUE) ~ "forb",
    species %in% c("forb 1", "Forb", "forbe", "forbe_1", "forbe_2", "forb_1", "forbe_unknown", "forb_unknown") ~ "forb",
    TRUE ~ species
  ))

# Look at all unique hits
unique(forbfest$species) # 315 -> 318

# bar chart to look at frequency of unique hits
forbfest %>% 
  group_by(species) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
  xlab("Hit") +
  ylab("Proportion") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# Consolidate life stages--actually maybe just for richness not for percent cover 
allLifeStages <- forbfest %>% 
  mutate(species = case_when(
    grepl("^ABCO_", species, ignore.case = TRUE) ~ "ABCO",
    grepl("^ABMA_", species, ignore.case = TRUE) ~ "ABMA",
    grepl("^ACMA_", species, ignore.case = TRUE) ~ "ACMA",
    grepl("^CADE_", species, ignore.case = TRUE) ~ "Calocedrus decurrens",
    grepl("^JUOC_", species, ignore.case = TRUE) ~ "JUOC",
    grepl("^PILA_", species, ignore.case = TRUE) ~ "PILA",
    grepl("^PIJE_", species, ignore.case = TRUE) ~ "PIJE",
    grepl("^PIPO_", species, ignore.case = TRUE) ~ "Pinus ponderosae",
    grepl("^PSME_", species, ignore.case = TRUE) ~ "PSME",
    grepl("^QUCH_", species, ignore.case = TRUE) ~ "QUCH",
    grepl("^QUKE_", species, ignore.case = TRUE) ~ "QUKE",
    grepl("^QUWI_", species, ignore.case = TRUE) ~ "QUWI",
    TRUE ~ species
  ))

unique(allLifeStages$species) # 283 ->292

# bar chart to look at frequency of unique hits
allLifeStages %>% 
  group_by(species) %>%
  summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=species, y=freq),stat="identity") +
  xlab("Hit") +
  ylab("Proportion") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# group by plot 
allLifeStages %>% group_by(plotID)  %>% 
summarize (n = n()) %>%
  mutate(total = sum(n),
         freq = n / total) %>%
  ggplot() +
  geom_bar(mapping=aes(x=plotID, y=freq),stat="identity") +
  xlab("Hit") +
  ylab("Proportion") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

##-------- Part 4: Use Species Code Dictionary & Taxonstand to apply scientific names --------
#This may eventually migrate to a separate script

#read in Species Code Dictionary csv file
spDict <- read_csv("Data/RawData/Test Species Code Dictionary - Sheet1.csv")
#replace spaces with underscores in column names
names(spDict) <- gsub(" ", "_", names(spDict))
#keep only relevant columns
spDict <- spDict %>% select(Code, Scientific_Name)

#use merge to map codes in dictionary to codes in df
all_plots_understory <- merge(all_plots_understory, spDict, by.x = "species", by.y = "Code", all.x = F) #CHANGE BACK TO TRUE to keep all vals
all_plots_understory <- all_plots_understory %>% 
  mutate(species = case_when(
    !is.na(all_plots_understory$Scientific_Name) ~ all_plots_understory$Scientific_Name
  )) %>% select(!Scientific_Name)

##########################################################################################
#From here on, all of this will apply to both YOSE and SEKI data. May need to move to another script or bring in SEKI data

library(U.Taxonstand)
library(readxl)

#U.Taxonstand database
spDatabase1 <- read_excel("C:/Users/tazli/Downloads/YOSE_SugarPine/DataClean/Plants_LCVP_database_part1.xlsx")
spDatabase2 <- read_excel("C:/Users/tazli/Downloads/YOSE_SugarPine/DataClean/Plants_LCVP_database_part2.xlsx")
spDatabase3 <- read_excel("C:/Users/tazli/Downloads/YOSE_SugarPine/DataClean/Plants_LCVP_database_part3.xlsx")
spDatabase <- rbind(spDatabase1, spDatabase2, spDatabase3)
rm(spDatabase1, spDatabase2, spDatabase3)

#use U.Taxonstand to spell check scientific names (takes a second)
nameMatch <- nameMatch(spList=known$species, spSource=spDatabase, author = TRUE, max.distance= 4)
#keep only rows with fuzzy matching
nameMatchFuzzy <- nameMatch %>% select(Submitted_Name, Fuzzy, Name_in_database) %>% filter(Fuzzy == TRUE) %>% unique()
#match accepted names to the misspelled rows
known <- merge(known, nameMatchFuzzy, by.x = "species", by.y = "Submitted_Name", all.x = TRUE)
#change misspelled names to database names
known <- known %>% mutate(species = case_when(
  !is.na(Name_in_database) & Fuzzy == TRUE ~ Name_in_database, 
  TRUE ~ species
)) %>% select(species, plotID, dOut_m, pin_vs_assoc)

##-------------------------------Part 6: Write dataframe to csv-----------------------------------
