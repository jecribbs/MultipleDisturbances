
# (3) Recode And Clean Understory Data/Pre-processing for Richness----------------------------------------

# Authors: Jenny Cribbs
# Date: 21 April 2024

# Input: all_plots_understory data frame with 4 columns from step 2

# Code description: This cleaning script recodes mispellings, redundancies/synonyms e.g. scientific code and common name or code and full name, and lumps unknowns and life stages. 

# Desired Output: (1) A clean dataframe with one consistent code for each species, substrate, or association. (2) For richness, we will then filter out the substrates, so that the richness calculation includes only unique understory plant species (hits + associations). Unknowns will have to be filtered out entirely or lumped...discuss! (3) For percent cover, filter out associations since we only want to know what was hit (species and substrate). Probably best to put 2 and 3 as part of their respective analysis scripts.

# -------------------------------------------------------------------------

# Read in data (or run scripts 1 and 2)
# all_plots_understory <- read_csv("UnderstoryDataLong.csv")

# Look at all unique hits
unique(all_plots_understory$species) # 501

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

# Correct misspellings
spellcheck <- all_plots_understory %>% 
  mutate(species = case_when(
    species %in% c("littter", "ltter", "littler", "litterlitter", "lItter", "pinecone", "bark", "unknown_DD") ~ "litter",
    species %in% c("bare", "dirt", "DG", "dg") ~ "bareground",
    species %in% c("rcok", "gravel") ~ "rock",
    species %in% c("^_DD") ~ "wood",
    species == "Pranus" ~ "Prunus",
    species %in% c("PPREM", "PREM") ~ "Prunus emarginata",
    species == "cynocerous" ~ "Cynosurus echinatus",
    species %in% c("striptanthis", "streptanthus tortuosus") ~ "Streptanthus tortuosus",
    species %in% c("Saxifrogacea", "saxifrogacea", "Saxifrage", "unk_saxifrage") ~ "Saxifragaceae",
    species %in% c("castilleja", "castelleja") ~ "Castilleja sp.",
    grepl("^QUWE_", species, ignore.case = TRUE) ~ "QUWI",
    TRUE ~ species
  ))

# Look at all unique hits
unique(spellcheck$species) # 483

# Convert common names to scientific 
known <- spellcheck %>% 
  mutate(species = case_when(
    #species %in% c("moss1", "moss2") ~ "moss", # do we want to count brophytes, lichens, etc???
    species == "cryptantha/plagiobothrys" ~ "Boraginaceae",
    species == "brome" ~ "Bromus",
    species == "fireweed" ~ "Chamerion angustifolium",
    species == "horsetail" ~ "Equisetum sp.",
    species == "rush" ~ "juncus",
    species == "unk_apeaceae" ~ "Apiaceae",
    species %in% c("elgl", "ELGL") ~ "Elymus glaucus",
    species %in% c("whisker plant", "whisker brush") ~ "Leptosiphon ciliatus",
    TRUE ~ species
  ))

# convert codes to scientific names
mutate(species == case_when(
  species == "BRTE" ~ "Bromus tectorum",
  species == "RIRO" ~ "Ribes roezlii",
  species == "CEIN" ~ "Ceanothus integerrimus",
  species == "CHFO" ~ "Chamaebatia foliosa",
  species -- "ARVI" ~ "Arctostaphylos viscida"
  
)
       )

# Look at all unique hits
unique(known$species) # 479

# lump all carex for now
carexcheck <- known %>% 
  mutate(species = case_when(
    grepl("^carex_", species, ignore.case = TRUE) ~ "Carex sp.",
    species == "sedge" ~ "Carex sp.",
    species == "carex" ~ "Carex sp.",
    species == "CAREXI" ~ "Carex sp.",
    TRUE ~ species
  ))
# Look at all unique hits
unique(carexcheck$species) # 424

# lump all unknown grasses for now
grassgroup <- carexcheck %>% 
  mutate(species = case_when(
    grepl("^grass_", species, ignore.case = TRUE) ~ "Poaceae",
    TRUE ~ species
  ))
# Look at all unique hits
unique(grassgroup$species) # 373

# lump all unknown forbs for now
forbfest <- grassgroup %>% 
  mutate(species = case_when(
    grepl("^forb_", species, ignore.case = TRUE) ~ "forb",
    species %in% c("forb 1", "Forb", "forbe", "forbe_1", "forbe_2", "forb_1", "forbe_unknown", "forb_unknown") ~ "forb",
    TRUE ~ species
  ))

# Look at all unique hits
unique(forbfest$species) # 315

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

unique(allLifeStages$species) # 283

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
