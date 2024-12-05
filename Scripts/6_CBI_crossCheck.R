
# install and load packages
#install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
# Bring in Data ----------------------------------------------------------
# read in data from script 4_UnderstorySpeciesRichness.R
understoryRich <- read.csv("prismFireRichness.csv")

# read in manually reviewed fire severity (MTBS, plot assessment, plot photos)
fireSeverityChecked <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/fireCompare_22052024.csv")
# read in data from PlotLevelData.R

understoryFire <- fireSeverityChecked %>% left_join(understoryRich) %>% select(-severity)

cbi <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/Data/CleanData/plot_CBI_data.csv")

understoryFire <- understoryFire %>% left_join(cbi, by = "plotID")

cbicheck <- understoryFire %>% select(Fire_Severity_Beg, Fire_Severity_plotAverge, mtbs_severity,predicted_CBI)

write.csv(cbicheck, "predictedCBI_ground_check.csv")

# Plot and Linear regression CBI versus understory richness ------------------------------------------------
# take a look at relationship btw richness and CBI fire severity
understoryFire %>%
  ggplot() + geom_dotplot(mapping = aes( x = predicted_CBI, y = richness))

plot(understoryFire$predicted_CBI, understoryFire$richness)
abline(lm(understoryFire$richness ~ understoryFire$predicted_CBI))
