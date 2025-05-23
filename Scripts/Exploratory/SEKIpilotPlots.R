library (tidyverse)

# read in preliminary data
roughResin <- read.csv("/Users/jennifercribbs/Documents/SEKI_beetles/ResinDuctRoughCount_SEKI2021.csv")

# data wrangling
pilotResin <- roughResin |>
  mutate(plot = str_sub(Core_ID, start = 3L, end = 5L), # correct data entry for PIMO0913A, also check than M11 is supposed to be N11
         tree_num = str_sub(Core_ID, start = -3L, end = -2),
         core = str_sub(Core_ID, start = -1L, end = -1L))
         
summary(roughResin)

# Look at scatterplot of resin ducts across years colored by species 
plot(roughResin$Calendar_Year, roughResin$Resin_Duct_Count, col = as.factor(roughResin$Species), pch = 16, ylab = "Resin Duct Count", xlab = "Year")

legend("topleft", legend = levels(as.factor(roughResin$Species)), col = 1:length(levels(as.factor(roughResin$Species))), pch = 16)

# Look at differences in median resin duct count by species
ggplot(roughResin, aes(x = Species, y = Resin_Duct_Count, fill = Species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Resin Duct Count by Species", x = "Species", y = "Resin Duct Count")
# there is a lot more variation in PILA resin duct counts, probably because PILA represents the majority of the samples

# Are there significant differences between species?
# check normality
shapiro.test(roughResin$Resin_Duct_Count)  # If n > 50, use visualizations instead

# Split the Core_ID column
roughResin <- roughResin %>%
  mutate(Aspect = substr(Core_ID, 3, 3),
         Plot = substr(Core_ID, 4, 5),
         Tree = substr(Core_ID, 6, 7),
         Core = substr(Core_ID, 8, 8))

# View the updated dataframe
print(roughResin)

unique(roughResin$Plot)
# probably 7 plots, I think 91 is an error
roughResin %>%  filter(Plot == "") # 51 blank lines
roughResin <- roughResin %>% filter(Plot != "")
roughResin %>%  filter(Plot == "91") # 52 that are missing aspect, so parsing is wrong
# Check in the lab that PM plot 09 is North facing then fix 
roughResin %>%  ifelse(Plot == "91", )

summary(roughResin)
roughResin %>% group_by(Plot) %>% summarize(Tree_count = n_distinct(Tree))

#Can use lme4 or brms 

#Fixed
#growth--simulate pick a mean, and some variance, and length of dataset
#species--categorical
#size--maybe join other tree data
#climate--from PRISM, maybe just preip to start


#Random effect of tree nested within plot
#plot | lme4
#1|plot/tree

#Resin_Duct_Count ~ growth + species + growth:species + climate + growth * climate

# Resin_Duct_Count ~ growth * species + growth * climate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# distribution right-skewed 
hist(roughResin$Resin_Duct_Count)
