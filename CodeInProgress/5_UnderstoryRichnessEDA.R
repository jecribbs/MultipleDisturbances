# read in data from script 4_UnderstorySpeciesRichness.R
understoryRich <- read.csv("prismFireRichness.csv")

boxplot(understoryRich$richness)
group_by(severity) %>% 
  ggplot() + geom_boxplot(mapping = aes(severity, richness))

# should add plot as a random effect, but need to convert it back to numeric--maybe add year as a random effect?
# figure margins too large--may need to scale first before plotting and modeling
plot(understoryRich$vpdmax ~ understoryRich$tmean)

abline(lm(richness ~ severity + time_since + vpdmax + tmean + ppt, data = understoryRich))
