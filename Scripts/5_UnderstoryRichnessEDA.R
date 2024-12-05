
# Bring in Data ----------------------------------------------------------
# read in data from script 4_UnderstorySpeciesRichness.R
understoryRich <- read.csv("prismFireRichness.csv")

# read in data from PlotLevelData.R
treelist <- read.csv("treelist.csv")
test <- treelist %>%  select(unique(plotID))
# join on the ground fire data with richness and other plot data
understoryRich <- left_join(understoryRich, treelist)

# rename to make the source of the mtbs data clear
understoryRich <- understoryRich %>% rename(mtbs_severity = severity)


# Boxplots for Fire Binary ------------------------------------------------

# take a look at relationship btw richness and MTBS fire severity
understoryRich %>% group_by(mtbs_severity) %>% 
  ggplot() + geom_boxplot(mapping = aes(mtbs_severity, richness))

understoryRich %>% group_by(plot_fireseverity) %>% 
  ggplot() + geom_boxplot(mapping = aes(plot_fireseverity, richness))

understoryRich$firebinary <- as.factor(understoryRich$firebinary)

# boxplot with notches for fire and no fire
understoryRich %>% 
  ggplot() + 
  geom_boxplot(mapping = aes(x = firebinary, y = richness), notch = TRUE) +
  xlab("Fire Occurrence") +
  ylab("Species Richness")

# identify outliers plots 9 and 54
outliers <- understoryRich %>% filter(richness > 35)

# calculate means and SDs for burned and unburned
understoryRich %>% group_by(firebinary) %>% 
  summarize(Mean = mean(richness), 
            SD = sd(richness))


# Test for Statistical Significance ---------------------------------------

# Subset the data into two groups: burned and unburned
burned_group <- subset(understoryRich, firebinary == 1)$richness
unburned_group <- subset(understoryRich, firebinary == 0)$richness
# Perform independent samples t-test
t_test_result <- t.test(burned_group, unburned_group)
# Print the result
print(t_test_result)

# Create a dataframe for plotting
plot_data <- data.frame(
  Group = c("Burned", "Unburned"),
  Richness = c(mean(burned_group), mean(unburned_group)),
  p_value = c("", format.pval(t_test_result$p.value))
)

# Plot boxplot with p-value
ggplot(understoryRich, aes(x = factor(firebinary), y = richness)) +
  geom_boxplot(notch = TRUE) +
  geom_text(data = plot_data, aes(x = Group, y = Richness, label = paste("p =", p_value)), vjust = -0.5) +
  xlab("Fire Occurrence") +
  ylab("Richness")

# Assuming understoryRich is your dataframe and fire_severity is the variable indicating fire severity (with levels low, medium, high)


# Boxplots for Fire Severity ----------------------------------------------
# need to convert fire severity back to a category/factor 
# Round the plot_fireseverity variable
understoryRich$rounded_fireseverity <- round(understoryRich$plot_fireseverity)

# Create a ggplot with rounded fire severity
plotsev <- ggplot(understoryRich, aes(x = as.factor(rounded_fireseverity), y = richness, fill = as.factor(rounded_fireseverity))) +
  geom_boxplot(notch = TRUE) +
  xlab("Plot Fire Severity") +
  ylab("Species Richness") +
  scale_fill_manual(values = c("0" = "blue", "1" = "yellow", "2" = "orange", "3" = "red")) + # Adjust colors as needed
  guides(fill = guide_legend(title = "Fire Severity"))

ggsave("RichFire_plotSeverity.png", plot = plotsev)

ggplot(understoryRich) +
  geom_dotplot(mapping = aes(x = plot_fireseverity, y = richness))

# look at how MTBS and on the groud fire variables compare
fireCompare <- understoryRich %>% select(plotID, mtbs_severity, plot_fireseverity, firebinary)
# write results to a csb
write.csv(fireCompare, "fireCompare.csv")

# look at trends in fire and climate variables
plot(understoryRich$plot_fireseverity, understoryRich$richness)
plot(as.numeric(understoryRich$mtbs_severity), understoryRich$richness)
plot(understoryRich$vpdmax, understoryRich$richness)
plot(understoryRich$tmean, understoryRich$richness)
plot(understoryRich$ppt, understoryRich$richness)

# should add plot as a random effect, but need to convert it back to numeric--maybe add year as a random effect?
# figure margins too large--may need to scale first before plotting and modeling
plot(understoryRich$vpdmax ~ understoryRich$tmean)

abline(lm(richness ~ severity + time_since + vpdmax + tmean + ppt, data = understoryRich))

# read in manually reviewed data
fireCompare <- read.csv("/Users/jennifercribbs/Documents/YOSE/Analysis/MultipleDisturbances/fireCompare_11052024JEC.csv") 

unique(fireCompare$mtbs_severity)

fireCompare <- fireCompare %>% mutate(mtbs_numeric = recode(mtbs_severity,
                                  "unburned" = 0,
                                  "low" = 1,
                                  "moderate" = 2,
                                  "high_severity" = 3,
                                  "high" = 3,
                                  ))
fireCompare <- fireCompare %>% mutate(difference = mtbs_numeric - plot_fireseverity)

fireCompareTrimmed <- fireCompare %>% select(mtbs_numeric, plot_fireseverity, difference, notes)

write.csv(fireCompareTrimmed, "fireCompareTrimmed.csv")

