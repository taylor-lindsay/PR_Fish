# attempting to do some ANOVAs 
#install.packages("ggpubr")
#install.packages("multcompView")
library(multcompView)
library("ggpubr")
library(tidyverse)
library(plyr)
library(dplyr)
library(broom)
library(ggplot2)
benthic_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Benthic-sessile_data_1999-2020_(updated_4-10-2020).csv', fileEncoding="latin1")
abundance_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_abundance_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
biomass_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_biomass_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
size_frequency_abundance_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_size-frequency_abundance_data_1999-2020_(updated_10-04-2020).csv', fileEncoding="latin1")
site_classification_database3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Site_Classification_Database_(updated_3-8-2020).csv', fileEncoding="latin1")
species_info <- read.csv('~/PR_project/species_info.csv', fileEncoding="latin1")

total_column <- function(data) {
  df1 <- data[rowSums(is.na(data)) != ncol(data) - 7, ] #removes rows with all NA values 
  df2 <- df1 %>%
    mutate(total = rowSums( df1[,-c(1:7)],na.rm=TRUE)) #adds column of totals that excludes NAs 
  return(df2)} 

year_summary <- function(data) {
  data <- data %>%filter(!is.na(total))
  df_year <- ddply(data, "YEAR", summarise, 
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd / sqrt(N))
  return(df_year)}  


df_append <- function(df) {
  do.call(rbind, lapply(df, function(x) {
    cbind(get(x), source = x)
  }))}

p_val <- function(data, x, y) {
  model <- lm(x ~ y, data = data)
  p_value <- glance(model)$p.value
  p_value2 <- signif(p_value,3)
  return(p_value2)}

r_sqr <- function(data, x, y) {
  model <- lm(x ~ y, data = data)
  r_squared <- glance(model)$r.squared
  r_squared2 <- signif(r_squared,3)
  return(r_squared2) }


abundance_data3$DEPTH.ZONE <- abundance_data3$DEPTH.ZONE %>% 
  gsub("intermediate", "Intermediate", .) %>%
  gsub("shallow", "Shallow", .) %>%
  gsub("very shallow", "Very Shallow", .) %>%
  gsub("very Shallow", "Very Shallow", .) 

biomass_data3$DEPTH.ZONE <- biomass_data3$DEPTH.ZONE %>% 
  gsub("intermediate", "Intermediate", .) %>%
  gsub("shallow", "Shallow", .) %>%
  gsub("very shallow", "Very Shallow", .) %>%
  gsub("very Shallow", "Very Shallow", .) 

abundance_data3$SITE.NAME <- abundance_data3$SITE.NAME %>% 
  gsub("Berbera", "Berbería", .)  %>%
  gsub("Caa Gorda", "Caña Gorda", .) %>%
  gsub("Windward Reef ", "Windward Reef", .) %>%
  gsub("West Caballo Blanco (2001)", "West Caballo Blanco", .) %>%
  gsub("Maria Langa 5m ", "Maria Langa 5m", .) %>%
  gsub("Canal Luis Pea", "Canal Luis Peña", .) 

biomass_data3$SITE.NAME <- biomass_data3$SITE.NAME %>% 
  gsub("Berbera", "Berbería", .)  %>%
  gsub("Caa Gorda", "Caña Gorda", .) %>%
  gsub("Windward Reef ", "Windward Reef", .) %>%
  gsub("West Caballo Blanco (2001)", "West Caballo Blanco", .) %>%
  gsub("Maria Langa 5m ", "Maria Langa 5m", .) %>%
  gsub("Canal Luis Pea", "Canal Luis Peña", .) 

#abundance_data3 <- abundance_data3 %>% left_join(., MPA, by = "SITE.NAME")
#test <- abundance_data3 %>% select(SITE.NAME, Coral.Biotope)


# Parrot fish -------------------------------------------------------------

# Parrot fish species data 
parrotfish_species <- species_info %>%
  filter(c(Com.Eco == "Parrotfishes" | Family == "Scaridae"))

# Parrot fish abundance data 
parrotfish_abundance <- abundance_data3 %>%
  select(c(1:7, parrotfish_species[,2])) 
parrotfish_abundance <- total_column(parrotfish_abundance)
parrotfish_abundance_year <- year_summary(parrotfish_abundance)

#parrot fish biomass data 
parrotfish_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(parrotfish_species[,2]))) 
parrotfish_biomass <- total_column(parrotfish_biomass) 
parrotfish_biomass_year <- year_summary(parrotfish_biomass)

#combined summary 
parrotfish_summary <- full_join(parrotfish_abundance_year, parrotfish_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))


# Grouper -----------------------------------------------------------------

# Save just the snapper species info to a new df
grouper_species <- species_info %>%
  filter(Com.Eco == "Groupers")
#Grouper Abundance 
grouper_abundance <- abundance_data3 %>%
  select(c(1:7, one_of(grouper_species[,2])))
grouper_abundance <- total_column(grouper_abundance)
grouper_abundance_year <- year_summary(grouper_abundance)
#Grouper Biomass
grouper_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(grouper_species[,2]))) 
grouper_biomass <- total_column(grouper_biomass) 
grouper_biomass_year <- year_summary(grouper_biomass)
#summary
grouper_summary <- full_join(grouper_abundance_year, grouper_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))


# Snapper -----------------------------------------------------------------

# Save just the snapper species info to a new df
snapper_species <- species_info %>%
  filter(Com.Eco == "Snappers")
# snapper abundance 
snapper_abundance <- abundance_data3 %>%
  select(c(1:7, one_of(snapper_species[,2])))
snapper_abundance <- total_column(snapper_abundance)
snapper_abundance_year <- year_summary(snapper_abundance)
# snapper biomass
snapper_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(snapper_species[,2]))) 
snapper_biomass <- total_column(snapper_biomass) 
snapper_biomass_year <- year_summary(snapper_biomass)
# summary 
snapper_summary <- full_join(snapper_abundance_year, snapper_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))

# Depths  -----------------------------------------------------------------

#PARROTFISH X DEPTH X YEAR 

parrotfish_abundance2.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = parrotfish_abundance)
summary(parrotfish_abundance2.aov)
#plot(parrotfish_abundance.aov, 2)
TukeyHSD(parrotfish_abundance.aov)

parrotfish_biomass.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = parrotfish_biomass)
summary(parrotfish_biomass.aov)

ggboxplot(parrotfish_biomass, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")
ggboxplot(parrotfish_abundance, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")

#GOUPER X DEPTH X YEAR 
grouper_abundance.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = grouper_abundance)
summary(grouper_abundance.aov)
TukeyHSD(grouper_abundance.aov)
grouper_biomass.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = grouper_biomass)
summary(grouper_biomass.aov)

ggboxplot(grouper_biomass, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")
ggboxplot(grouper_abundance, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")

# SNAPPER X DEPTH X YEAR 
snapper_abundance.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = snapper_abundance)
summary(snapper_abundance.aov)
snapper_biomass.aov <- aov(total ~ DEPTH.ZONE*YEAR*LOCATION, data = snapper_biomass)
summary(snapper_biomass.aov)

ggboxplot(snapper_biomass, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")
ggboxplot(snapper_abundance, x = "DEPTH.ZONE", y = "total", 
          color = "DEPTH.ZONE",
          order = c("Very Shallow", "Shallow", "Intermediate", "Mesophotic"),
          ylab = "Weight", xlab = "Treatment")





parrotfish_abundance.aov <- aov(total ~ DEPTH.ZONE, data = parrotfish_abundance)
summary(parrotfish_abundance.aov)
#plot(parrotfish_abundance.aov, 2)
#TUKEY <- TukeyHSD(parrotfish_abundance.aov)

ANOVA <- function(data, x, y) {
  ANOVA_test <- aov(x ~ y, data = data)
  ANOVA_summary <- summary(ANOVA_test)
  return(ANOVA_summary)
}

generate_label_df <- function(ANOVA_model, variable){
  TUKEY <- TukeyHSD(ANOVA_model) # save tukey output 
  Tukey.levels <- TUKEY[[variable]][,4] #extract labels from tukey 
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters']) #extract letters from tukey 
  Tukey.labels$treatment=rownames(Tukey.labels) #label rows as group names 
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ] 
  return(Tukey.labels)
}

#testing 
parrotfish_abundance.aov <- aov(total ~ DEPTH.ZONE, data = parrotfish_abundance)
LABELS <- generate_label_df(parrotfish_abundance.aov, "DEPTH.ZONE")
print(LABELS)

ggplot(parrotfish_abundance, aes(x = DEPTH.ZONE, y=total)) +
  geom_boxplot() +
  stat_summary(geom = 'text', label = LABELS$Letters, fun.y = max, vjust = -1)


# MPAs --------------------------------------------------------------------

# isolate MPA data 
MPA <- site_classification_database3 %>%
  select(Site.Name, Station.Within.MPA., MPA.Fishing.Restrictions,Coral.Biotope) %>%
  unique() 
colnames(MPA)[1] <- "SITE.NAME"

# add MPA data to the two datasets 
parrotfish_abundance_MPA <- parrotfish_abundance %>% 
  left_join(., MPA, by = "SITE.NAME") %>%
  filter(!is.na(Station.Within.MPA.))
parrotfish_biomass_MPA <- parrotfish_biomass %>% 
  left_join(., MPA, by = "SITE.NAME") %>%
  filter(!is.na(Station.Within.MPA.))

parrotfish_abundance_MPA.aov <- aov(total ~ Station.Within.MPA., data = parrotfish_abundance_MPA)
summary(parrotfish_abundance_MPA.aov)
parrotfish_biomass_MPA.aov <- aov(total ~ Station.Within.MPA., data = parrotfish_biomass_MPA)
summary(parrotfish_biomass_MPA.aov)

# plot 
ggplot(parrotfish_abundance_MPA, aes(x = Station.Within.MPA., y=total)) +
  geom_boxplot() +
  theme_light() +
  labs(y="Mean Biomass (g/60m^2)", x = "MPA Status", title = "grouper Biomass ANOVA") 
ggplot(parrotfish_biomass_MPA, aes(x = Station.Within.MPA., y=total)) +
  geom_boxplot() +
  theme_light() +
  labs(y="Mean Biomass (g/60m^2)", x = "MPA Status", title = "grouper Biomass ANOVA") 

