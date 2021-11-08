# benthic coral cover analysis 

# Libraries & Load Data ---------------------------------------------------

# Import Libraries 
#library(multcompView)
#library("ggpubr")
library(tidyverse)
#library(plyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggpubr)

# import datasets
# Load Data 

benthic_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data_clean/PRCRMP_Benthic-sessile_data_1999-2020clean.csv', stringsAsFactors = FALSE)
abundance_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data_clean/PRCRMP_Fish-inverts_abundance_data_1999-2020clean.csv', stringsAsFactors = FALSE)
biomass_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data_clean/PRCRMP_Fish-inverts_biomass_data_1999-2020clean.csv', stringsAsFactors = FALSE)
size_frequency_abundance_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data_clean/PRCRMP_Fish-inverts_size-frequency_abundance_data_1999-2020clean.csv', stringsAsFactors = FALSE)
site_classification_database3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data_clean/PRCRMP_Site_Classification_Database_clean.csv')
species_info <- read.csv('~/Desktop/GITHUB/PR_Fish/data_analysis_files/species_info.csv')

#Find the class of each data point 
#sapply(benthic_data3, class)

# Prep for graphing  ----------------------------------------------------------

# pivot benthic 3 data 
benthic_longer <- benthic_data3%>% dplyr:: select(grep("^.+total.$", names(benthic_data3)),
                                                         grep("YEAR", names(benthic_data3)),
                                                         grep("SITE.NAME", names(benthic_data3)))%>%
  pivot_longer(cols = ends_with(c("total.", "erect.", "encrusting.")), names_to = "type", values_to = "percent") %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME")

#Find the mean for each location & year 
benthic_means <- benthic_longer %>% 
  filter(!is.na(percent)) %>%
  group_by(YEAR, type) %>%
  summarize(., mean_percent = mean(percent))

# Linear models of benthos cover over time  -----------------------------------------

# linear model output for each benthos type USING MEANS
by_type_means <- group_by(benthic_means_totals, type)
lm_benthic_totals <- do(by_type_means, glance(lm(YEAR ~ percent, data = .)))

# linear model output for each benthos type USING ALL DATA
by_type <- group_by(benthic_longer, type)
lm_benthic_totals_output_alldata <- do(by_type, glance(lm(YEAR ~ percent, data = .)))
write_csv(lm_benthic_totals_output_alldata, '~/Desktop/GITHUB/PR_Fish/Results/lm_benthic_totals_output_alldata.csv')

# interpretation of results: 
# The following cover types decreased significantly over the course of the study
# turf algae, recently dead coral, stony corals total, abiotic total
# The following cover types increased significantly over the course of the study 
# macroalgae, Peyssonneliaceae, CCA, sponges, cyanobacteria 

# graph benthos cover by type 
graph_benthic_types <- ggplot(benthic_means, aes(YEAR, mean_percent)) +
  geom_point() +
  geom_smooth( method = "lm", se = FALSE) +
  facet_wrap(~ type, scales = "free") +
  labs(y="Mean Percent Cover", x = "Year")
  
ggsave("graph_benthic_types.jpg", width = 10,
       height = 5, plot = graph_benthic_types, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# note that multiple graphs have a peak around 2005/2006, there was a bleaching event in 

# MPA status basic analysis  ----------------------------------------------

# filter data to focus on MPAs 
MPA_benthic_means <- benthic_longer %>% 
  filter(!is.na(percent)) %>%
  group_by(YEAR, type, STATION.WITHIN.MPA.) %>%
  summarize(., mean_percent = mean(percent))

# line graph comparing MPAs for each benthic type 
ggplot(MPA_benthic_means, aes(x = YEAR, y = mean_percent, color = STATION.WITHIN.MPA.)) +
  geom_point() + 
  geom_smooth( method = "lm", se = FALSE) +
  facet_wrap(~ type, scales = "free")

# boxplots comparing MPAs for each benthic type 
MPA_boxplot_types <- ggplot(MPA_benthic_means, aes(x = STATION.WITHIN.MPA., y = mean_percent)) +
  geom_boxplot() + 
  stat_compare_means(method = "t.test") + 
  labs(y="Mean Percent Cover", x = "MPA Status") + 
  facet_wrap(~ type, scales = "free") 

#save boxplots image
ggsave("MPA_boxplot_types.jpg", width = 8,
       height = 10, plot = MPA_boxplot_types, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# Parotfish abundance --------------------------------------------------------------------

# significantly more OUTSIDE of MPAs! 

# Pivot fish data & join with species & site data
abundance_longer_fish <- abundance_data3 %>% 
  pivot_longer(!c(LOCATION, YEAR, SAMPLE.CODE,X,REGION,SITE.NAME, DEPTH.ZONE,TRANSECT), names_to = "genus_species", values_to = "abundance") %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") %>%
  inner_join(., species_info, by = "genus_species") %>%
  filter(Class == c("Actinopterygii", "Elasmobranchii"))

# just look at parrotfish
parrotfish_species <- abundance_longer_fish %>%
  filter(c(Com.Eco == "Parrotfishes" | Family == "Scaridae"))

# find the total parotfish per transect across all species 
parrotfish_species_sum <- parrotfish_species %>%
  group_by(YEAR, SITE.NAME, TRANSECT) %>%
  summarise(., total_abundance = sum(abundance)) %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") 

# graph boxplot comparing MPA status 
MPA_parotfish_boxplot <- ggplot(parrotfish_species_sum, aes(x=STATION.WITHIN.MPA., y= total_abundance)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test") 
ggsave("MPA_parotfish_boxplot.jpg", width = 5,
       height = 8, plot = MPA_parotfish_boxplot, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# find means to determine which is significantly higher 
parrotfish_species_sum %>%
  group_by(STATION.WITHIN.MPA.) %>%
  filter(!is.na(total_abundance)) %>%
  summarize(mean = mean(total_abundance))
  

# Grouper Abundance -------------------------------------------------------

# significantly more in MPAS

# just look at grouper
grouper_species <- abundance_longer_fish %>%
  filter(Com.Eco == "Groupers")

# find the total parotfish per transect across all species 
grouper_species_sum <- grouper_species  %>%
  group_by(YEAR, SITE.NAME, TRANSECT) %>%
  summarise(., total_abundance = sum(abundance)) %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") 

# graph boxplot comparing MPA status 
MPA_grouper_boxplot <- ggplot(grouper_species_sum, aes(x=STATION.WITHIN.MPA., y= total_abundance)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test") 

ggsave("MPA_grouper_boxplot.jpg", width = 5,
       height = 8, plot = MPA_grouper_boxplot, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# find means to determine which is significantly higher 
grouper_species_sum %>%
  group_by(STATION.WITHIN.MPA.) %>%
  filter(!is.na(total_abundance)) %>%
  summarize(mean = mean(total_abundance))


# Snapper abundance -------------------------------------------------------

# no significant difference 

# just look at grouper
snapper_species <- abundance_longer_fish %>%
  filter(Com.Eco == "Snappers")

# find the total parotfish per transect across all species 
snapper_species_sum <- snapper_species  %>%
  group_by(YEAR, SITE.NAME, TRANSECT) %>%
  summarise(., total_abundance = sum(abundance)) %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") 

# graph boxplot comparing MPA status 
MPA_snapper_boxplot <- ggplot(snapper_species_sum, aes(x=STATION.WITHIN.MPA., y= total_abundance)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test") 

ggsave("MPA_snapper_boxplot.jpg", width = 5,
       height = 8, plot = MPA_snapper_boxplot, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# find means to determine which is significantly higher 
snapper_species_sum %>%
  group_by(STATION.WITHIN.MPA.) %>%
  filter(!is.na(total_abundance)) %>%
  summarize(mean = mean(total_abundance))


# Sharks abundance --------------------------------------------------------

# they found a total of 6 sharks over this entire data set, 
# and they were all found in MPAs, but likely not worth looking at 

# just look at shark
shark_species <- abundance_longer_fish %>%
  filter(Class == "Elasmobranchii")

# find the total shark per transect across all species 
shark_species_sum <- shark_species  %>%
  group_by(YEAR, SITE.NAME, TRANSECT) %>%
  summarise(., total_abundance = sum(abundance)) %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") 

# graph boxplot comparing MPA status 
MPA_shark_boxplot <- ggplot(shark_species_sum, aes(x=STATION.WITHIN.MPA., y= total_abundance)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test") 

ggsave("MPA_shark_boxplot.jpg", width = 5,
       height = 8, plot = MPA_shark_boxplot, path = '~/Desktop/GITHUB/PR_Fish/Results/')

# find means to determine which is significantly higher 
shark_species_sum %>%
  group_by(STATION.WITHIN.MPA.) %>%
  filter(!is.na(total_abundance)) %>%
  summarize(mean = mean(total_abundance))
