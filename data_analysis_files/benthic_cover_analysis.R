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
