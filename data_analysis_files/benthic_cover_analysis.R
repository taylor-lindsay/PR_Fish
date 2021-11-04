# benthic coral cover analysis 

# Libraries & Load Data ---------------------------------------------------

# Import Libraries 
#library(multcompView)
#library("ggpubr")
library(tidyverse)
#library(plyr)
#library(dplyr)
#library(broom)
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

#Find the mean for each location & year 
benthic_means <- benthic_data3 %>% 
  filter(!is.na(YEAR)) %>%
  #filter(!.$Abiotic..total. == "") %>%
  group_by(YEAR,SITE.NAME) %>%
  summarise_all(mean)

# Seperate just the totals, and join with site data 
benthic_means_totals <- benthic_means %>% dplyr:: select(grep("total", names(benthic_means)),
                                                   grep("YEAR", names(benthic_means)),
                                                   grep("SITE.NAME", names(benthic_means))) %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME") 





# Graphing Benthic Cover----------------------------------------------------------------


#benthic_means_totals %>% select(YEAR, SITE.NAME, Stony.Corals..total., Macroalgae..total., )
ggplot(benthic_means_totals) +
  geom_point(aes(YEAR, Stony.Corals..total.),stat = "summary", fun = "mean", color = "red") +
  geom_smooth(aes(YEAR, Stony.Corals..total.), method = "lm", se = FALSE, color = "red") +
  geom_point(aes(YEAR, Macroalgae..total.),stat = "summary", fun = "mean", color = "green") + 
  geom_smooth(aes(YEAR, Macroalgae..total.), method = "lm", se = FALSE, color = "green")

lm_stony_corals_total <- lm(Stony.Corals..total., Macroalgae..total. ~ YEAR, data = benthic_data3)
summary(lm_stony_corals_total)


# Graphing Parrot Fish abundance over time 
ggplot(parrotfish_abundance_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (per 30m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))