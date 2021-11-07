# benthic coral cover analysis 

# Libraries & Load Data ---------------------------------------------------

# Import Libraries 
#library(multcompView)
#library("ggpubr")
library(tidyverse)
library(plyr)
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

#Find the mean for each location & year 
benthic_means <- benthic_data3 %>% 
  filter(!is.na(YEAR)) %>%
  #filter(!.$Abiotic..total. == "") %>%
  group_by(YEAR,SITE.NAME) %>%
  summarise_all(mean)

benthic_longer <- benthic_data3%>% dplyr:: select(grep("^.+total.$", names(benthic_data3)),
                                                         grep("YEAR", names(benthic_data3)),
                                                         grep("SITE.NAME", names(benthic_data3)))%>%
  pivot_longer(cols = ends_with(c("total.", "erect.", "encrusting.")), names_to = "type", values_to = "percent") %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME")


# Seperate just the totals, and join with site data 
benthic_means_totals <- benthic_means %>% dplyr:: select(grep("^.+total.$", names(benthic_means)),
                                                   grep("YEAR", names(benthic_means)),
                                                   grep("SITE.NAME", names(benthic_means)))%>%
  pivot_longer(cols = ends_with(c("total.", "erect.", "encrusting.")), names_to = "type", values_to = "percent") %>%
  inner_join(., site_classification_database3, 
             by = "SITE.NAME")

# linear model

# linear model output for each benthos type USING MEANS
by_type <- group_by(benthic_means_totals, type)
lm_benthic_totals <- do(by_type, glance(lm(YEAR ~ percent, data = .)))

# linear model output for each benthos type USING ALL DATA
by_type <- group_by(benthic_means_totals, type)
lm_benthic_totals <- do(by_type, glance(lm(YEAR ~ percent, data = .)))


# Graphing Benthic Cover----------------------------------------------------------------


#benthic_means_totals %>% select(YEAR, SITE.NAME, Stony.Corals..total., Macroalgae..total., )
ggplot(benthic_means_totals) +
  geom_point(aes(YEAR, Stony.Corals..total.),stat = "summary", fun = "mean", color = "red") +
  geom_smooth(aes(YEAR, Stony.Corals..total.), method = "lm", se = FALSE, color = "red") +
  geom_point(aes(YEAR, Macroalgae..total.),stat = "summary", fun = "mean", color = "green") + 
  geom_smooth(aes(YEAR, Macroalgae..total.), method = "lm", se = FALSE, color = "green") +
  geom_point(aes(YEAR, Abiotic..total.),stat = "summary", fun = "mean", color = "black") +
  geom_smooth(aes(YEAR, Abiotic..total.), method = "lm", se = FALSE, color = "black") +
  geom_point(aes(YEAR, Abiotic..total.),stat = "summary", fun = "mean", color = "black") +
  geom_smooth(aes(YEAR, Abiotic..total.), method = "lm", se = FALSE, color = "black") +
  geom_point(aes(YEAR, CCA..total. ),stat = "summary", fun = "mean", color = "pink") +
  geom_smooth(aes(YEAR, CCA..total. ), method = "lm", se = FALSE, color = "pink") +
  geom_point(aes(YEAR, Turf.Algae..total.),stat = "summary", fun = "mean", color = "yellow") +
  geom_smooth(aes(YEAR, Turf.Algae..total.), method = "lm", se = FALSE, color = "yellow") +
  geom_point(aes(YEAR, Octocorals..total.erect.),stat = "summary", fun = "mean", color = "orange") +
  geom_smooth(aes(YEAR, Octocorals..total.erect.), method = "lm", se = FALSE, color = "orange") 

benthic_means_totals %>%
  group_by(YEAR) %>%
  summary(mean) %>%
  write_csv(benthic_means_totals, '~/Desktop/benthic_means_totals.csv')


lm_stony_corals_total <- lm(YEAR ~ Stony.Corals..total., data = benthic_data3)
summary <- summary(lm_stony_corals_total)

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