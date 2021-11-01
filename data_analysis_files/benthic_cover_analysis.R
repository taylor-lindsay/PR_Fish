# benthic coral cover analysis 

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
benthic_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data3/PRCRMP_Benthic-sessile_data_1999-2020_(updated_4-10-2020).csv', fileEncoding="latin1")
abundance_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data3/PRCRMP_Fish-inverts_abundance_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
biomass_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data3/PRCRMP_Fish-inverts_biomass_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
size_frequency_abundance_data3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data3/PRCRMP_Fish-inverts_size-frequency_abundance_data_1999-2020_(updated_10-04-2020).csv', fileEncoding="latin1")
site_classification_database3 <- read.csv('~/Desktop/GITHUB/PR_Fish/raw_data3/PRCRMP_Site_Classification_Database_(updated_3-8-2020).csv', fileEncoding="latin1")
species_info <- read.csv('~/Desktop/GITHUB/PR_Fish/data_analysis_files/species_info.csv')
#clean datasets

# Clean data labels 
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

names(site_classification_database3) <- toupper(names(site_classification_database3))

site_classification_database3$SITE.NAME <- site_classification_database3$SITE.NAME %>% 
  gsub("Berbera", "Berbería", .)  %>%
  gsub("Caa Gorda", "Caña Gorda", .) %>%
  gsub("Windward Reef ", "Windward Reef", .) %>%
  gsub("West Caballo Blanco (2001)", "West Caballo Blanco", .) %>%
  gsub("Maria Langa 5m ", "Maria Langa 5m", .) %>%
  gsub("Canal Luis Pea", "Canal Luis Peña", .) 

benthic_data3$SITE.NAME <- benthic_data3$SITE.NAME %>% 
  gsub("BerberÕa", "Berbería", .)  %>%
  gsub("CaÐa Gorda", "Caña Gorda", .) %>%
  gsub("Windward Reef ", "Windward Reef", .) %>%
  gsub("West Caballo Blanco (2001)", "West Caballo Blanco", .) %>%
  gsub("Maria Langa 5m ", "Maria Langa 5m", .) %>%
  gsub("Canal Luis PeÐa", "Canal Luis Peña", .) 

# Save just the columns I want & merge with site info 
benthic_totals <- benthic_data3 %>% dplyr:: select(grep("total", names(benthic_data3)),
      grep("YEAR", names(benthic_data3)),
      grep("LOCATION", names(benthic_data3)),
      grep("SITE.NAME", names(benthic_data3))) %>%
      full_join(., site_classification_database3, 
            by = "SITE.NAME")
    
# Graph average coral cover by year 

benthic_totals_means <- benthic_totals %>% filter(YEAR)







# filter down to just Cabo Rojo
# benthic_cabo_rojo <- benthic_totals %>% filter(LOCATION == "Cabo Rojo")

# add MPA data column
#benthic_cabo_rojo$MPA <- 
#if (benthic_cabo_rojo$SITE.NAME == "Resuellos") {benthic_cabo_rojo$MPA = "Yes"} 
  #else {benthic_cabo_rojo$MPA = "no"} benthic_cabo_rojo <- benthic_cabo_rojo %>%
  #mutate(MPA = case_when(
  #  benthic_cabo_rojo$SITE.NAME == "Resuellos" ~ "Recovered" ))


