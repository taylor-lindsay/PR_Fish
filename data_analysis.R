# Load Libraries & Data
library(tidyverse)
library(dplyr)
library(broom)
benthic_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Benthic-sessile_data_1999-2020_(updated_4-10-2020).csv', fileEncoding="latin1")
abundance_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_abundance_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
biomass_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_biomass_data_1999-2020_(updated_04-10-2020).csv', fileEncoding="latin1")
size_frequency_abundance_data3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Fish-inverts_size-frequency_abundance_data_1999-2020_(updated_10-04-2020).csv', fileEncoding="latin1")
site_classification_database3 <- read.csv('~/PR_project/raw_data3/PRCRMP_Site_Classification_Database_(updated_3-8-2020).csv', fileEncoding="latin1")
species_info <- read.csv('~/PR_project/species_info.csv')


# Functions ---------------------------------------------------------------

total_column <- function(data) {
  df1 <- data[rowSums(is.na(data)) != ncol(data) - 7, ] #removes rows with all NA values 
  df2 <- df1 %>%
    mutate(total = rowSums( df1[,-c(1:7)],na.rm=TRUE)) #adds column of totals that excludes NAs 
  return(df2)
} 

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

# Parrotfish Abundance  --------------------------------------------------------------

# Save just the parrotfish species info to a new df
parrotfish_species <- species_info %>%
  filter(c(Com.Eco == "Parrotfishes" | Family == "Scaridae"))

# Save just parrotfish abundance data & calculate the sum of all species 
parrotfish_abundance <- abundance_data3 %>%
  select(c(1:7, parrotfish_species[,2])) 
parrotfish_abundance <- parrotfish_abundance %>%
  mutate(total = rowSums( parrotfish_abundance[,8:22] ))

#Graphing the parrotfish 

#All species x year 
ggplot(parrotfish_abundance, aes(YEAR, total)) +
  geom_point() +
#  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE)

# Year Means 
parrotfish_abundance_year <- ddply(parrotfish_abundance, "YEAR", summarise, 
                               N    = length(total),
                               mean = mean(total),
                               sd   = sd(total),
                               se   = sd / sqrt(N))

#plot yearly mean 
ggplot(parrotfish_abundance_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (units)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))

#linear model for all parrotfishes based on means
model_parrotfish_abundance_year <- lm(YEAR ~ mean, data = parrotfish_abundance_year)
summary(model_parrotfish_abundance_year)
# p-value: 0.04612,	Adjusted R-squared:  0.2016 Multiple R2 = .2548

p_val(parrotfish_abundance_year, parrotfish_abundance_year$YEAR, parrotfish_abundance_year$mean)
r_sqr(parrotfish_abundance_year, parrotfish_abundance_year$YEAR, parrotfish_abundance_year$mean)

p_val(parrotfish_abundance, parrotfish_abundance$YEAR, parrotfish_abundance$total)
r_sqr(parrotfish_abundance, parrotfish_abundance$YEAR, parrotfish_abundance$total)










#linear model for all parroftishes based on totals: YEAR
model_parrotfish_abundance <- lm(total ~ YEAR, data = parrotfish_abundance)
summary(model_parrotfish_abundance)
#p-value: 7.19e-05, Adjusted R-squared:  0.01027 

#linear model for all parroftishes based on totals: REGION 
model_parrotfish_abundance <- lm(total ~ REGION, data = parrotfish_abundance)
summary(model_parrotfish_abundance)
#p-value: 4.742e-10, Adjusted R-squared:  0.03372 

#linear model for all parroftishes based on totals: DEPTH.ZONE
model_parrotfish_abundance <- lm(total ~ DEPTH.ZONE, data = parrotfish_abundance)
summary(model_parrotfish_abundance)
# p-value: 2.921e-06, Adjusted R-squared:  0.02072 

# Parrotfish Biomass ------------------------------------------------------

# Save just the parrotfish species info to a new df
parrotfish_species <- species_info %>%
  filter(c(Com.Eco == "Parrotfishes" | Family == "Scaridae"))

# Save just parrotfish abundance data & calculate the sum of all species 
parrotfish_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(parrotfish_species[,2]))) 
parrotfish_biomass <- parrotfish_biomass %>%
  mutate(total = rowSums( parrotfish_biomass[,8:15] )) %>%
  filter(!is.na(total))

#Graphing the parrotfish 

#All species x year 
ggplot(parrotfish_biomass, aes(YEAR, total)) +
  geom_point() +
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE)

# Biomass Year Means 
parrotfish_biomass_year <- ddply(parrotfish_biomass, "YEAR", summarise, 
                                   N    = length(total),
                                   mean = mean(total),
                                   sd   = sd(total),
                                   se   = sd / sqrt(N))

#plot yearly mean 
ggplot(parrotfish_biomass_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Biomass (units)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))      

#### From RMD 
parrotfish_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(parrotfish_species[,2]))) 
parrotfish_biomass <- total_column(parrotfish_biomass) 
parrotfish_biomass_year <- year_summary(parrotfish_biomass)

ggplot(parrotfish_biomass_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Biomass (g/60m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))


#linear model for all parrotfishes based on means
model_parrotfish_biomass_year <- lm(YEAR ~ mean, data = parrotfish_biomass_year)
summary(model_parrotfish_biomass_year)
# p-value: 0.4807,	Adjusted R-squared:  -0.08619 

#linear model for all parroftishes based on totals: YEAR
model_parrotfish_biomass_year <- lm(total ~ YEAR, data = parrotfish_biomass)
summary(model_parrotfish_abundance_year)
#p-value: 0.04612, Adjusted R-squared:  0.2016 

#linear model for all parroftishes based on totals: REGION 
model_parrotfish_abundance_region <- lm(total ~ REGION, data = parrotfish_biomass)
summary(model_parrotfish_abundance_region)
# p-value: 0.004254, Adjusted R-squared:  0.02393 

#linear model for all parroftishes based on totals: DEPTH.ZONE
model_parrotfish_abundance_depth <- lm(total ~ DEPTH.ZONE, data = parrotfish_biomass)
summary(model_parrotfish_abundance_depth)
# p-value: 0.612, Adjusted R-squared:  -0.002824


# Parrotfish abundance + biomass ------------------------------------------

parrotfish_summary <- full_join(parrotfish_abundance_year, parrotfish_biomass_year, by = 'YEAR', suffix = c(".abundance", ".biomass"))
# TO DO
    # Build the best model 
    # look at interactions of species 


# Snapper -----------------------------------------------------------------


# Save just the snapper species info to a new df
snapper_species <- species_info %>%
  filter(Com.Eco == "Snappers")

# Save just snapper abundance & biomass, find totals 
snapper_abundance <- abundance_data3 %>%
  select(c(1:7, snapper_species[,2])) 
snapper_abundance <- snapper_abundance %>%
  mutate(total = rowSums( snapper_abundance[,8:15] ))
snapper_biomass <- biomass_data3 %>%
  select(c(1:7, snapper_species[,2]))
snapper_biomass <- snapper_biomass %>%
  mutate(total = rowSums( snapper_biomass[,8:15] ))
snapper_biomass <- snapper_biomass %>%
  mutate(total = rowSums( snapper_biomass[,-c(1:7)])) 

snapper_abundance <- total_column(snapper_abundance)
snapper_abundance_year <- year_summary(snapper_abundance)

# in case this might help 
p_val <- function(data, x, y) {
  model <- lm(x ~ y, data = data)
  p_value <- glance(model)$p.value
  p_value2 <- signif(p_value,3)
  return(p_value2)}

#print(snapper_biomass[,-c(1:7)])       
         
# Year Means 
snapper_abundance_year <- ddply(snapper_abundance, "YEAR", summarise, 
                                   N    = length(total),
                                   mean = mean(total),
                                   sd   = sd(total),
                                   se   = sd / sqrt(N))


snapper_biomass_year <- ddply(snapper_biomass, "YEAR", summarise, 
                                N    = length(total),
                                mean = mean(total),
                                sd   = sd(total),
                                se   = sd / sqrt(N))

#plot yearly mean 
ggplot(snapper_abundance_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (units)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))

ggplot(snapper_biomass_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (units)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))


# Grouper -----------------------------------------------------------------


# Save just the snapper species info to a new df
grouper_species <- species_info %>%
  filter(Com.Eco == "Groupers")
grouper_abundance <- abundance_data3 %>%
  select(c(1:7, one_of(grouper_species[,2])))

# Calculate totals & summary table 
grouper_abundance <- total_column(grouper_abundance)
grouper_abundance_year <- year_summary(grouper_abundance)

# Plot 
ggplot(grouper_abundance_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (per 30m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))

#isolate grouper data 
grouper_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(grouper_species[,2]))) 
grouper_biomass <- total_column(grouper_biomass) 
grouper_biomass_year <- year_summary(grouper_biomass)

#plot grouper biomass
ggplot(grouper_biomass_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Biomass (g/60m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))

# All fish ----------------------------------------------------------------

# Save all fish species info to a new df
fish_species <- species_info %>%
  filter(Class == "Actinopterygii" | Class == "Elasmobranchii")
fish_abundance <- abundance_data3 %>%
  select(c(1:7, one_of(fish_species[,2])))

# Calculate totals & summary table 
fish_abundance <- total_column(fish_abundance)
fish_abundance_year <- year_summary(fish_abundance)

# Plot 
ggplot(fish_abundance_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Abundance (per 30m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))

#isolate snapper data 
fish_biomass <- biomass_data3 %>%
  select(c(1:7, one_of(fish_species[,2]))) 
fish_biomass <- total_column(fish_biomass) 
fish_biomass_year <- year_summary(fish_biomass)

ggplot(fish_biomass_year, aes(YEAR, mean)) +
  geom_line(linetype = "dashed", color="red") +
  geom_point() +
  theme_light() + 
  #  facet_wrap(~REGION) +
  geom_smooth(method = "lm", se = FALSE, color="red") +
  labs(y="Mean Biomass (g/60m^2)", x = "Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05))



# 3 fishes ----------------------------------------------------------------

fish_abundance_summary <- df_append(c("snapper_abundance_year", "parrotfish_abundance_year", "grouper_abundance_year"))
fish_biomass_summary <- df_append(c("snapper_biomass_year", "parrotfish_biomass_year", "grouper_biomass_year"))

ggplot(fish_abundance_summary, aes(YEAR, mean, color = source))+
  geom_line(linetype = "dashed") +
  geom_point(color="black") +
  theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05), color = "black")

ggplot(fish_biomass_summary, aes(YEAR, mean, color = source))+
  geom_line(linetype = "dashed") +
  geom_point(color="black") +
  theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05), color = "black")
  

# second axes  ------------------------------------------------------------

#attempting to make a plot comparing biomass & abundance of one fish type 

#snapper
snapper_summary <- full_join(snapper_abundance_year, snapper_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))

ggplot(snapper_summary, aes(x=YEAR))+
  geom_line(aes(y=mean.abundance * 10000),linetype = "dashed", color = "blue") +
  geom_line(aes(y=mean.biomass), linetype = "dashed", color = "red") +
  theme_light() +
  scale_y_continuous(name = "Biomass (red)", sec.axis = sec_axis(trans=~./10000, name = "Abundance (blue)"))

#grouper 
grouper_summary <- full_join(grouper_abundance_year, grouper_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))

ggplot(grouper_summary, aes(x=YEAR))+
  geom_line(aes(y=mean.abundance * 1000),linetype = "dashed", color = "blue") +
  geom_line(aes(y=mean.biomass), linetype = "dashed", color = "red") +
  theme_light() +
  scale_y_continuous(name = "Biomass (red)", sec.axis = sec_axis(trans=~./1000, name = "Abundance (blue)"))

#parrot 
parrotfish_summary <- full_join(parrotfish_abundance_year, parrotfish_biomass_year, by = "YEAR", suffix = c(".abundance", ".biomass"))

ggplot(parrotfish_summary, aes(x=YEAR))+
  geom_line(aes(y=mean.abundance * 100),linetype = "dashed", color = "blue") +
  geom_line(aes(y=mean.biomass), linetype = "dashed", color = "red") +
  theme_light() +
  scale_y_continuous(name = "Biomass (red)", sec.axis = sec_axis(trans=~./100, name = "Abundance (blue)"))



# Benthic substrate -------------------------------------------------------


stony_coral_species <- species_info %>%
  filter(c(Com.Eco == "Stony Coral spp." | Order == "Scleractinia"))

# Save just coral abundance data & calculate the sum of all species 
stony_coral_cover <- benthic_data3 %>%
  select(c(1:7, one_of(stony_coral_species[,2]))) 
stony_coral_cover_total <- stony_coral_cover %>%
  mutate(total = rowSums( stony_coral_cover[,8:58] )) #%>%
  filter(!is.na(total))

# pull out substrate totals 
total_names <- grep("total", names(benthic_data3), value = TRUE)
benthic_totals <- benthic_data3 %>%
  select(c(1:7, one_of(total_names))) 

benthic_totals_long <- pivot_longer(benthic_totals, 8:27, names_to = "CATEGORY", values_to = "PERCENT.COVER")
  
# Just the "Stony Corals Total" summary 
stony_corals_means <- benthic_totals %>%
  group_by(YEAR) %>%
  summarise(mean = mean(as.integer(Stony.Corals..total.)))

#graph just mean and year for the stony corals 
ggplot(stony_corals_means, aes(YEAR, mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#linear model total coral X year 
model_total_coral_year <- lm(as.integer(Stony.Corals..total.) ~ YEAR, data = benthic_totals)
summary(model_total_coral_year)
# p-value: 0.002482, Adjusted R-squared:  0.005688 
  
#linear model total coral X region
model_total_coral_region <- lm(as.integer(Stony.Corals..total.) ~ REGION, data = benthic_totals)
summary(model_total_coral_region)
# p-value: < 2.2e-16, Adjusted R-squared:  0.09714  

#linear model total coral X DEPTH
model_total_coral_depth <- lm(as.integer(Stony.Corals..total.) ~ DEPTH.ZONE, data = benthic_totals)
summary(model_total_coral_depth)
# p-value: 0.01281, Adjusted R-squared:  0.007973 
