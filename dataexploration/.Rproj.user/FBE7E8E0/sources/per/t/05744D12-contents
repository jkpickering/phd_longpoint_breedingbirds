# 
# RStudio Script File ----------------------------------------------------------

# Project Title:     Statistical Analyses for Doctoral (PhD) Thesis
# Project Theme:     Breeding Bird Data Analysis
# Author:            Joshua Pickering
# Last Updated:      2024-12-30
# RStudio Version:   Size: 262.79 MB | SHA-256: 09E1E38A | Version: 2024.04.2+764 | Released: 2024-06-10

R.version

# ------------------------------------------------------------------------------
# Set up work file
# Set working directory --------------------------------------------------------
setwd("D:/PhDThesis_RWorkingDirectory/rmd")

# Load required packages (Alphabetically sorted)--------------------------------

library(ade4)
library(adespatial)
library(betareg)
library(brms)
library(data.table)
library(devtools)
library(dplyr)
library(effects)
library(emmeans)
library(ggalt)
library(ggConvexHull)
library(ggforce)
library(ggplot2)
library(ggpubr)
library(glmm)
library(gpboost)
library(grid)
library(lattice)
library(lme4)
library(lmtest)
library(memisc)
library(mgcv) 
library(mvabund)
library(patchwork)
library(plot3D)
library(plyr)
library(RColorBrewer)
library(readr)
library(rgl)
library(rlist)
library(rstan)
library(rstatix)
library(scales)
library(scatterplot3d)
library(tibble)
library(tidyquant)
library(tidyr) 
library(tidyverse)
library(vegan)
library(vegan3d)
# *Periodically review necessary installed packages


# ------------------------------------------------------------------------------
# Undertake GAM analysis with mgcv package: ------------------------------------
# Date: 2025-01-09-------------------------------------------------------------- 



test_GAM_data <- read.csv("TestGAMAnalysis_Bird_Data_2025_01_09.csv", header = T)

test_GAM <- gam(territoryabundance ~ s(sampleyear, by = speciescode) + s(woodcountbelowbrowse) + s(sitesize) + sitecode, 
                data = test_GAM_data,
                method = "REML")

test_GAM_data$speciescode <- as.factor(test_GAM_data$speciescode)

summary(test_GAM)

# Create new data for predictions
new_data <- data.frame(
  predictor = seq(min(test_GAM$predictor), max(test_GAM$predictor), length.out = 100),
  species = unique(test_GAM$species)
)

# Predict values
new_data$predicted <- predict(gam_model, newdata = new_data, type = "response")


test_GGPlot_GAM <- ggplot(test_GAM, aes(x = predictor, y = predicted, color = species)) +
  geom_line(size = 1) +
  labs(title = "GAM Predictions for Multiple Species",
       x = "Predictor",
       y = "Predicted Abundance") +
  theme_minimal()

plot(test_GAM)

test_GGPlot_GAM <- ggplot(test_GAM, aes())

?ggplot



# ggplot# ------------------------------------------------------------------------------
# Explore GAMs analysis with mgcv package: -------------------------------------
# Date: 2024-12-30-------------------------------------------------------------- 

# GAM with example fabricated dataset
set.seed(123)
bird_data <- data.frame(
  Year = rep(2001:2010, each = 20),
  Site = rep(letters[1:5], times = 40),
  Species = sample(c("Sparrow", "Robin", "Blackbird"), 200, replace = TRUE),
  Abundance = rpois(200, lambda = 10),
  Temperature = rnorm(200, mean = 20, sd = 2),
  VegetationCover = runif(200, 40, 70)
)

# Fit a GAM
gam_model <- gam(
  Abundance ~ s(Year) + s(Temperature) + s(VegetationCover) + Site,
  family = poisson(link = "log"), # Use Poisson for count data
  data = bird_data
)

summary(gam_model)

plot(gam_model, pages = 1)


# You can also use the model to predict bird abundance under new conditions or visualize temporal trends:
predicted <- predict(gam_model, newdata = new_data, type = "response")















# CHAT GPT EXAMPLE WITH CODE:

# Load required packages
library(mgcv)
library(ggplot2)

# Example data: Multi-species abundance across plots and years
data <- data.frame(
  Year = rep(2000:2005, each = 4),
  Plot = rep(c("Plot1", "Plot2"), each = 2, times = 6),
  Species = rep(c("SpeciesA", "SpeciesB"), times = 12),
  Abundance = c(52.3, 60.1, 55.8, 65.3, 60.1, 68.4, 67.2, 72.3, 71.3, 78.1, 80.0, 85.2, 
                62.5, 70.0, 68.0, 75.5, 73.0, 82.0, 77.5, 88.5, 85.0, 90.0, 92.0, 98.5),
  Temperature = rep(c(15.2, 15.5, 16.0, 16.8, 17.2, 18.0), each = 4),
  HabitatQuality = rep(c(7.8, 8.0, 8.1, 8.3, 8.5, 8.7), each = 4)
)

# Fit a GAM with separate smooth terms for each species
gam_model <- gam(Abundance ~ s(Year, by = Species, k = 5) + 
                   s(Temperature) + 
                   s(HabitatQuality) + 
                   Plot,
                 data = data,
                 method = "REML")

# To ensure proper use of `by`, make sure `Species` is a factor
data$Species <- as.factor(data$Species)

# Summary of the model
summary(gam_model)

# Predict abundance for visualization
new_data <- expand.grid(
  Year = seq(2000, 2005, length.out = 100),
  Plot = unique(data$Plot),
  Species = unique(data$Species),
  Temperature = mean(data$Temperature),
  HabitatQuality = mean(data$HabitatQuality)
)

new_data$Predicted_Abundance <- predict(gam_model, newdata = new_data)

# Plot predictions for each species in each plot
ggplot(new_data, aes(x = Year, y = Predicted_Abundance, color = Species)) +
  geom_line() +
  facet_wrap(~ Plot) +
  geom_point(data = data, aes(x = Year, y = Abundance, color = Species), alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Species Abundance Over Time by Plot and Species",
    x = "Year",
    y = "Predicted Abundance",
    color = "Species"
  )





gam_model <- gam(Abundance ~ s(Year, by = Species) + s(Temperature) + s(HabitatQuality) + Plot,
                 data = data,
                 method = "REML")


# EXAMPLE DELINEATION

ggplot(bird_data, aes(Temperature, Abundance)) +
  geom_point()

lm_y <- lm(Abundance ~ Temperature, data = bird_data)

ggplot(bird_data, aes(Temperature, Abundance)) +
  geom_point() +
  geom_smooth(method = lm)

plot(lm_y, which = 1)

gam_y <- gam(Abundance ~ s(Temperature), method = "REML")


x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)
y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))
Sample_data <- data.frame(y, x)

ggplot(Sample_data, aes(x, y)) +
  geom_point()

# Step 2: Aggregate data by Year and Site for community-level analysis
# Summarize total abundance and environmental averages for each site per year
community_data <- bird_data %>%
  group_by(Year, Site) %>%
  summarise(
    TotalAbundance = sum(Abundance),
    MeanTemperature = mean(Temperature),
    MeanVegetationCover = mean(VegetationCover),
    .groups = "drop" # Prevent grouped output
  )

# Check the aggregated data
head(community_data)

# Step 3: Fit a GAM model
# TotalAbundance is modeled as a function of time and environmental variables
gam_model <- gam(
  TotalAbundance ~ s(Year) + s(MeanTemperature) + s(MeanVegetationCover),
  family = poisson(link = "log"), # Appropriate for count data
  data = bird_data
)

# Step 4: Summarize and interpret the model
summary(gam_model)

# Step 5: Visualize smooth terms
par(mfrow = c(1, 3)) # Set layout for multiple plots
plot(gam_model, shade = TRUE, pages = 1)

# Step 6: Predict and visualize trends over time
# Create predictions based on the model
predicted <- data.frame(
  Year = community_data$Year,
  Fitted = predict(gam_model, type = "response")
)

# Plot predicted abundance trends over time
ggplot(predicted, aes(x = Year, y = Fitted)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Predicted Total Abundance Over Time",
    x = "Year",
    y = "Fitted Abundance"
  ) +
  theme_minimal()










# ------------------------------------------------------------------------------
# Explore fourth-corner analysis with ade4 package: ----------------------------
# Date: 2024-11-14--------------------------------------------------------------

R <- read.csv("R_env_variables_REVISED.csv", row.names = 1)

L <- read.csv("L_sp_abundance_REVISED.csv", row.names = 1)

Q <- read.csv("Q_sp_traits_NUMBER.csv", row.names = 1)

## SHOW SPECIES ABUNDANCE IN BOXPLOT

bird_spp <- mvabund(L)

plot(bird_spp)

## FOURTH CORNER ANALYSIS

testresult <- fourthcorner(R, L, Q, modeltype = 6, nrepet = 9999)

plot(testresult)

## TESTING RLQ ANALYSIS
## The RLQ analysis is performed by four successive steps:
## 1. a correspondence analysis (COA) on abundance matrix
## 2. a multivariate analysis on trait matrix using column weights from step 1
## 3. a multivariate analysis on environmental matrix using row weights from step 1
## 4. the RLQ analysis comparing the co-variance of the three previous steps with co-intertia analysis

afcL.aravo <- dudi.coa(L, scannf = FALSE)

acpQ.aravo <- dudi.hillsmith(Q, row.w = afcL.aravo$cw,
                             scannf = FALSE)

acpR.aravo <- dudi.hillsmith(R, row.w = afcL.aravo$lw,
                             scannf = FALSE)

rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)

plot(rlq.aravo)

summary(rlq.aravo)

t1 <- order(rlq.aravo$c1[,1])

dotchart(rlq.aravo$c1[t1,1], pch=16, 
         labels = names(Q)[t1])

abline(v=0, lty=2)

## TEST 2

afcL.aravo <- dudi.coa(L, scannf = FALSE)

acpQ.aravo <- dudi.acm(Q, row.w = afcL.aravo$cw,
                             scannf = FALSE)

acpR.aravo <- dudi.acm(R, row.w = afcL.aravo$cw,
                             scannf = FALSE)

rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)

plot(rlq.aravo)






coa.abundance <- dudi.coa(L, scannf = FALSE, nf=2)

summary(coa.abundance)

coa.abundance


mca.trait <- dudi.acm(Q, scannf = F,
                            row.w = coa.abundance$cw)

mca.trait <- dudi.acm(Q, scannf = F, row.w = coa.abundance$cw)



pca.trait <- dudi.pca(Q, scannf = FALSE, 
                      row.w = coa.abundance$cw)

pca.env <- dudi.pca(R, scannf = FALSE, 
                    row.w = coa.abundance$lw)

rlqF <- rlq(pca.env, coa.abundance, pca.trait, 
            scannf = FALSE)

summary(rlqF)

plot(rlqF)



result <- fourthcorner(R, L, Q, modeltype = 6, nrepet = 9999)

print(result)

plot(result)




t1 <- order(rlqF$c1[,1])

dotchart(rlqF$c1[t1,1], pch=16, 
         labels = names(trait)[t1])

abline(v=0, lty=2)
















# ------------------------------------------------------------------------------
# Explore and visualize data: --------------------------------------------------

# Import data set (read.csv function)
bird_data <- read.csv("bird_data.csv", header = TRUE)

# View data set (view function)
view(bird_data)

# Explore entire data set ------------------------------------------------------

# Explore dimensions (dim function) of data set 
dim(bird_data)

# Explore structure (str function) of data set 
str(bird_data)

# Explore similar to structure (glimpse function) of data set exploration
glimpse(bird_data)

# Identify column names of data set (names function)
names(bird_data)

# Identify number of column variables of data set (length function)
length(bird_data)

# General summary of data set (summary function)
summary(bird_data)

# Explore specific variables of the data set -----------------------------------

# Identify a variable's class type (class function)
class(bird_data$breeding_territories)

# Identify a variable's total length (length function)
length(bird_data$species_common_name)

# Identify all unique data points from a variable (unique function)
unique(bird_data$site_code_abcd)

# Identify occurrence of each data point within a variable (table function)
View(table(bird_data$site_code_abcd))

# View the occurrence of each data point within a variable (view and table function)
view(sort(table(bird_data$site_name), decreasing = TRUE))

# Explore the occurrence of data under a specific variable (barplot function)
barplot(sort(table(bird_data$species_common_name), decreasing = TRUE))

# Explore the occurrence of data under a specific variable (boxplot function)
boxplot(bird_data$breeding_territories)

# Explore the occurrence of data within bins (i.e., 0-5, 5-10) under a specific variable (hist function)
hist(bird_data$breeding_territories)

# ------------------------------------------------------------------------------
# Analyze: ---------------------------------------------------------------------
# Data set:       bird_species_richness_91_16_21
# Activity 1:     Species richness for all plots in sampling years: 1991, 2016 and 2021


# ------------------------------------------------------------------------------
# Example from MSc Data Analysis:

## Regression ggplot

# Import dataset from csv file
wtdeerpopdensity <- read.csv("wtdeer_popdensitiy.csv")

# Produce model with ggplot function
wtdeerpopdensity.plot <- ggplot(wtdeerpopdensity) +
  geom_point(aes(x = Year, y = Deer_density)) +
  geom_line(aes(x = Year, y = Deer_density)) +
  theme_classic() +
  ylab("White-tailed deer density (individuals/km )") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(breaks = seq(1870, 2050, by = 9))

# Plot model
wtdeerpopdensity.plot

# ------------------------------------------------------------------------------

# Import data set
bird_species_richness_91_16_21 <- read.csv("bird_species_richness_91_16_21.csv", header = TRUE)

# Review column names of data set
names(bird_species_richness_91_16_21)

# Species richness for all sites over time -------------------------------------
bird_species_richness_91_16_21%>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~sampling_year_yyyy, nrow = 1)+
  labs(title = "Species richness of breeding bird communities over time", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for all sites over time -------------------------------------
bird_species_richness_91_16_21%>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding bird communities over time", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# ------------------------------------------------------------------------------
# Analyze: ---------------------------------------------------------------------
# Data set:       bird_species_richness_91_16_21
# Activity 1:     Species richness for all plots in sampling years: 1991 - 2021

# Import data set
bird_species_richness <- read.csv("bird_species_richness.csv", header = TRUE)

# Review column names of data set
names(bird_species_richness)

# Species richness for all sites over time
bird_species_richness%>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding bird communities over time", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()



# ------------------------------------------------------------------------------
# Analyze: ---------------------------------------------------------------------
# Data set:       bird_species_richness_91_16_21
# Activity 1:     Species richness for individual plots in sampling years: 1991 - 2021

# Species richness for individual sites over time ------------------------------
# BGGR

bird_species_richness%>%
  filter(site_code_abcd == "BGGR") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Bluegrass - Milkweed Grassland (BGGR)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# DCJS

bird_species_richness%>%
  filter(site_code_abcd == "DCJS") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Dry Cottonwood - Juniper Savannah (DCJS)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# DCSD

bird_species_richness%>%
  filter(site_code_abcd == "DCSD") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Dry Cottonwood Sand Dune (DCSD)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# IDSS

bird_species_richness%>%
  filter(site_code_abcd == "IDSS") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Intergrading Dune - Swale Savannah (IDSS)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# RARO

bird_species_richness%>%
  filter(site_code_abcd == "RARO") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Ash - Red Oak Savannah (RARO)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# ROIS

bird_species_richness%>%
  filter(site_code_abcd == "ROIS") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Oak - Ironwood Savannah (ROIS)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# ROMF

bird_species_richness%>%
  filter(site_code_abcd == "ROMF") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Oak - Sugar Maple Forest (ROMF)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# ROMS

bird_species_richness%>%
  filter(site_code_abcd == "ROMS") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Oak - Sugar Maple Savannah (ROMS)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# ROWB

bird_species_richness%>%
  filter(site_code_abcd == "ROWB") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Oak - White Birch Savannah (ROWB)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# ROWP

bird_species_richness%>%
  filter(site_code_abcd == "ROWP") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Red Oak - White Pine Savannah (ROWP)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# SRS1

bird_species_richness%>%
  filter(site_code_abcd == "SRS1") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Sedge - Rush Swale #1 (SRS1)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# SRS2

bird_species_richness%>%
  filter(site_code_abcd == "SRS2") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Sedge - Rush Swale #2 (SRS2)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# STDP

bird_species_richness%>%
  filter(site_code_abcd == "STDP") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Sedge - Tamarack Dune Pond (STDP)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# TMSL

bird_species_richness%>%
  filter(site_code_abcd == "TMSL") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: Tamarack Slough (TMSL)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()

# Species richness for individual sites over time ------------------------------
# WPWC

bird_species_richness%>%
  filter(site_code_abcd == "WPWC") %>%
  ggplot(aes(sampling_year_yyyy, species_richness,
             colour = site_code_abcd))+
  geom_point(aes(size = site_size_ha))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species richness of breeding birds over time: White Pine - White Cedar Savannah (WPWC)", 
       x = "Year", 
       y = "Species richness")+
  theme_bw()








# Analyze data: ----------------------------------------------------------------

# Activity 1:     Species Diversity
# Format:         Calculated diversity for sites in given years

# Import data set
bb_data_diversity <- read.csv("bb_data_table.csv", header = TRUE)

# Review column names of data set
names(bb_data_diversity)

str(bb_data_diversity)

bb_sp_curve <- specaccum(comm = bb_data_diversity,
                         method = "random",
                         permutations = 100)

plot(bb_sp_curve)

bb_sp_curve <- specaccum(comm = bb_data_diversity,
                         method = "collector",
                         permutations = 100)

plot(bb_sp_curve)

bb_sp_curve <- specaccum(comm = bb_data_diversity,
                         method = "exact",
                         permutations = 100)

plot(bb_sp_curve)

bb_species_richness <- specnumber(bb_data_diversity)

bb_species_richness

mean(bb_species_richness)

sd(bb_species_richness)

bb_shannon <- diversity(bb_data_diversity, index = "shannon")

head(bb_shannon)

bb_shannon_

plot(bb_shannon)

# Import env data set
bb_data_env <- read.csv("bb_data_table_env.csv", header = TRUE)

head(bb_data_env)

# To plot the output from the mds using ggplot a new datasheet needs to be created which contains the x,y points for each site. You can do this by calling the scores of you mds.

bb_shannon_df <- as.data.frame(bb_shannon) #save NMDS results into dataframe

bb_shannon_df <- cbind(bb_shannon_df, Year = bb_data_env$sampling_year_yyyy) #add grouping variable "Management" to dataframe

bb_shannon_df <- cbind(bb_shannon_df, Size = bb_data_env$site_size_ha)

bb_shannon_df <- cbind(bb_shannon_df, Site_Code = bb_data_env$site_code_abcd)

bb_shannon_df <- cbind(bb_shannon_df, Site_Name = bb_data_env$site_name)

bb_shannon_df <- cbind(bb_shannon_df, Year_Site = bb_data_env$unique)

bb_shannon_df

bb_shannon_df%>%
  ggplot(aes(Year, bb_shannon,
           colour = Site_Code))+
  geom_point(aes(size = Size))+
  geom_smooth(method = lm, se = F)+
  labs(title = "Species diversity of breeding birds over time", 
       x = "Year", 
       y = "Shannon Index of Diversity")+
  theme_bw()


bb_shannon_plot <- ggplot(bb_shannon_df,
                          aes(x = "Year_Site",
                              y = "bb_shannon"))+
  coord_fixed()+
  theme_classic()+ 
  geom_point()+
  geom_encircle(aes(fill = "Size"), s_shape = 1, expand = 0,
                alpha = 0.2, color = "black", show.legend = FALSE)

bb_shannon_plot + labs(title = "Basic ordination plot",
                       x = "Year x Site",
                       y = "Shannon index of diversity") #displays plot





# Analyze data: ----------------------------------------------------------------

# Activity 1:     Community Composition
# Format:         Individual NMDS Plots for given year; 1991, 2016, and 2021

# Import data set from csv file
bb_territories_matrix <- read.csv("bb_territories_matrix_91_16_21.csv", header = TRUE)

# NMDS analysis of community data
bb_nmds_results <- metaMDS(comm = bb_territories_matrix[ , 4:86],
                           distance = "bray",
                           try = 100)

# Add NMDS results with original data through column binding
bb_territories_matrix_bind = bind_cols(bb_territories_matrix, bb_nmds_results$points)

# Use ggplot function to plot NMDS results by year
bb_territories_matrix_bind %>% 
  ggplot(aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_convexhull(aes(colour = sampling_year_yyyy), alpha = 0.3) +
  facet_grid(.~ sampling_year_yyyy) +
  coord_fixed()

# Use ggplot function to plot NMDS results for all years
bb_distance_matrix_1991 = bb_territories_matrix_bind %>% 
  dplyr::filter(sampling_year_yyyy != 1991)

bb_distance_matrix_2016_2021 = bb_territories_matrix_bind %>% 
  dplyr::filter(sampling_year_yyyy == 1991)

# Use ggplot function to plot NMDS results for all years compared to 1992 with hulls
bb_distance_matrix_1991 %>% 
  group_by(sampling_year_yyyy) %>% 
  nest() %>% 
  pull() %>% 
  map(function(ind.year){
    ggplot(NULL, aes(x = MDS1, y = MDS2)) +
      geom_point(data = bb_distance_matrix_2016_2021, 
                 color = "black") +
      geom_convexhull(data = bb_distance_matrix_2016_2021, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.2) +
      geom_point(data = ind.year,
                 color = "blue") +
      geom_convexhull(data = ind.year, 
                      color = "blue", 
                      fill = "grey",
                      alpha = 0.2) +
      coord_fixed() +
      xlim(c(-2.5, 2.5)) +
      ylim(c(-2.0, 2.0)) +
      xlab("NMDS1") +
      ylab("NMDS2") +
      theme_set(theme_classic(base_size = 20)) +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 1, 
                                            linetype = "solid")
      )
  })














# NMDS #2

set.seed(2)
community_matrix <- matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS <- metaMDS(community_matrix, # Our community-by-species matrix
                        k=2, trace = 0) # The number of reduced dimensions

data.scores <- as.data.frame(scores(example_NMDS))

data.scores$site <- rownames(data.scores)

data.scores

data.scores$grp <- rep(c("1", "2"), each = 5)

species.scores <- as.data.frame(scores(example_NMDS, "species"))

species.scores$species <- rownames(species.scores)

ggplot() + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha=1) +  # add the species labels
  geom_point(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, shape = as.factor(grp), colour=as.factor(site))) + # add the point markers
  geom_text(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, label = site, colour=as.factor(site)), vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("1" = "red", "2" = "blue")) +
  coord_equal() +
  theme_bw()




# NMDS with my data

# Import data set from csv file
bb_territories_matrix <- read.csv("bb_territories_matrix_91_16_21.csv", header = TRUE)

bb_territories_NMDS <- metaMDS(comm = bb_territories_matrix[1:45, 4:86],
                               distance = "bray",
                               try = 100,
                               k = 2,
                               trace = 0)

plot(bb_territories_NMDS)

print(bb_territories_NMDS)

str(bb_territories_NMDS)

bb_territories_NMDS$points %>% head()

species_scores1 <- as.data.frame(bb_territories_NMDS$species)



species_scores1$site <- rownames(species_scores1)



data.scores$grp <- rep(c("1", "2"), each = 5)

species.scores <- as.data.frame(scores(example_NMDS, "species"))

species.scores$species <- rownames(species.scores)

ggplot() + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha=1) +  # add the species labels
  geom_point(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, shape = as.factor(grp), colour=as.factor(site))) + # add the point markers
  geom_text(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, label = site, colour=as.factor(site)), vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("1" = "red", "2" = "blue")) +
  coord_equal() +
  theme_bw()












## TESTING

# Import data set from csv file
bb_territories_matrix <- read.csv("bb_territories_matrix_91_16_21.csv", header = TRUE)

bb_territories_NMDS <- metaMDS(comm = bb_territories_matrix[1:45, 4:86],
                               distance = "bray",
                               try = 100,
                               k = 2,
                               trace = 0)

plot(bb_territories_NMDS)

print(bb_territories_NMDS)

str(bb_territories_NMDS)

bb_territories_NMDS$points %>% head()

ggplot() + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha=1) +  # add the species labels
  geom_point(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, shape = as.factor(grp), colour=as.factor(site))) + # add the point markers
  geom_text(data = data.scores,aes(x = species.NMDS1, y = species.NMDS2, label = site, colour=as.factor(site)), vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("1" = "red", "2" = "blue")) +
  coord_equal() +
  theme_bw()
















# ------------------------------------------------------------------------------

# FUNCTIONAL

# NMDS for GGPLOT by data and env data

bb_community_data <- read.csv("bbc_91-16-21_community_data.csv", header = TRUE)

bb_community_env <- read.csv("bbc_91-16-21_community_env.csv", header = TRUE)


bb_community_nmds <- metaMDS(bb_community_data, distance = "bray", autotransform = F)

bb_community_envfit <- envfit(bb_community_nmds, bb_community_env, permutations = 999) # this fits environmental vectors

bb_community_spp_fit <- envfit(bb_community_nmds, bb_community_data, permutations = 999) # this fits species vectors


# To plot the output from the mds using ggplot a new datasheet needs to be created which contains the x,y points for each site. You can do this by calling the scores of you mds.

site.scrs <- as.data.frame(scores(bb_community_nmds, display = "sites")) #save NMDS results into dataframe

site.scrs <- cbind(site.scrs, Year = bb_community_env$sampling_year_yyyy) #add grouping variable "Management" to dataframe

site.scrs <- cbind(site.scrs, Site = bb_community_env$site_code_abcd) #add grouping variable of cluster grouping to dataframe

#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

head(site.scrs)


spp.scrs <- as.data.frame(scores(bb_community_spp_fit, display = "vectors")) #save species intrinsic values into dataframe

spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe

head(spp.scrs)


bb_nmds_plot <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2))+#adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Year", shape = "Site")+ # add legend labels for Management and Landuse
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10))

bb_nmds_plot + labs(title = "Basic ordination plot") #displays plot

bb_nmds_plot+
  geom_segment(data = spp.scrs,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), 
               colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species),
                           cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with species vectors")+
  geom_convexhull(aes(), alpha = 0.3) +
  geom_point(data = spp.scrs, aes(NMDS1, NMDS2))+
  facet_grid(.~ Year)









# Example for convex hull
library(ggplot2)
library(plyr)

# some fake data:
mydata <- data.frame(Taxa = c('Cha','Cha','Cha','Cha','Cha','Cha','Hyd','Hyd','Hyd','Hyd','Hyd','Hyd'),
                     dn = c(10.2,10.7,4.9,5.4,8.6,8.0, 6.6,10.2,9.7,8.1,8.8,8.7),
                     dc =c(-20.4,-19.7,-21.0,-20.6,-21.2,-20.9,-19.2,-17.0,-18.2,-16.5,-15.8,-15.8))

# calculate convex hulls:
chulls <- ddply(mydata, .(Taxa), function(mydata) mydata[chull(mydata$dn, mydata$dc), ])

# plot them:
ggplot(data=mydata, aes(x=dn, y=dc, color=Taxa)) + geom_point() +
  geom_polygon(data=chulls, aes(x=dn, y=dc, fill=Taxa, alpha=0.2)) 










# You have to set the seed _before_ you generate random data, not after
set.seed(1) 
dt <- data.table(xdata=runif(15), ydata=runif(15), level=rep(c("a","b","c"), each=5),
                 key="level")

str(dt)

hulls <- site.scrs[, .SD[chull(NMDS1, "NMDS2")], by = "Site "]


hulls2 <- site.scrs[, .SD[chull(NMDS1, NMDS2)], by = "Year "]

str(site.scrs)







view(iris)

hull <- chull(NMDS1, NMDS2)
site.scrs %>% group_by(Year) %>%
  slice
chull(NMDS1, NMDS2)

plot + geom_polygon(data = hull, alpha = 0.2, 
                    aes(fill = Species,colour = Species))


geom_convexhull(data = site.scrs,
                group_by(Year), 
                color = "black", 
                fill = "white",
                alpha = 0.2)


geom_polygon(data = site.scrs, aes(x = NMDS1, y = NMDS2, group = Year, fill = Year))

geom_polygon(aes(fill = Year, group = Year))# add legend at right of plot







# Example data preparation
# Assume you have a dataframe `bird_data` with columns: 'Year', 'Site', 'Species', 'Count'
# For simplicity, let's create some example data
set.seed(42)
bird_data <- data.frame(
  Year = rep(2018:2020, each = 100),
  Site = rep(paste0("Site", 1:10), each = 10, times = 3),
  Species = rep(paste0("Species", 1:10), times = 30),
  Count = sample(1:50, 300, replace = TRUE)
)

view(bird_data)

# Prepare the data for NMDS
# Aggregate counts by Year, Site, and Species
bird_agg <- aggregate(Count ~ Year + Site + Species, data = bird_data, sum)

view(bird_agg)

# Spread the data to wide format for each year
library(tidyr)
bird_wide <- bird_agg %>%
  pivot_wider(names_from = Species, values_from = Count, values_fill = list(Count = 0))

view(bird_wide)

# Separate data by year
bird_2018 <- bird_wide[bird_wide$Year == 2018, -1]
bird_2019 <- bird_wide[bird_wide$Year == 2019, -1]
bird_2020 <- bird_wide[bird_wide$Year == 2020, -1]

# Function to perform NMDS and plot
plot_nmds <- function(data, year) {
  nmds <- metaMDS(data, distance = "bray", k = 2, trymax = 100)
  nmds_data <- as.data.frame(scores(nmds))
  nmds_data$Site <- rownames(nmds_data)
  
  p <- ggplot(nmds_data, aes(x = NMDS1, y = NMDS2, label = Site)) +
    geom_point() +
    geom_text(vjust = 2) +
    ggtitle(paste("NMDS Plot for Year", year)) +
    theme_minimal()
  
  return(p)
}

# Plot NMDS for each year
plot_2018 <- plot_nmds(bird_2018, 2018)
plot_2019 <- plot_nmds(bird_2019, 2019)
plot_2020 <- plot_nmds(bird_2020, 2020)

# Combine plots side by side
grid.arrange(plot_2018, plot_2019, plot_2020, ncol = 3)


# ------------------------------------------------------------------------------
# CHATGPT #2

# Example data preparation
set.seed(42)
bird_data <- data.frame(
  Year = rep(2018:2020, each = 100),
  Site = rep(paste0("Site", 1:10), each = 10, times = 3),
  Species = rep(paste0("Species", 1:10), times = 30),
  Count = sample(1:50, 300, replace = TRUE)
)

# Prepare the data for NMDS
bird_agg <- aggregate(Count ~ Year + Site + Species, data = bird_data, sum)

library(tidyr)
bird_wide <- bird_agg %>%
  pivot_wider(names_from = Species, values_from = Count, values_fill = list(Count = 0))

# Combine data for NMDS
bird_combined <- bird_wide[,-1]
rownames(bird_combined) <- paste(bird_wide$Year, bird_wide$Site, sep = "_")

# Run NMDS on the combined data
nmds <- metaMDS(bird_combined[, 2:10], distance = "bray", k = 2, trymax = 100)

nmds_data <- as.data.frame(scores(nmds)$sites)

nmds_data$Year <- sapply(strsplit(rownames(nmds_data), "_"), `[`, 1)

nmds_data$Site <- sapply(strsplit(rownames(nmds_data), "_"), `[`, 2)

# Function to plot NMDS for a specific year
plot_nmds_year <- function(data, year) {
  data_year <- data[data$Year == year,]
  p <- ggplot(data_year, aes(x = NMDS1, y = NMDS2, label = Site)) +
    geom_point() +
    geom_text(vjust = 2) +
    ggtitle(paste("NMDS Plot for Year", year)) +
    theme_minimal()
  return(p)
}

# Plot NMDS for each year
plot_2018 <- plot_nmds_year(nmds_data, 2018)
plot_2019 <- plot_nmds_year(nmds_data, 2019)
plot_2020 <- plot_nmds_year(nmds_data, 2020)

plot_2018

# Combine plots side by side
combined_plot <- plot_2018 + plot_2019 + plot_2020 + plot_layout(ncol = 3)
print(combined_plot)


# ------------------------------------------------------------------------------

# Example data preparation
set.seed(42)
bird_data <- data.frame(
  Year = rep(2018:2020, each = 100),
  Site = rep(paste0("Site", 1:10), each = 10, times = 3),
  Species = rep(paste0("Species", 1:10), times = 30),
  Count = sample(1:50, 300, replace = TRUE)
)

# Prepare the data for NMDS
bird_agg <- aggregate(Count ~ Year + Site + Species, data = bird_data, sum)

library(tidyr)
bird_wide <- bird_agg %>%
  pivot_wider(names_from = Species, values_from = Count, values_fill = list(Count = 0))

# Combine data for NMDS
bird_combined <- bird_wide[,-1]
rownames(bird_combined) <- paste(bird_wide$Year, bird_wide$Site, sep = "_")

# Run NMDS on the combined data
nmds <- metaMDS(bird_combined [, 2:10], distance = "bray", k = 2, trymax = 100)
nmds_data <- as.data.frame(scores(nmds)$Site)
nmds_data$Year <- sapply(strsplit(rownames(nmds_data), "_"), `[`, 1)
nmds_data$Site <- sapply(strsplit(rownames(nmds_data), "_"), `[`, 2)


# Function to plot NMDS for a specific year
plot_nmds_year <- function(data, year) {
  data_year <- data[data$Year == year,]
  p <- ggplot(data_year, aes(x = NMDS1, y = NMDS2, label = Site, color = Year)) +
    geom_point() +
    geom_text(vjust = 2, show.legend = FALSE) +
    ggtitle(paste("NMDS Plot for Year", year)) +
    theme_minimal() +
    scale_color_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "green"))
  return(p)
}

# Plot NMDS for each year
plot_2018 <- plot_nmds_year(nmds_data, "2018")
plot_2019 <- plot_nmds_year(nmds_data, "2019")
plot_2020 <- plot_nmds_year(nmds_data, "2020")

comb_plot1 <- plot_2018 + plot_2019 + plot_2020

# Combine plots side by side using patchwork
combined_plot <- plot_2018 + plot_2019 + plot_2020 + plot_layout(ncol = 3)
print(combined_plot)
























# ------------------------------------------------------------------------------


# Activity 1:     Species richness
# Format:         Individual NMDS Plots for given year; 1991, 2016, and 2021

bird_data_table <- read.csv("bird_data_table.csv", header = TRUE)

view(bird_data_table)










# Activity 1:     NMDS
# Format:         Individual NMDS Plots for given year; 1991, 2016, and 2021

# Import data set from csv file
bird_community_data <- read.csv("bbc_91-16-21_community.csv", header = TRUE)
view(bird_community_data)

# NMDS analysis of community data
bbc.nmdsresults <- metaMDS(comm = bbc.speciescomposition.data[ , 3:85],
                           distance = "bray",
                           try = 100)

# Add NMDS results with original data through column binding
bbc.nmdsmatrix = bind_cols(bbc.speciescomposition.data, bbc.nmdsresults$points)

# Use ggplot function to plot NMDS results for all years
ws_distance_matrix_1992 = bbc.nmdsmatrix %>% 
  dplyr::filter(sampling_year_yyyy != 1991)

ws_distance_1992 = bbc.nmdsmatrix %>% 
  dplyr::filter(sampling_year_yyyy == 1991)

# Use ggplot function to plot NMDS results for all years compared to 1992 with hulls
ws_distance_matrix_1992 %>% 
  group_by(sampling_year_yyyy) %>% 
  nest() %>% 
  pull() %>% 
  map(function(ind.year){
    ggplot(NULL, aes(x = MDS1, y = MDS2)) +
      geom_point(data = ws_distance_1992, 
                 color = "black") +
      geom_convexhull(data = ws_distance_1992, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.2) +
      geom_point(data = ind.year,
                 color = "blue") +
      geom_convexhull(data = ind.year, 
                      color = "black", 
                      fill = "black",
                      alpha = 0.2) +
      coord_fixed() +
      xlim(c(-2.0,2.5)) +
      ylim(c(-1.5, 1.5)) +
      xlab("NMDS1") +
      ylab("NMDS2") +
      theme_set(theme_classic(base_size = 20)) +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 1, 
                                            linetype = "solid")
      )
  })







## ADDITIONAL 3-Pane Viewer of NMDS Plots

# Use ggplot function to plot NMDS results by year
bbc.nmdsmatrix %>% 
  ggplot(aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_convexhull() +
  facet_grid(.~ sampling_year_yyyy) +
  coord_fixed()


---------------------------------------------------------------

# Vegetation cover data set
# Species composition change over time
# NMDS ordination
# metaMDS function from Vegan package
# Functional
# 2022-07-15

# Input data separately for data and env:
bbc.communitycomposition.data <- read.csv("bbc_91-16-21_community_data.csv")

str(bbc.communitycomposition.data)
summary(bbc.communitycomposition.data)

bbc.communitycomposition.env <- read.csv("bbc_91-16-21_community_env.csv")
str(bbc.communitycomposition.env)
summary(bbc.communitycomposition.env)

attach(bbc.communitycomposition.env)

# NMDS analysis of bbc community data, option 2 (with details)
bbc.nmdsresults <- metaMDS(bbc.communitycomposition.data,
                                   distance = "bray", 
                                   k = 2, trymax = 1, 
                                   autotransform = T, 
                                   noshare = 0.1, 
                                   expand = T, 
                                   plot = F)

# Explore data of nmds matrix
bbc.nmdsresults

bbc.nmdsresults$points

str(bbc.nmdsresults)

# Explore NMDS data
scores(bbc.nmdsresults)

# Basic Plot
plot(bbc.nmdsresults)


bbc.communitycomposition.fit <- envfit(bbc.nmdsresults,
                                       bbc.communitycomposition.data,
                                       permutations = 999, na.rm = TRUE)
bbc.communitycomposition.fit

bbc.communitycomposition.ordi <- ordiplot(bbc.nmdsresults, 
                                  choices = c(1,2),
                                  type = "none",
                                  xlab = "NMDS1", 
                                  ylab = "NMDS2")

points(bbc.nmdsresults, "species", pch = 1.2, col = "black", cex = 1.0, adj = 0.9)

points(bbc.nmdsresults, "sites", pch = 1, col = "black", cex = 2.1, adj = 0.9)

text(bbc.nmdsresults, "sites", pch = 1, col = "black", cex = 2.1, adj = 0.9)

text(bbc.nmdsresults, "species", pch = 1, col = "black", cex = 1, adj = 0.9)


ordihull(bbc.nmdsresults, 
         groups = bbc.communitycomposition.env$sampling_year_yyyy, 
         draw = c("polygon"), 
         col =  c("gray", "darkgoldenrod1","blue"))

# Produce a stress plot for evaluating fit of model
stressplot(bbc.nmdsresults)

# Fit is good: R^2 = 0.973


## GGPLOT VERSION:

# Import data set from csv file
bbc.speciescomposition.data <- read.csv("bbc_91-16-21_community.csv", header = TRUE)

# NMDS analysis of community data
bbc.nmdsresults <- metaMDS(comm = bbc.speciescomposition.data[ , 3:102],
                           distance = "bray",
                           try = 100)

# Add NMDS results with original data through column binding
bbc.nmdsmatrix = bind_cols(bbc.speciescomposition.data, bbc.nmdsresults$points)

# Use ggplot function to plot NMDS results by year
bbc.nmdsmatrix %>% 
  ggplot(aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_convexhull(aes(colour = "white")) +
  facet_grid(.~ sampling_year_yyyy) +
  coord_fixed()













##______________________________________________________________________________




# Figure 4

# Import dataset from csv file
vc_distance_matrix <- read.csv("vc_distance_matrix_92_21.csv", header = TRUE)

# NMDS analysis of community data
vc_nmds_results <- metaMDS(comm = vc_distance_matrix[ , 4:406],  # Define the community data 
                           distance = "bray",       # Specify a bray-curtis distance
                           try = 100)               # Number of iterations

# Add NMDS results with original data through column binding
vc_distance_matrix = bind_cols(vc_distance_matrix, vc_nmds_results$points)

# Use ggplot function to plot NMDS results by year
vc_distance_matrix %>% 
  ggplot(aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_convexhull(aes(colour = Year), alpha = 0.3) +
  facet_grid(.~ Year) +
  coord_fixed()

# Use ggplot function to plot NMDS results for all years
vc_distance_matrix_1992 = vc_distance_matrix %>% 
  dplyr::filter(Year != 1992)
vc_distance_1992 = vc_distance_matrix %>% 
  dplyr::filter(Year == 1992)

# Use ggplot function to plot NMDS results for comparison by year with hulls
vc_distance_matrix_1992 %>% 
  group_by(Year) %>% 
  nest() %>% 
  pull() %>% 
  map(function(ind.year){
    ggplot(NULL, aes(x = MDS1, y = MDS2)) +
      geom_point(data = vc_distance_1992, 
                 color = "black") +
      geom_convexhull(data = vc_distance_1992, 
                      color = "black", 
                      fill = "white",
                      alpha = 0.2) +
      geom_point(data = ind.year,
                 color = "blue") +
      geom_convexhull(data = ind.year, 
                      color = "black", 
                      fill = "black",
                      alpha = 0.2) +
      coord_fixed() +
      xlim(c(-2.0,2.5)) +
      ylim(c(-1.5, 1.5)) +
      xlab("NMDS1") +
      ylab("NMDS2") +
      theme_set(theme_classic(base_size = 20)) +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 1, 
                                            linetype = "solid")
      )
  })

# Figure 5A

# Figure 5B

# Figure 6A

# Import dataset from csv file
ws_browseheight_sums <- read.csv("woodystem_browseheight_sums.csv")

# Produce model with ggplot function
woodystem_withinbrowselayer.plot <- ggplot(ws_browseheight_sums) +
  geom_point(aes(x = year, y = density_withinbrowselayerstems)) +
  geom_line(aes(x = year, y = density_withinbrowselayerstems)) +
  theme_classic() +
  ylab("Stem density (/ha)") +
  xlab("Year")

# Plot model
woodystem_withinbrowselayer.plot

# Figure 6B

# Import dataset from csv file
ws_browseheight_sums <- read.csv("woodystem_browseheight_sums.csv")

# Produce model with ggplot function
woodystem_abovebrowselayer.plot <- ggplot(ws_browseheight_sums) +
  geom_point(aes(x = year, y = density_abovebrowselayerstems)) +
  geom_line(aes(x = year, y = density_abovebrowselayerstems)) +
  theme_classic() +
  ylab("Stem density (/ha)") +
  xlab("Year")

# Plot model
woodystem_abovebrowselayer.plot

# Figure 7A

# Import dataset from csv file
vc_nonpreferredandpreferred_sums <- read.csv("vegetationcover_nonpreferredandpreferred_sums.csv")

# Produce model with ggplot function
vc_nonpreferred.plot <- ggplot(vc_nonpreferredandpreferred_sums) +
  geom_point(aes(x = year, y = nonpreferred)) +
  geom_line(aes(x = year, y = nonpreferred)) +
  theme_classic() +
  ylab("Cover estimates") +
  xlab("Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,9000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1991,2022))

# Plot model
vc_nonpreferred.plot

# Figure 7B

# Import dataset from csv file
vc_nonpreferredandpreferred_sums <- read.csv("vegetationcover_nonpreferredandpreferred_sums.csv")

# Produce model with ggplot function
vc_preferred.plot <- ggplot(vc_nonpreferredandpreferred_sums) +
  geom_point(aes(x = year, y = preferred)) +
  geom_line(aes(x = year, y = preferred)) +
  theme_classic() +
  ylab("Cover estimates") +
  xlab("Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,9000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1991,2022))

# Plot model
vc_preferred.plot

# Figure 8A

# Import dataset from csv file
vc_exoticandnative_sums <- read.csv("vegetationcover_exoticandnative_sums.csv")

# Produce model with ggplot function
vc_exotic.plot <- ggplot(vc_exoticandnative_sums) +
  geom_point(aes(x = year, y = exotic_cover)) +
  geom_line(aes(x = year, y = exotic_cover)) +
  theme_classic() +
  ylab("Cover estimates") +
  xlab("Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,4000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1991,2022))

# Plot model
  vc_exotic.plot

# Figure 8B

# Import dataset from csv file
vc_exoticandnative_sums <- read.csv("vegetationcover_exoticandnative_sums.csv")
  
# Produce model with ggplot function
vc_native.plot <- ggplot(vc_exoticandnative_sums) +
  geom_point(aes(x = year, y = native_cover)) +
  geom_line(aes(x = year, y = native_cover)) +
  theme_classic() +
  ylab("Cover estimates") +
  xlab("Year") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,14000)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1991,2022))
  
# Plot model
vc_native.plot

# Table 2

# Import dataset from csv file
woodydiversity.data <- read.csv("woodystem_shannondiversity.csv")

# Conduct Shapiro test to test for normality
shapiro.test(woodydiversity.data$spdiversity)

# Plot data as histogram as an additional visual test for normality
hist(woodydiversity.data$spdiversity)

# Conduct analysis using a two-part hurdle model for species diversity over years
woodyspdiversity.hurdle.model <- brm(bf(spdiversity ~ year),
                                     data = woodydiversity.data,
                                     family = hurdle_gamma())

woodyspdiversity.hurdle.model

# Table 3

# Import dataset from csv file
vegetationcoverdiversity.data <- read.csv("vegetationcover_spdiversity.csv")

# Conduct Shapiro test to test for normality
shapiro.test(vegetationcoverdiversity.data$spdiversity)

# Conduct analysis using a two-part hurdle model for species diversity over years
vegetationcoverspdiversity.hurdle.model <- brm(bf(spdiversity ~ year),
                                               data = vegetationcoverdiversity.data,
                                               family = hurdle_gamma())

vegetationcoverspdiversity.hurdle.model

# Table 4

woodystem_sensitivity.data <- read.csv("woodystem_sensitivityproportions.csv")

woodystem_sensitivity.zoibmodel <- bf(psensitive ~ year,
                                      phi ~ year,
                                      zoi ~ year,
                                      coi ~ year,
                                      family = zero_one_inflated_beta())

woodystem_sensitivity.zoibmodel

woodystem_sensitivity.fit <- brm(formula = woodystem_sensitivity.zoibmodel,
                                 data = woodystem_sensitivity.data)

woodystem_sensitivity.fit

# Table 5


vegetationcover_sensitivity.data <- read.csv("vegetationcover_sensitivityproportions.csv")

vegetationcover_sensitivity.zoibmodel <- bf(psensitive ~ year,
                                            phi ~ year,
                                            zoi ~ year,
                                            coi ~ year,
                                            family = zero_one_inflated_beta())

vegetationcover_sensitivity.fit <- brm(formula = vegetationcover_sensitivity.zoibmodel,
                                       data = vegetationcover_sensitivity.data)

vegetationcover_sensitivity.fit

# Table 6

vegetationcover_nonpreferred.data <- read.csv("vegetationcover_nonpreferred.csv")

vegetationcover_nonpreferred.zoibmodel <- bf(pgrass ~ year,
                                             phi ~ year,
                                             zoi ~ year,
                                             coi ~ year,
                                             family = zero_one_inflated_beta())

vegetationcover_nonpreferred.fit <- brm(formula = vegetationcover_nonpreferred.zoibmodel,
                                        data = vegetationcover_nonpreferred.data)

vegetationcover_nonpreferred.fit

# Table 7


vegetationcover_nativeexotic.data <- read.csv("vegetationcover_nativeexotic.csv")

vegetationcover_nativeexotic.zoibmodel <- bf(pexotic ~ year,
                                             phi ~ year,
                                             zoi ~ year,
                                             coi ~ year,
                                             family = zero_one_inflated_beta())

vegetationcover_nativeexotic.fit <- brm(formula = vegetationcover_nativeexotic.zoibmodel,
                                        data = vegetationcover_nativeexotic.data)

vegetationcover_nativeexotic.fit

# Figure S2

# Plot two-part hurdle model results with predictive trendline
plot(conditional_effects(woodyspdiversity.hurdle.model),
     points = TRUE,
     point_args = list(width = .05, shape = 1))

# Figure S3

# Plot two-part hurdle model results with predictive trendline
plot(conditional_effects(vegetationcoverspdiversity.hurdle.model),
     points = TRUE,
     point_args = list(width = .05, shape = 1))


# Figure S4

woodystem_sensitivity.plot <- plot(conditional_effects(woodystem_sensitivity.fit, dpar = "mu"),
                                   points = T,
                                   point_args = list(width = .05, shape = 1))

# Figure S5

vegetationcover_sensitivity.plot <- plot(conditional_effects(vegetationcover_sensitivity.fit, dpar = "mu"),
                                         points = TRUE,
                                         point_args = list(width = .05, shape = 1))

# Figure S6

vegetationcover_nonpreferred.plot <- plot(conditional_effects(vegetationcover_nonpreferred.fit, dpar = "mu"),
                                          points = TRUE,
                                          point_args = list(width = .05, shape = 1))

# Figure S7

vegetationcover_nativeexotic.plot <- plot(conditional_effects(vegetationcover_nativeexotic.fit, dpar = "mu"),
                                          points = TRUE,
                                          point_args = list(width = .05, shape = 1))

# Table S4

# Import dataset from csv file
woodystem_sprichness.data <- read.csv("woodystem_sprichness.csv")

# Conduct Shapiro test to test for normality
shapiro.test(woodystem_sprichness.data$richness)

# Conduct generalized linear mixed model (GLMM) for species richness over years
woodystem_sprichness.glmm <- glmer(richness ~ year + (1| post) + (1| sitename),
                                   data = woodystem_sprichness.data,
                                   family = poisson)

# Produce summary information of GLMM
mtable(woodystem_sprichness.glmm)

# Table S5

# Import dataset from csv file
vegetationcover_sprichness.data <- read.csv("vegetationcover_sprichness.csv")

# Conduct Shapiro test to test for normality
shapiro.test(vegetationcover_sprichness.data$richness)

# Conduct generalized linear mixed model (GLMM) for species richness over years
vegetationcover_sprichness.glmm <- glmer(richness ~ year + (1| post) + (1| sitename), 
                                         data = vegetationcover_sprichness.data, 
                                         family = poisson)

# Produce summary information of GLMM
mtable(vegetationcover_sprichness.glmm)


# Table S6 - within browse layer (1992-2021)

# Import dataset from csv file
woodystem_withinbrowse9221.data <- read.csv("woodystem_browseheight_92_21.csv")

# Conduct Shapiro test to test for normality
shapiro.test(woodystem_withinbrowse9221.data$withinbrowselayerstems)

# Conduct generalized linear mixed model (GLMM) for species richness over years
woodystem_withinbrowse9221.glmm <- glmer(withinbrowselayerstems ~ year + (1| post) + (1| sitename), 
                                        data = woodystem_withinbrowse9221.data,
                                        family = poisson)

# Produce summary information of GLMM
mtable(woodystem_withinbrowse9221.glmm)

# Table S7

# Import dataset from csv file
woodystem_abovebrowse9294.data <- read.csv("woodystem_browseheight_92_94.csv")

# Conduct Shapiro test to test for normality
shapiro.test(woodystem_abovebrowse9294.data$withinbrowselayerstems)

# Conduct generalized linear mixed model (GLMM) for species richness over years
woodystem_abovebrowse9294.glmm <- glmer(abovebrowselayerstems ~ year + (1| post) + (1| sitename), 
                                        data = woodystem_abovebrowse9294.data,
                                        family = poisson)

# Produce summary information of GLMM
mtable(woodystem_abovebrowse9221.glmm)

# Table S8

# Import dataset from csv file
woodystem_abovebrowse9221.data <- read.csv("woodystem_browseheight_92_21.csv")

# Conduct Shapiro test to test for normality
shapiro.test(woodystem_abovebrowse9221.data$withinbrowselayerstems)

# Conduct generalized linear mixed model (GLMM) for species richness over years
woodystem_abovebrowse9221.glmm <- glmer(abovebrowselayerstems ~ year + (1| post) + (1| sitename), 
                                        data = woodystem_abovebrowse9221.data,
                                        family = poisson)

# Produce summary information of GLMM
mtable(woodystem_abovebrowse9221.glmm)
