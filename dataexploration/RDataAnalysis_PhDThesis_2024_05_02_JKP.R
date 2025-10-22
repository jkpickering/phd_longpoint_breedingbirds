# 
# RStudio Work File ------------------------------------------------------------

# Project Title:     Statistical Analyses for Doctoral (PhD) Thesis
# Project Theme:     Breeding Bird Data Analysis
# Author:            Joshua Pickering
# Last Updated:      2024-07-17
# RStudio Version:   4.6.2 (2021)

# ------------------------------------------------------------------------------
# Set up work file
# Set working directory --------------------------------------------------------
setwd("D:/PhDThesis_RWorkingDirectory")

# Load required packages -------------------------------------------------------
library(vegan)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(tidyquant)
library(ggConvexHull)
library(dplyr)
library(scatterplot3d)
library(vegan3d)
library(readr)
library(rlist)
library(tibble)
library(rgl)
library(plot3D)
library(RColorBrewer)
library(tidyverse)
library(rstatix)
library(lme4)
library(gpboost)
library(glmm)
library(effects)
library(scales)
library(betareg)
library(emmeans)
library(lmtest)
library(memisc)
library(zoib)
library(brms)
library(rstan)

# *Periodically review necessary installed packages

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
table(bird_data$site_name)

# View the occurrence of each data point within a variable (view and table function)
view(sort(table(bird_data$site_name), decreasing = TRUE))

# Explore the occurrence of data under a specific variable (barplot function)
barplot(sort(table(bird_data$species_common_name), decreasing = TRUE))

# Explore the occurrence of data under a specific variable (boxplot function)
boxplot(bird_data$breeding_territories)

# Explore the occurrence of data within bins (i.e., 0-5, 5-10) under a specific variable (hist function)
hist(bird_data$breeding_territories)

# ------------------------------------------------------------------------------
# Analyze data: ----------------------------------------------------------------

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
