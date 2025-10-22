#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Species Accumulation Curves
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-02-09
# Last Updated:   2025-02-24
# Description:    This script includes results of exploratory summary data and 
#                 visualizations for the species richness of breeding bird 
#                 census abundance data collected at Long Point, Ontario, 
#                 Canada from 1991 - 2021
#
# Data Sources:   "../data/bird_spterritories_abund.csv"
# Outputs:        "../results/plots/"
#                 1. "bird.abund.totals.plot.png"
#                 2. "bird.abund.totals.plotly.html"
#                 3.
#                 
# R Version:      4.4.2 (2024-10-31 ucrt)
#
# NOTES:          Run statistical models and visualize species richness data
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Script setup

# Clear all objects from the R Environment prior to data exploration
rm(list = ls())  # Remove all objects

#-----------------------------------------------------------------------------#

# Identify and install necessary R packages for data exploration

# To reduce unnecessary loading, run 'options' function
options(warn = -1)  # Suppress warnings if packages are already loaded

# Identify required packages
list.of.packages <- c("ggplot2",
                      "dplyr",
                      "tidyr",
                      "patchwork",
                      "broom",
                      "plotly",
                      "ggpubr",
                      "lme4",
                      "htmlwidgets",
                      "mgcv",
                      "tidyverse",
                      "pheatmap",
                      "vegan",
                      "purrr")

# Create function to install packages if they are missing from the library
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# Install missing packages
lapply(list.of.packages, install_if_missing)

# Load required packages
lapply(list.of.packages, library, character.only = TRUE) 

#-----------------------------------------------------------------------------#

# Import csv file of breeding bird census abundance (territory) data

# Import data from csv file, saved in the 'data' sub-folder of the rproject
bird.abund.data <- read.csv("../data/bird_spterritories_abund.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Complete a species accumulation curve for sites using the 'speaccum' function
sac.site.matrix <- bird.abund.data %>%
  group_by(sitecode, species_4code_IBP) %>%
  summarize(abundance = sum(territories)) %>%
  spread(species_4code_IBP, abundance, fill = 0) %>%
  ungroup() %>%
  select(-sitecode) # Remove site column for matrix

sac.site <- specaccum(sac.site.matrix, method = "random", permutations = 100)


# Convert species accumulation object to data frame
sac.site.df <- data.frame(
  Effort = sac.site$sites,  # Number of sites sampled
  Richness = sac.site$richness,  # Mean species richness
  Lower = sac.site$richness - sac.site$sd,  # Lower bound of confidence interval
  Upper = sac.site$richness + sac.site$sd   # Upper bound of confidence interval
)

# Plot SAC with confidence intervals
species.site.accumulation.plot <- ggplot(sac.site.df, aes(x = Effort, y = Richness)) +
  geom_line(color = "black", size = 1) +  # SAC Line
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, fill = "grey") +  # Confidence Interval
  theme_minimal() +
  labs(
    title = "Species Accumulation Curve over sites",
    x = "Sampling Effort (Sites)",
    y = "Species Richness"
  ) +
  theme(text = element_text(size = 14))

# Visualize SAC plot for sites
species.site.accumulation.plot

#-----------------------------------------------------------------------------#

# Complete a species accumulation curve for years using the 'speaccum' function
sac.year.matrix <- bird.abund.data %>%
  group_by(year, species_4code_IBP) %>%
  summarize(abundance = sum(territories)) %>%
  spread(species_4code_IBP, abundance, fill = 0) %>%
  ungroup() %>%
  select(-year) # Remove site column for matrix

sac.year <- specaccum(sac.year.matrix, method = "random", permutations = 100)


# Convert species accumulation object to data frame
sac.year.df <- data.frame(
  Effort = sac.year$sites,  # Number of sites sampled
  Richness = sac.year$richness,  # Mean species richness
  Lower = sac.year$richness - sac.year$sd,  # Lower bound of confidence interval
  Upper = sac.year$richness + sac.year$sd   # Upper bound of confidence interval
)

# Plot SAC with confidence intervals
species.year.accumulation.plot <- ggplot(sac.year.df, aes(x = Effort, y = Richness)) +
  geom_line(color = "black", size = 1) +  # SAC Line
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, fill = "grey") +  # Confidence Interval
  theme_minimal() +
  labs(
    title = "Species accumulation curve over time",
    x = "Sampling years",
    y = "Species richness"
  ) +
  theme(text = element_text(size = 14))

# Visualize SAC plot for sites
species.year.accumulation.plot

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# *Summary of total territories accumulated over time

# Summarize abundance per site-year-species
site_year_species <- bird.abund.data %>%
  group_by(sitecode, year, species_4code_IBP) %>%
  summarize(abundance = sum(territories), .groups = "drop")

# Convert to presence-absence (1 if species present, 0 otherwise)
site_year_species <- site_year_species %>%
  mutate(Presence = ifelse(abundance > 0, 1, 0))

# Calculate cumulative species richness per site over time
sac_time <- site_year_species %>%
  group_by(sitecode, year) %>%
  summarize(sp_richness = n_distinct(species_4code_IBP), .groups = "drop") %>%
  arrange(sitecode, year) %>%
  group_by(sitecode) %>%
  mutate(cumul_richness = cumsum(sp_richness))  # Cumulative sum over years

ggplot(sac_time, aes(x = year, y = cumul_richness, group = sitecode)) +
  geom_line(color = "blue", size = 1) +  # Line for SAC
  geom_point(color = "black", size = 2) +  # Points for each year
  facet_wrap(~sitecode) +  # Separate plots for each site
  theme_minimal() +
  labs(
    title = "Species Accumulation Curve Over Time for Each Site",
    x = "Year",
    y = "Cumulative Species Richness"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    text = element_text(size = 14)
  )



#-----------------------------------------------------------------------------#

# Species accumulation curve for total richness for sites over time, wrapped with 'facet_wrap' function

# Ensure unique site-year-species records
site_year_species <- bird.abund.data %>%
  group_by(sitecode, year, species_4code_IBP)

# Find the first year each species appears at each site
first_appearance <- site_year_species %>%
  group_by(sitecode, species_4code_IBP) %>%
  summarize(first_year = min(year), .groups = "drop")

# Count cumulative unique species at each site over time
sac_time <- first_appearance %>%
  group_by(sitecode, first_year) %>%
  summarize(new_species = n(), .groups = "drop") %>%  # Count species appearing in each year
  arrange(sitecode, first_year) %>%
  group_by(sitecode) %>%
  mutate(cumulative_richness = cumsum(new_species)) %>%  # Accumulate species richness over time
  rename(year = first_year)  # Rename for consistency

# visualize
ggplot(sac_time, aes(x = year, y = cumulative_richness, group = sitecode)) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_point() +
  facet_wrap(~sitecode, scales = "free_y") +  # Separate plots for each site
  theme_pubr() +
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Centers the title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Customizes x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Customizes y-axis title
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y.left = element_text(size = 8, color = "black")) +
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +  # Separate plots per site
  labs(title = "Species accumulation curve over time per site
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Cumulative number of species") +
  xlim(1991, 2021) + # Set x-limit minimum and maximum
  scale_y_continuous(limits = c(0, NA))

#-----------------------------------------------------------------------------#

