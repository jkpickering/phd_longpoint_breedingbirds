#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Species Richness
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-02-12
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
bird.abund.data <- read.csv("bird_spterritories_abund.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Need to start simple, add complexity. Next step is to include species 
# characteristic data for analyses to group species by management groups, 
# foraging guilds, preferred foods, family/genus, nesting guilds, etc.

# Import data from csv file, saved in the 'data' sub-folder of the rproject
speciesdescription.data <- read.csv("../data/bird_spcharacteristics.csv", header = T)

# Identify summary information about the data (e.g., mean, median, min and max)
summary(speciesdescription.data) # Summarize key information within the dataset

# Join species information data "speciesdescription.data" with territory data for analyses
territoryguild.data <- full_join(bird.abund.data, speciesdescription.data, by = c("species_4code_IBP"))

# Review column names for reference in analyses
names(territoryguild.data) # Identify column names within the dataset

# -----------------------------------------------------------------------------#

# Summarize species abundance by guild
guild_summary <- territoryguild.data %>%
  group_by(habitat_guild_BOTW) %>%
  summarise(total_abundance = sum(territories))

# Loop through each guild and create a separate plot
guilds <- unique(territoryguild.data$habitat_guild_BOTW)

# Create multiple plots (nest guild) for each site
for(g in guilds) {

  # Create a ggplot for each guild
  p <- ggplot(filter(territoryguild.data, habitat_guild_BOTW == g),
              aes(x = year,
                  y = guild_summary,
                  group = sitecode)) +
    geom_smooth(method = "lm") +
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
    facet_wrap(~ sitecode, ncol = 3, nrow = 5) +  # Separate plots per site
    labs(title = paste("Territories of", sp, "over time at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Number of territories") +
    xlim(1991, 2021) # Set x-limit minimum and maximum
  
  print(p)  # Print the plot for each guild
}

print(p)

# Create a smoothed trend line wrapped plot (by site) to visualize the total abundance (territories) for each site over time using the 'facet_wrap' function
bird.total.abund.smooth.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_abundance, group = sitecode)) +
  geom_smooth(method = "loess", size = 1, colour = "steelblue") +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
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
  labs(title = "Total breeding bird territories for monitoring sites over time
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

# Visualize the total abundance (territories) sampled over time for each site (site x year)
bird.total.abund.smooth.plot

# Visualize individual species data over time, for individual sites

# Get list of species to filter data by species
species_list <- unique(territoryguild.data$species_4code_IBP)  

# Loop through each unique species and create separate plots
for (sp in species_list) {
  # Filter data for the specific species
  bird.species.territories.plot  <- ggplot(filter(territoryguild.data, species_4code_IBP == sp), aes(x = year, y = territories, color = sitecode, group = sitecode)) +
    geom_point(size = 1.2) + # Add points for better visibility
    geom_smooth(method = "lm") +
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
    facet_wrap(~ sitecode, ncol = 3, nrow = 5) +  # Separate plots per site
    labs(title = paste("Territories of", sp, "over time at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Number of territories") +
    xlim(1991, 2021) # Set x-limit minimum and maximum
  
  # Print plot in the R console
  print(bird.species.territories.plot)
  
}

# Save each plot (optional)
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/",sp,"_territories.png"), plot = bird.species.territories.plot, width = 8, height = 6)

# -----------------------------------------------------------------------------#

# Visualize species guild (Nesting Guild) abundance data over time, for individual sites

# Get list of species to filter data by species
nest_guild_list <- unique(territoryguild.data$nest_guild_BOTW)  

# Loop through each unique species and create separate plots
for (sp in nest_guild_list) {
  # Filter data for the specific species
  bird.species.territories.plot  <- ggplot(filter(territoryguild.data, 
                                                  nest_guild_BOTW == sp), 
                                           aes(x = year, 
                                               y = territories, 
                                               color = sitecode, 
                                               group = sitecode)) +
    geom_point(size = 1.2) + # Add points for better visibility
    geom_smooth(method = "lm") +
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
    facet_wrap(~ sitecode, ncol = 3, nrow = 5) +  # Separate plots per site
    labs(title = paste("Territories of", sp, "over time at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Number of territories") +
    xlim(1991, 2021) # Set x-limit minimum and maximum
  
  # Print plot in the R console
  print(bird.species.territories.plot)
  
}
