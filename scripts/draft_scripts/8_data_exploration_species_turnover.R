#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Species Turnover & Community Shifts
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-02-24
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
list.of.packages <- c("ggplot2", "dplyr", "tidyr", "patchwork" , "broom", "plotly", "ggpubr", "lme4", "htmlwidgets", "mgcv", "tidyverse", "pheatmap", "vegan", "purrr")

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

# Species Turnover & Community Shifts

# Summarize presence per species-year
species_turnover <- data %>%
  group_by(Year, Species) %>%
  summarise(Present = 1, .groups = "drop") %>%
  spread(Species, Present, fill = 0)

# Heatmap for species presence over time
library(pheatmap)
pheatmap(as.matrix(species_turnover[,-1]), cluster_rows = FALSE, main = "Species Turnover Over Time")

# OR 

# Run NMDS on species-site matrix
nmds <- metaMDS(site_species_matrix[,-1], distance = "bray", k = 2)

# Plot NMDS
ordiplot(nmds, type = "text")



