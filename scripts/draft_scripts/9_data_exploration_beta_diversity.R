#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Beta Diversity
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

# Beta Diversity & Similarity Between Sites

# Create a site-species matrix (presence-absence)
species.diversity.matrix <- bird.abund.data %>%
  group_by(year, sitecode, species_4code_IBP) %>%
  summarise(total_territories = sum(territories), .groups = "drop") %>%
  spread(species_4code_IBP, total_territories, fill = 0)

# Compute Jaccard or Bray-Curtis Dissimilarity
jaccard_dist <- vegdist(species.diversity.matrix[,-2], method = "jaccard")

bray_dist <- vegdist(species.diversity.matrix[,-2], method = "bray")

# Perform Hierarchical Clustering
hc <- hclust(jaccard_dist, method = "average")

# Plot Dendrogram
plot(hc, main = "Hierarchical Clustering of Sites (Jaccard)", xlab = "Sites")

