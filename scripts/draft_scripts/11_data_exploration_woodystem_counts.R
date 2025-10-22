#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Woody Stem Abundance (Count) Data
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-02-27
# Last Updated:   2025-02-27
# Description:    This script includes results of exploratory summary data and 
#                 visualizations for count data associated with woody stems,
#                 sampled across 150 quadrats at Long Point, Ontario, 
#                 Canada from 1991 - 2021.
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

# Explore data

# Identify the structure of the data
glimpse(bird.abund.data)

# Identify the structure of the data
str(bird.abund.data)

# Check for missing values within the data
any(is.na(bird.abund.data))

# Check unique counts for identified variables
bird.abund.data %>%
  summarise(
    Unique_Species_Samples = n_distinct(uniqueID),
    Unique_Site_Samples = n_distinct(sample),
    Unique_Years = n_distinct(year),
    Unique_Sites = n_distinct(sitecode),
    Unique_Species = n_distinct(species_4code_IBP))

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird.abund.data) # Summarize key information within the dataset