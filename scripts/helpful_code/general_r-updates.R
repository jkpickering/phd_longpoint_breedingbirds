#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          General Code
# Sub-title:      R Updates Script

# Author:         Joshua Pickering, PhD Candidate
# Affiliation:    University of Waterloo
# Creation Date:  2025-07-24
# Last Updated:   2025-07-24

# Description:    This script includes code and instructions for updating R and
#                 updates for R packages

# Data Sources:   n/a

# R Version:      4.5.1 (2025-06-13 ucrt)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 1.0 Script setup

#-----------------------------------------------------------------------------#
# 1.1 Check for updates for R and R Studio

# Install package for checking r version and update process
install.packages("installr")  # install installr package

# Load package for R updates
library(installr)

# Check for R version updates
updateR()

# Check version of R
R.version.string # notes R version string

# If R is still running previous version, go to 'Tools' -> 'Global Options...'
# -> Under 'General', manually select the new R version under 'R Sessions'
# Next step is to select 'Apply' and 'Cancel', then close out and restart 
# RStudio to change version use.

# Re-check R version if necessary
R.version # notes r version, system, and nickname

#-----------------------------------------------------------------------------#
# 1.2 Clear all objects from the R Environment prior to data exploration

# Remove all existing data from the Environment with 'rm' function
rm(list = ls())  # remove all objects

# Identify the working directory for analyses
getwd() # view working directory filepath

#-----------------------------------------------------------------------------#
# 1.3 Identify and updates R packages

# Update already installed packages
update.packages(checkBuilt = TRUE, ask = FALSE) # updates all installed packages

# Run 'options' function to suppress warnings
options(warn = -1)  # suppress warnings if packages are already loaded

# List required packages (listed alphabetically for ease of use/revision)
list.of.packages <- c("broom", # organize data
                      "dplyr",
                      "forcats",
                      "ggplot2", # plotting and custom visualizations
                      "ggpubr", # publisher-quality visualizations
                      "ggrepel",
                      "grid", # produces multiple plot options
                      "htmlwidgets", # customizable interactive visualizations
                      "installr", # R software update package
                      "lme4",
                      "mgcv",
                      "patchwork",
                      "pheatmap", # heatmap design
                      "plotly", # interactive ggplot options and visualizations
                      "purrr",
                      "stringr",
                      "tibble", # sorting data
                      "tidyr", # data cleaning and organization
                      "tidytext", # data text
                      "tidyverse", # data organization and manipulation
                      "vegan") # ecological statistics options

# Create function to install packages if they are missing from the library
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
} # new function for installing packages if they have not previously been

# Install missing packages
lapply(list.of.packages, # vector of listed packages
       install_if_missing) # use created function for installing packages

# Load required packages
lapply(list.of.packages, # vector of listed packages
       library, # identify the r library
       character.only = TRUE) # run characters

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# END of script

