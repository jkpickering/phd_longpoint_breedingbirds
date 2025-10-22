#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-01-30
# Last Updated:   2025-02-11
# Description:    This script provides data import files in preparation for
#                 analyses for breeding bird census data collected at Long
#                 Point, Ontario, Canada from 1991 - 2021.
# Data Sources:   /data/raw/"FILE NAME.csv"
# Outputs:        /results/"FILE NAME.csv"
# R Version:      4.4.2 (2024-10-31 ucrt)
# Dependencies:   dplyr, ggplot2, scales, tidyverse
# NOTES: 
#-----------------------------------------------------------------------------#

# Notice of a need to update pop-up should populate the R Studio program if
# the RStudio program needs an update. But, you can also select the 'Help' tab
# from top of page and then select 'Check for Updates' if necessary.

# To identify the current R version being used in this script, use the
# 'version' command to recall the version and update if necessary.
version

# Copy following functions into R x64 when updates are needed.
install.packages("installr")
library(installr)
installr(GUI = TRUE)

# Identify and confirm the current working directory.
getwd()

# Set the current working directory after identifying with previous function.
setwd("D:/r_projects/longpoint_breedingbirds/scripts")