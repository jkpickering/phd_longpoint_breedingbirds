#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Nature Counts Data Exploration

# Author:         Joshua Pickering, PhD Candidate
# Affiliation:    University of Waterloo
# Creation Date:  2025-07-22
# Last Updated:   2025-07-22

# Description:    This script includes results of exploratory summary data and 
#                 visualizations for breeding bird census territory data
#                 collected at Long Point, Ontario, Canada from 1991 - 2021

# Data Sources:   "../data/bird_spterritories_abund.csv"

# R Version:      4.4.2 (2024-10-31 ucrt)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 1.0 Script setup

#-----------------------------------------------------------------------------#
# 1.1 Clear all objects from the R Environment prior to data exploration

# Remove all existing data from the Environment with 'rm' function
rm(list = ls())  # remove all objects

# Identify the working directory for analyses
getwd() # view working directory filepath

#-----------------------------------------------------------------------------#
# 1.2 Identify and install necessary R packages
# Remove text option to complete code if necessary (i.e., #)

# install.packages("remotes") # install remotes package
# remotes::install_github("BirdsCanada/naturecounts") # install nature counts

# Load specified downloaded packages for Nature Counts data management
library(naturecounts) # load naturecounts package
library(tidyverse) # load tidyverse for data download and filtering

nc_permissions(username = "jkpickering") # check which data you have permissions

nc_count(username = "jkpickering",
         show = "all") # check which data you don't have permission for.

obba.data <- nc_data_dl(request_id = 258243, username = "jkpickering")
1

collections <- meta_collections()
view(collections)

rare.test <- nc_data_dl(collections = "OBBA2RC",
                   username = "jkpickering",
                   request_id = 258243)

nc_query_table(username="jkpickering")

# no access yet from Catherine, note that 'Trends', and 'TrendIndices' provides regional trends for species.




