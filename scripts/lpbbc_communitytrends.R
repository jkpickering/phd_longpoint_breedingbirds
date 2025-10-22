#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Long Point Breeding Bird Census (BBC) Project
# Sub-title:      Data chapter 1 - community trends

# Author:         Joshua Pickering, PhD Candidate
# Affiliation:    University of Waterloo
# Creation Date:  2025-08-18
# Last Updated:   2025-10-03

# Description:    This script includes results of exploratory summary data and 
#                 visualizations for breeding bird census territory data
#                 collected at Long Point, Ontario, Canada from 1991 - 2021

# Data Sources:   "../data/bird_spterritories_abund.csv"
#                 "../data/bird_spcharacteristics.csv"
#                 "../data/site_characteristics.csv"

# R Version:      4.5.1 (2025-06-13 ucrt)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 1.0 Script setup

#-----------------------------------------------------------------------------#
# 1.1 Clear all objects from the R Environment prior to data exploration

# Remove all existing data from the Environment with 'rm' function
rm(list = ls())  # remove all objects

# Identify the working directory for analyses
getwd() # view working directory filepath

# Check version of R for script
R.version.string # version R check

#-----------------------------------------------------------------------------#
# 1.3 Identify and install necessary R packages

# To reduce unnecessary loading, run 'options' function
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

# 2.0 Import required data files (csv format required)

#-----------------------------------------------------------------------------#
# 2.1 Import bird species territories data (i.e., site abundance)

# Import data from csv file
bird_territories <- read.csv("../data/bird_species_territories_v1.csv",
                            header = T)

# Check for missing values within the data
any(is.na(bird_territories)) # identify if any data are 'n.a.' or missing

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird_territories) # Summarize key information within the dataset

# OPTIONAL: Remove text format to run code (i.e., remove '#') if needed
# view(bird_territories) # views data in new window

#-----------------------------------------------------------------------------#
# 2.2 Import bird species traits data (e.g., nesting guild name)

# Import data from csv file, saved in the 'data' sub-folder of the rproject
bird_traits <- read.csv("../data/bird_species_traits_v2.csv",
                                    header = T)

# Check for missing values within the data
any(is.na(bird_traits)) # identify if any data are 'n.a.' or missing

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird_traits) # Summarize key information within the dataset

# OPTIONAL: Remove text format to run code (i.e., remove '#') if needed
# view(bird_traits) # views data in new window

#-----------------------------------------------------------------------------#
# 2.3 Import site characteristics data (e.g., site location, area)

# Import data from csv file, saved in the 'data' sub-folder of the rproject
site_traits <- read.csv("../data/site_env_characteristics_v1.csv",
                        header = T)

# Check for missing values within the data
any(is.na(site_traits)) # identify if any data are 'n.a.' or missing

# Identify summary information about the data (e.g., mean, median, min and max)
summary(site_traits) # Summarize key information within the dataset

# OPTIONAL: Remove text format to run code (i.e., remove '#') if needed
# view(site_traits) # views data in new window

#-----------------------------------------------------------------------------#
# 2.4 Join species information data "speciesdescription.data" with territory 
# data for analyses
bird_territories_with_traits <- full_join(bird_territories,
                                          bird_traits,
                                          by = c("species_4code_IBP"))

#-----------------------------------------------------------------------------#
# 2.5 Join species information data "speciesdescription.data" with territory 
# data for analyses
bird_territories_with_traits <- full_join(bird_territories_with_traits,
                                          site_traits,
                                          by = c("sitecode"))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 3.0  Species richness: data visualizations

#-----------------------------------------------------------------------------#
# 5.1 Plot the species richness for sites over time

# To filter specific sites only, include this code and then revise ncol format:
# filter(sitecode %in% c("ROWB", "ROMS", "ROIS", "RARO", "ROMF")) %>% 

# Create a bar chart plot to visualize the total species richness for each site
richness_by_site_plot <- bird_territories %>%
  group_by(sitecode) %>%
  summarise(richness = n_distinct(species_4code_IBP)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = sitecode,
             y = richness)) +
  geom_bar(stat = "identity",
           fill = "steelblue") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0,
                               "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black",
                                   hjust = 1,
                                   angle = 45), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  labs(x = "Site",
       y = "Species richness") +
  scale_y_continuous(limits = c(0, 60),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_discrete(expand = c(0, 0.5))

# Visualize total species richness per site totals
print(richness_by_site_plot) # visualize plot

#-----------------------------------------------------------------------------#
# 5.1.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/richness_by_site_plot.png"),
# plot = richness_by_site_plot,
# width = 8,
# height = 6)

#-----------------------------------------------------------------------------#
# 5.2 Plot the species richness for sites over time using facet_wrap

# To filter specific sites only, include this code and then revise ncol format:
# filter(sitecode %in% c("ROWB", "ROMS", "ROIS", "RARO", "ROMF")) %>%

# Create a smoothed trend line wrapped plot (by site) to visualize the species richness for each site over time using the 'facet_wrap' function
richness_over_time_by_site_plot <- bird_territories %>%
  group_by(sitecode,
           year) %>%
  summarise(richness = n_distinct(species_4code_IBP)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = year, 
             y = richness, 
             group = sitecode)) +
  geom_smooth(method = lm,
              size = 1,
              colour = "steelblue") +
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0,
             color = "black") +  # Horizontal line
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, 
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, 
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode, 
             ncol = 3, 
             nrow = 5, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Species richness") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 10),
                     minor_breaks = seq(0, 50, by = 5)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird_territories$year),
                                  max(bird_territories$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

# Visualize richness over time for each site (n = 15)
print(richness_over_time_by_site_plot) # visualize plot

#-----------------------------------------------------------------------------#
# 5.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/richness_over_time_by_site_plot.png"),
# plot = richness_over_time_by_site_plot,
# width = 8.18,
# height = 6.78)

#-----------------------------------------------------------------------------#
# 5.3 Plot the species richness for sites over time using facet_wrap
  
# To filter specific sites only, include this code and then revise ncol format:
# filter(sitecode %in% c("ROWB", "ROMS", "ROIS", "RARO", "ROMF")) %>%
  
# Create a smoothed trend line wrapped plot (by site) to visualize the species richness for each site over time using the 'facet_wrap' function
richness_over_time_by_site_plot <- bird_territories %>%
group_by(sitecode,
           year) %>%
summarise(richness = n_distinct(species_4code_IBP)) %>%
print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
ggplot(aes(x = year, 
             y = richness, 
             group = sitecode)) +
  geom_smooth(method = "loess", # specify smoothing line
                size = 1, # set point size
                colour = "steelblue") + # set point colour
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0,
             color = "black") +  # Horizontal line
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, 
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, 
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode, 
             ncol = 3, 
             nrow = 5, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Species richness") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 10),
                     minor_breaks = seq(0, 50, by = 5)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird_territories$year),
                                  max(bird_territories$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

# Visualize richness over time for each site (n = 15)
print(richness_over_time_by_site_plot) # visualize plot

#-----------------------------------------------------------------------------#
# 5.3.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/richness_over_time_by_site_plot.png"),
# plot = richness_over_time_by_site_plot,
# width = 8.18,
# height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 6.0 Species diversity (Shannon Index of diversity): data visualizations

#-----------------------------------------------------------------------------#
# 6.1 Plot the species diversity for sites

# Create a site-species matrix (presence-absence)
site_by_species_matrix <- bird_territories %>%
  group_by(year,
           sitecode,
           species_4code_IBP) %>%
  summarise(total_territories = sum(territories),
            .groups = "drop") %>%
  spread(species_4code_IBP,
         total_territories,
         fill = 0)

# Compute the Shannon index per site and time
site_by_species_matrix$diversity <- diversity(site_by_species_matrix[,3:ncol(site_by_species_matrix)],
                                                index = "shannon")


# Identify the average species diversity (Shannon Index) for each site
mean_diversity <- site_by_species_matrix %>%
  group_by(sitecode) %>%
  summarise(mean_diversity = mean(diversity),
            .groups = "drop")

# To filter specific sites only, include this code and then revise ncol format:
# filter(sitecode %in% c("ROWB", "ROMS", "ROIS", "RARO", "ROMF")) %>%

# Plot using ggplot function
diversity_by_site_plot <- ggplot(mean_diversity, aes(x = sitecode,
                           y = mean_diversity)) +
  geom_bar(stat = "identity",
           fill = "steelblue") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  labs(x = "Site",
       y = "Species diversity (Shannon Index of Diversity)") +
  scale_y_continuous(breaks = seq(0, 4,
                                  by = .25),
                     limits = c(0, 3.25),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0.5))

# Visualize species diversity for each site 
print(diversity_by_site_plot) # visualize plot

#-----------------------------------------------------------------------------#
# 6.1.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/diversity_by_site_plot.png"),
# plot = diversity_by_site_plot,
# width = 8,
# height = 6)

#-----------------------------------------------------------------------------#
# 6.2 Plot the species diversity over time for each site with facet_wrap

# plot
diversity_over_time_by_site_plot <- ggplot(site_by_species_matrix,
       aes(x = year,
           y = diversity,
           group = sitecode)) +
  geom_smooth(method = lm,
              size = 1, 
              colour = "steelblue") +
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  geom_vline(xintercept = 1990,
             color = "black") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode,
             ncol = 3,
             nrow = 5, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird_territories$year),
                                  max(bird_territories$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

#Visualize
print(diversity_over_time_by_site_plot)

#-----------------------------------------------------------------------------#
# 6.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/diversity_over_time_by_site_plot.png"),
# plot = diversity_over_time_by_site_plot,
# width = 8.18,
# height = 6.78)

#-----------------------------------------------------------------------------#
# 6.3 Plot the species diversity over time for each site with facet_wrap

# plot
diversity_over_time_by_site_plot <- ggplot(site_by_species_matrix,
                                           aes(x = year,
                                               y = diversity,
                                               group = sitecode)) +
  geom_smooth(method = "loess",
              size = 1, 
              colour = "steelblue") +
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  geom_vline(xintercept = 1990,
             color = "black") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode,
             ncol = 3,
             nrow = 5, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird_territories$year),
                                  max(bird_territories$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

#Visualize
print(diversity_over_time_by_site_plot)

#-----------------------------------------------------------------------------#
# 6.3.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
# "../results/plots/diversity_over_time_by_site_plot.png"),
# plot = diversity_over_time_by_site_plot,
# width = 8.18,
# height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 11.0 Bird species accumulation curves: data visualizations

#-----------------------------------------------------------------------------#
# 11.1 Plot nesting guild trends over time, for individual sites

# Complete a species accumulation curve for years using the 'speaccum' function
sac.year.matrix <- bird_territories_with_traits %>%
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
species.year.accumulation.plot <- ggplot(sac.year.df, 
                                         aes(x = Effort, y = Richness)) +
  geom_line(color = "steelblue",
            size = 1) +  # SAC Line
  geom_ribbon(aes(ymin = Lower,
                  ymax = Upper), 
              alpha = 0.3, 
              fill = "grey") +  # Confidence Interval
  theme_pubr() +
  labs(x = "Number of years since start of monitoring",
       y = "Number of species") +
  scale_y_continuous(breaks = seq(0, 110, by = 10),
                     limits = c(0, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(0, 40, by = 5),
                     limits = c(NA, NA),
                     expand = c(0, 0))

# Visualize SAC plot for sites
species.year.accumulation.plot

#-----------------------------------------------------------------------------#
# 11.2 Plot species accumulation curve for all sites over time with 'facet_wrap'

# Convert to presence-absence
site_year_species <- bird_territories_with_traits %>%
  filter(territories > 0) %>%
  filter(dominant_ELC_site_code %in% c("WOD")) %>% 
  distinct(sitecode,
           year,
           species_4code_IBP)

# Get cumulative species richness per site over time
sac_time <- site_year_species %>%
  arrange(sitecode, year) %>%
  group_by(sitecode, year) %>%
  summarise(species_list = list(unique(species_4code_IBP)), .groups = "drop") %>%
  group_by(sitecode) %>%
  mutate(
    cumulative_species = accumulate(species_list, ~ union(.x, .y)),  # track cumulative species
    species_seen = map_int(cumulative_species, length)               # count how many species seen so far
  ) %>%
  select(sitecode, year, species_seen)  # final data for plotting

print(sac_time)

# Plot
sac_wrapped <- ggplot(sac_time, aes(x = year,
                                    y = species_seen,
                                    group = sitecode)) +
  geom_smooth(method = "loess",
              color = "steelblue",
              size = 1) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~sitecode,
             ncol = 3,
             nrow = 5,
             scales = "free_y") +
  theme_pubr() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1.0, "cm"),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,
                                  color = "black", margin = margin(b = 15)),
        axis.title.x.bottom = element_text(size = 12, face = "bold",
                                           colour = "black", margin = margin(t = 15)),
        axis.title.y.left = element_text(size = 12, face = "bold",
                                         colour = "black", margin = margin(r = 15)),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y.left = element_text(size = 8, color = "black")) +
  labs(x = "Year",
       y = "Number of species") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(limits = c(NA, NA), expand = c(0, 0))

print(sac_wrapped)

#-----------------------------------------------------------------------------#
# 11.3 Explore Beta Diversity to determine if there is species turnover

# Make a site-by-species matrix for each year

#-----------------------------------------------------------------------------#
# Example: Jaccard dissimilarity matrix for all years at one site
# Option 1
comm_matrix <- table(site_year_species$year, site_year_species$species_4code_IBP)
beta_dist <- vegdist(comm_matrix, method = "jaccard", binary = TRUE)

#-----------------------------------------------------------------------------#
# Option 2

# Wide format: year as rows, species as columns
pa_matrix <- bird_territories_with_traits %>%
  filter(territories > 0) %>%
  distinct(sitecode, year, species_4code_IBP) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = species_4code_IBP, values_from = present, values_fill = 0)

# If you're working with a single site (e.g., "ROWB")
site_pa <- pa_matrix %>% filter(sitecode == "ROWB") %>% select(-sitecode)
row_years <- site_pa$year
site_pa <- site_pa %>% select(-year)

# Compute Jaccard dissimilarity matrix
dissim_matrix <- vegdist(site_pa, method = "jaccard", binary = TRUE)

# Convert to full matrix
dissim_df <- as.matrix(dissim_matrix)

# Get year-to-year dissimilarities (i.e., row i vs. row i+1)
year_to_year <- tibble(
  year = row_years[-1],  # Year i+1
  dissimilarity = map_dbl(2:nrow(dissim_df), ~ dissim_df[.x - 1, .x])
)

ggplot(year_to_year, aes(x = year, y = dissimilarity)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Year", y = "Year-to-Year Jaccard Dissimilarity",
       title = "Species Turnover Over Time") +
  theme_minimal(base_size = 14)

#-----------------------------------------------------------------------------#
# Option 3 For facet_wrap all sites

# STEP 1: Convert to presence-absence table (wide format)
pa_matrix <- bird_territories_with_traits %>%
  filter(dominant_ELC_site_code %in% c("WOD")) %>% 
  filter(territories > 0) %>%
  distinct(sitecode, year, species_4code_IBP) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = species_4code_IBP, values_from = present, values_fill = 0)

# STEP 2: Loop over each site to compute year-to-year dissimilarity
dissim_by_site <- pa_matrix %>%
  group_split(sitecode) %>%
  map_dfr(~{
    df <- .x
    site <- df$sitecode[1]
    
    # Extract and clean matrix
    row_years <- df$year
    df_clean <- df %>% select(-sitecode, -year)
    
    if (nrow(df_clean) < 2) return(NULL)  # skip if only 1 year
    
    # Compute dissimilarity
    diss <- vegdist(df_clean, method = "jaccard", binary = TRUE)
    diss_mat <- as.matrix(diss)
    
    # Get year-to-year dissimilarity (offset = 1)
    tibble(
      sitecode = site,
      year = row_years[-1],
      dissimilarity = map_dbl(2:nrow(diss_mat), ~ diss_mat[.x - 1, .x])
    )
  })

ggplot(dissim_by_site, aes(x = year, y = dissimilarity)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~ sitecode, scales = "free_y", ncol = 3) +
  labs(x = "Year", y = "Year-to-Year Jaccard Dissimilarity") +
  geom_point(size = 1.2, # set point size
             colour = "steelblue") + # set point color
  geom_hline(yintercept = 0.5, # create horizontal plot line for '0'
             color = "black",
             linetype = "dashed") +  # set line color
  geom_hline(yintercept = 0, # create horizontal plot line for '0'
             color = "black") +  # set line color
  geom_vline(xintercept = 1990, # create horizontal plot line for '0'
             color = "black") +  # set line color)
  theme_pubr() + # set plot theme as 'publication ready'
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, # set panel spacing distance
                               "cm"), # set spacing unit
        strip.text.x = element_text(size = 8, # set text size
                                    face = "bold"), # set text type
        strip.background = element_blank(), # remove background
        plot.title = element_text(size = 12, # set text size
                                  face = "bold", # set text type
                                  hjust = 0.5, # set text location
                                  color = "black", # set text color
                                  margin = margin(b = 15)), # set margin
        axis.title.x.bottom = element_text(size = 12, # set text size
                                           face = "bold", # set text type
                                           colour = "black", # set text color
                                           margin = margin(t = 15)), # setmargin
        axis.title.y.left = element_text(size = 12, # set text size
                                         face = "bold", # set text type
                                         colour = "black", # set text color
                                         margin = margin(r = 15)), # set margin
        axis.text.x = element_text(size = 8, # set text size
                                   color = "black"), # set text color
        axis.text.y.left = element_text(size = 8, # set text size
                                        color = "black")) + # set text color
  facet_wrap(~ sitecode, # separate plots by site
             ncol = 3, # set number of columns for facet_wrap plots
             nrow = 5, # set number of rows for facet_wrap plots
             scales = "fixed") +  # set y-axis values by plot
  scale_y_continuous(limits = c(0, 1.0), # set x-axis scale limits
                     expand = c(0, 0)) +  # set x-axis scale expansion points
  scale_x_continuous(breaks = seq(min(bird_territories_with_traits$year), # set min x-axis
                                  max(bird_territories_with_traits$year), # set max x-axis
                                  by = 5), # set gap x-axis breaks
                     limits = c(1990, 2022), # set x-axis scale limits
                     expand = c(0, 0)) # set x-axis scale expansion points


#-----------------------------------------------------------------------------#
# Summary statistics:

# First and last detection years per species
species_first_last <- site_year_species %>%
  group_by(sitecode, species_4code_IBP) %>%
  summarise(first_seen = min(year), last_seen = max(year), .groups = "drop")

# Count how many "new" species appeared in each year
new_species_per_year <- species_first_last %>%
  group_by(first_seen) %>%
  tally(name = "new_species")

species_freq <- site_year_species %>%
  group_by(species_4code_IBP) %>%
  summarise(years_present = n_distinct(year)) %>%
  arrange(desc(years_present))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# DRAFT:

# 12.0 Exploring dominant and rare species

#-----------------------------------------------------------------------------#
# 12.1 Plot dominant species

# Summarize total abundance per species across all years and sites
top_species <- bird_territories_with_traits %>%
  group_by(species_4code_IBP) %>%
  summarise(Total_Territories = sum(territories,
                                    na.rm = TRUE)) %>%
  arrange(desc(Total_Territories)) %>%
  slice_max(Total_Territories,
            n = 10)

# Plot
dominantspecies_plot <- ggplot(top_species, aes(x = reorder(species_4code_IBP,
                                    -Total_Territories), 
                        y = Total_Territories)) +
  geom_bar(stat = "identity",
           fill = "steelblue") +
  theme_pubr() +
  labs(title = "Ten most abundant bird species from 1991-2021
       at Long Point, Ontario, Canada",
       x = "Species",
       y = "Total territories") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 10),
        plot.title = element_text(size = 14,
                                  face = "bold")) +

print(dominantspecies_plot)


#-----------------------------------------------------------------------------#
# 12.2 Plot top 10 species for all sites over time with 'facet_wrap'

# Organize top 10 species data for each site.
site_species_totals <- bird_territories_with_traits %>%
  group_by(sitecode, species_4code_IBP) %>%
  summarise(Total_Territories = sum(territories, na.rm = TRUE), .groups = "drop")

top10_per_site <- site_species_totals %>%
  group_by(sitecode) %>%
  slice_max(Total_Territories, n = 10, with_ties = FALSE) %>%
  ungroup()

top10_per_site <- top10_per_site %>%
  group_by(sitecode) %>%
  mutate(species_ordered = factor(species_4code_IBP, levels = species_4code_IBP[order(Total_Territories)])) %>%
  ungroup()

dominantspecies_plot_wrapped <- ggplot(top10_per_site, 
                                       aes(x = species_ordered,
                                           y = Total_Territories)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ sitecode,
             scales = "free",
             ncol = 3) +
  theme_pubr() +
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"),
        strip.text.x = element_text(size = 8,
                                    face = "bold"),
        strip.background = element_blank(),
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Customizes x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Customizes y-axis title
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 7,
                                   color = "black"),
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) +
  labs(x = "Species",
       y = "Total male breeding bird territories")

print(dominantspecies_plot_wrapped)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 13.0 Exploring species turnover and composition change over time

#-----------------------------------------------------------------------------#
# 13.1 Plot species turnover and community composition shifts

# Step 1: Create presence/absence table (1 if species is present in a year)
species_turnover <- bird_territories_with_traits %>%
  group_by(year, species_4code_IBP) %>%
  summarise(Present = as.integer(any(territories > 0)), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Present, values_fill = 0)  # <-- pivot differently: year → column

class(species_matrix)

# Step 2: Create matrix
# Example: assume 'species_turnover' dataframe with species in first column

species_matrix <- as.matrix(species_turnover[ , -1])       # Remove species column
rownames(species_matrix) <- species_turnover[[1]]          # Assign species as row names

# Now reorder rows alphabetically by species names
species_matrix <- species_matrix[order(rownames(species_matrix)), ]

row_ann <- data.frame(Guild = bird_territories_with_traits$nest_guild_BOTW)
rownames(row_ann) <- bird_territories_with_traits$species_4code_IBP

# Step 3: Plot the heatmap
pres_abs_heatmap <- pheatmap(species_matrix,
         cluster_rows = FALSE,    # No clustering of species
         cluster_cols = FALSE,    # No clustering of years
         fontsize_row = 6,        # Font size for species names
         fontsize_col = 8,       # Font size for years
         angle_row = 45,
         angle_col = 45,          # Rotate year labels for better readability
         color = c("white", "steelblue"),  # Black = presence
         border_color = "grey",
         legend = TRUE,
         legend_breaks = c(0, 1),
         legend_labels = c("Absent", "Present"),
         show_rownames = TRUE,
         show_colnames = TRUE,
         annotation_row = row_ann,
         annotation_colors = list(Guild = c(
           "ground" = "green", "shrub" = "darkgreen", "tree" = "goldenrod")))

print(pres_abs_heatmap)

#-----------------------------------------------------------------------------#
# 13.2 Plot species presence/absence with facet_wrap

# Step 1: Create presence/absence long-format data
presence_df <- bird_territories_with_traits %>%
  mutate(Present = as.integer(territories > 0)) %>%
  group_by(sitecode, year, species_4code_IBP) %>%
  summarise(Present = max(Present), .groups = "drop")  # presence if territories > 0

# Step 2: Get unique list of sites
site_list <- unique(presence_df$sitecode)

# Step 3: Loop through each site and plot a heatmap
for (site in site_list) {
  
  site_data <- presence_df %>%
    filter(sitecode == site)
  
  heatmap_plot <- ggplot(site_data, aes(x = year, y = fct_rev(factor(species_4code_IBP)), fill = factor(Present))) +
    geom_tile(color = "white", 
              linewidth = 0.1) +
    scale_fill_manual(values = c("0" = "white",
                                 "1" = "black"),
                      labels = c("Absent",
                                 "Present"),
                      name = "Presence") +
    theme_pubr() +
    theme(legend.position = "bottom", # remove legend
          plot.title = element_text(size = 12, # specify text size
                                    face = "bold", # specify text style
                                    hjust = 0.5, # specify text location
                                    color = "black", # specify text colour
                                    margin = margin(b = 15)), # specify margin
          axis.title.x.bottom = element_text(size = 12, # specify text size
                                             face = "bold", # specify text style
                                             hjust = 0.5, # specify text location
                                             color = "black"), # specify margin
          axis.title.y.left = element_text(size = 6, # specify text size
                                           face = "bold", # specify text style
                                           hjust = 0.5, # specify text location
                                           vjust = 1.0, # specify text location
                                           color = "black"), # specify colour
          axis.text.x = element_text(size = 8, # specify text size
                                     color = "black"), # specify margin
          axis.text.y.left = element_text(size = 8, # specify text size
                                          color = "black")) + # specify margin
    labs(x = "Year", # x-axis label for plot
         y = paste("Species at:", site)) + # y-axis label for plot
    scale_x_continuous(breaks = seq(min(site_data$year),
                                    max(site_data$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))
  
  # Display the plot
  print(heatmap_plot)
}


#-----------------------------------------------------------------------------#
# 13.3 Plot community composition shifts with NMDS models

# Step 1: Filter to years of interest
community_data_subset <- bird_territories_with_traits %>%
  filter(year %in% c(1991, 2016, 2021))

# Step 2: Summarize to species abundance per year
community_matrix <- community_data_subset %>%
  group_by(year, species_4code_IBP) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = species_4code_IBP, values_from = total_abundance, values_fill = 0)

# Step 3: Separate metadata
year_labels <- community_matrix$year
comm_matrix <- community_matrix %>% select(-year)

# Step 4: Run NMDS (Bray-Curtis)
set.seed(123)
nmds_result <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)

# Step 5: Scores for sites and species
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$year <- as.factor(year_labels)

species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$species <- rownames(species_scores)

# Step 6: Compute convex hulls for year groups
hull_data <- site_scores %>%
  group_by(year) %>%
  slice(chull(NMDS1, NMDS2))

# Step 7: Plot
ggplot() +
  # Convex hull polygons
  geom_polygon(data = hull_data,
               aes(x = NMDS1, y = NMDS2, fill = year, group = year),
               alpha = 0.3, color = "black") +
  
  # Site points (year centroids)
  geom_point(data = site_scores,
             aes(x = NMDS1, y = NMDS2, color = year),
             size = 4) +
  
  # Species scores
  geom_text(data = species_scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            color = "black", size = 3, alpha = 0.8, check_overlap = TRUE) +
  
  scale_color_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  scale_fill_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  
  labs(title = "NMDS of Bird Community Composition (1991, 2016, 2021)",
       x = "NMDS Axis 1", y = "NMDS Axis 2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

#-----------------------------------------------------------------------------#
# OPTION 2

# Step 1: Filter to 3 focal years and species with total abundance ≥ 1.0
community_data_subset <- bird_territories_with_traits %>%
  filter(year %in% c(1991, 2016, 2021)) %>%
  group_by(species_4code_IBP) %>%
  filter(sum(territories, na.rm = TRUE) >= 1.0) %>%
  ungroup()

# Step 2: Summarize abundance per year and species
community_matrix <- community_data_subset %>%
  group_by(year, species_4code_IBP) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = species_4code_IBP, values_from = total_abundance, values_fill = 0)

# Step 3: Extract year labels and community matrix
year_labels <- community_matrix$year
comm_matrix <- community_matrix %>% select(-year)

# Step 4: Run NMDS (Bray-Curtis)
set.seed(123)
nmds_result <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)

# Step 5: Get NMDS scores
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$year <- as.factor(year_labels)

species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$species <- rownames(species_scores)

# Step 6: Create convex hulls for year groups
hull_data <- site_scores %>%
  group_by(year) %>%
  slice(chull(NMDS1, NMDS2))

# Step 7: Plot NMDS with polygons and species arrows
ggplot() +
  # Convex hull polygons by year
  geom_polygon(data = hull_data,
               aes(x = NMDS1, y = NMDS2, fill = year, group = year),
               alpha = 0.3, color = "black") +
  
  # NMDS points for year centroids
  geom_point(data = site_scores,
             aes(x = NMDS1, y = NMDS2, color = year),
             size = 4) +
  
  # Species arrows
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "gray30", linewidth = 0.6) +
  
  # Optional: label a few species near arrow tips
  geom_text_repel(data = species_scores,
                  aes(x = NMDS1, y = NMDS2, label = species),
                  size = 3, color = "black",
                  max.overlaps = 50, segment.size = 0.2, box.padding = 0.3) +
  
  scale_color_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  scale_fill_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  
  labs(
    title = "NMDS of Bird Community Composition (1991, 2016, 2021)",
    subtitle = "Polygons represent Bray-Curtis dissimilarity per year\nArrows show direction of species influence",
    x = "NMDS Axis 1",
    y = "NMDS Axis 2",
    color = "Year",
    fill = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

#-----------------------------------------------------------------------------#
# OPTION 3

# Step 1: Filter data to 3 focal years and species with total abundance ≥ 1.0
# species which are shrub-nesting but not marsh habitat preferred, and within
# only the 5 WOD sites
community_data_subset <- bird_territories_with_traits %>%
  filter(sitecode %in% c("ROWB",
                         "ROMS",
                         "ROIS",
                         "RARO",
                         "ROMF")) %>%
  filter(year %in% c(1991,
                     2016,
                     2021)) %>%
  group_by(species_4code_IBP) %>%
  filter(sum(territories,
             na.rm = TRUE) >= 1.0) %>%
  ungroup()

# Step 2: Create site-year-species matrix (allows per-site NMDS)
community_matrix <- community_data_subset %>%
  group_by(sitecode,
           year,
           species_4code_IBP) %>%
  summarise(total_abundance = sum(territories,
                                  na.rm = TRUE),
            .groups = "drop") %>%
  unite("site_year",
        sitecode,
        year,
        remove = FALSE) %>%
  pivot_wider(names_from = species_4code_IBP,
              values_from = total_abundance,
              values_fill = 0)

# Step 3: Extract metadata
meta_data <- community_matrix %>% select(sitecode,
                                         year,
                                         site_year)
comm_matrix <- community_matrix %>% select(-sitecode,
                                           -year,
                                           -site_year)

# Step 4: Run NMDS with Bray-Curtis dissimilarity
set.seed(123)
nmds_result <- metaMDS(comm_matrix,
                       distance = "bray",
                       k = 2,
                       trymax = 100)

# Step 5: Extract NMDS coordinates
site_scores <- as.data.frame(scores(nmds_result,
                                    display = "sites"))
site_scores$site_year <- meta_data$site_year
site_scores$year <- meta_data$year
site_scores$sitecode <- meta_data$sitecode

# Step 6: Extract species scores and calculate vector lengths
species_scores <- as.data.frame(scores(nmds_result,
                                       display = "species"))
species_scores$species <- rownames(species_scores)
species_scores$vector_length <- sqrt(species_scores$NMDS1^2 + species_scores$NMDS2^2)

# Step 7: Select top N contributing species
top_species <- species_scores %>%
  arrange(desc(vector_length)) %>%
  slice_head(n = 9)  # Top 12 species

# Step 8: Create convex hulls per year
hull_data <- site_scores %>%
  group_by(year) %>%
  slice(chull(NMDS1,
              NMDS2))

# Ensure 'year' is treated as a discrete factor
site_scores$year <- as.factor(site_scores$year)
hull_data$year <- as.factor(hull_data$year)


# Step 9: Plot NMDS with polygons, sites, arrows, and top species labels
ggplot() +
  # Polygons for each year's community spread
  geom_polygon(data = hull_data,
               aes(x = NMDS1,
                   y = NMDS2,
                   fill = year,
                   group = year),
               alpha = 0.2,
               color = "black",
               linewidth = 0.5) +
  
  # Site-level NMDS points
  geom_point(data = site_scores,
             aes(x = NMDS1,
                 y = NMDS2,
                 color = year),
             size = 2,
             alpha = 0.8) +
  
  # Arrows for species vectors
  geom_segment(data = top_species,
               aes(x = 0,
                   y = 0,
                   xend = NMDS1,
                   yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "gray30", linewidth = 0.6) +
  
  # Labels for top species
  geom_text_repel(data = top_species,
                  aes(x = NMDS1, y = NMDS2, label = species),
                  size = 3, color = "black",
                  segment.color = "gray50", max.overlaps = 50) +
  
  scale_color_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  scale_fill_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  
  labs(x = "NMDS 1",
    y = "NMDS 2",
    color = "Year",
    fill = "Year"
  ) +
  theme_pubr() +
  facet_wrap(~year)
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 14.0 Exploring beta diversity

#-----------------------------------------------------------------------------#
# 14.1 Beta Diversity & Similarity Between Sites

# Create a site-species matrix (presence-absence)
species.diversity.matrix <- bird_territories_with_traits %>%
  group_by(year,
           sitecode,
           species_4code_IBP) %>%
  summarise(total_territories = sum(territories),
            .groups = "drop") %>%
  spread(species_4code_IBP, 
         total_territories, 
         fill = 0)

# Compute Jaccard or Bray-Curtis Dissimilarity
jaccard_dist <- vegdist(species.diversity.matrix[,-2], 
                        method = "jaccard")

bray_dist <- vegdist(species.diversity.matrix[,-2], 
                     method = "bray")

# Perform Hierarchical Clustering
hc <- hclust(jaccard_dist, method = "average")

# Plot Dendrogram
plot(hc, 
     main = "Hierarchical Clustering of Sites (Jaccard)", 
     xlab = "Sites")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 15.0 GAMs: Exploring non-linear trends in abundance

#-----------------------------------------------------------------------------#
# 15.1 total number of male breeding bird territories

total_abund <- bird_territories_with_traits %>%
  group_by(sitecode, 
           year) %>%
  summarise(total_territories = sum(territories, 
                                    na.rm = TRUE))

total_abund <- total_abund %>%
  mutate(sitecode = as.factor(sitecode))


# Create a generalized additive model for abundance
gam_total <- gam(total_territories ~ s(year, by = sitecode) + sitecode,
                 data = total_abund,
                 family = nb())  # use for overdispersion



# Get predictions and confidence intervals
preds <- predict(gam_total, newdata = total_abund, type = "response", se.fit = TRUE)

# Add to your original data (assuming rows align)
total_abund$Fit <- preds$fit
total_abund$SE <- preds$se.fit

# Plot
gam_model <- ggplot(total_abund, 
                    aes(x = year, 
                        y = total_territories,
                        group = sitecode)) +
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  geom_line(aes(y = Fit), color = "black") +
  geom_ribbon(aes(ymin = Fit - 1.96*SE, 
                  ymax = Fit + 1.96*SE), 
              alpha = 0.2) +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode,
             ncol = 3,
             nrow = 5, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Number of male breeding bird territories") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(total_abund$year),
                                  max(total_abund$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

  
summary(gam_model)

#Visualize
print(gam_model)

#-----------------------------------------------------------------------------#
# 15.2 total number of male breeding bird territories

# Filter for only shrub-nesting species within WOD sites, and exclude any of
# the shrub-nesting species which prefer Marsh habitat.

# Create plot
total_abund <- bird_territories_with_traits %>%
  filter(sitecode %in% c("ROWB",
                         "ROMS",
                         "ROIS",
                         "RARO",
                         "ROMF")) %>%
  filter(nest_guild_BOTW %in% c("shrub")) %>%
  filter(habitat_guild_BOTW %in% c("scrub",
                                   "open woodlands",
                                   "towns")) %>%
  group_by(sitecode, 
           year) %>%
  summarise(total_territories = sum(territories, 
                                    na.rm = TRUE))

total_abund <- total_abund %>%
  mutate(sitecode = as.factor(sitecode))


# Create a generalized additive model for abundance
gam_total <- gam(total_territories ~ s(year, by = sitecode) + sitecode,
                 data = total_abund,
                 family = nb())  # use for overdispersion



# Get predictions and confidence intervals
preds <- predict(gam_total, newdata = total_abund, type = "response", se.fit = TRUE)

# Add to your original data (assuming rows align)
total_abund$Fit <- preds$fit
total_abund$SE <- preds$se.fit

# Plot
gam_model <- ggplot(total_abund, 
                    aes(x = year, 
                        y = total_territories,
                        group = sitecode)) +
  geom_point(size = 1.2,
             colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  geom_line(aes(y = Fit), color = "black") +
  geom_ribbon(aes(ymin = Fit - 1.96*SE, 
                  ymax = Fit + 1.96*SE), 
              alpha = 0.2) +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8,
                                    face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12,
                                         face = "bold",
                                         colour = "black",
                                         margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode,
             ncol = 2,
             nrow = 3, 
             scales = "free_y") +
  labs(x = "Year",
       y = "Number of male breeding bird territories") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(total_abund$year),
                                  max(total_abund$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))


summary(gam_model)

#Visualize
print(gam_model)

#-----------------------------------------------------------------------------#
# 15.2.1 Save plot for record (OPTIONAL)

# Save plot
# ggsave(filename = paste0("../results/plots/lpbbc_GAM_abundance_wrap_plot.png"),
#       plot = gam_model, 
#       width = 8.18, 
#       height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 16.0 GAMs: Exploring non-linear trends in abundance

#-----------------------------------------------------------------------------#
# 16.1 Individual species trends by year-site

# Filter for species with >5 records
species_filtered <- bird_territories_with_traits %>%
  group_by(sitecode, 
           species_4code_IBP) %>%
  filter(sum(territories,
             na.rm = TRUE) >= 5)

# Filter for species with >4 monitoring years
species_filtered_clean <- species_filtered %>%
  group_by(sitecode, species_4code_IBP) %>%
  filter(n_distinct(year) >= 4) %>%
  ungroup()

# Create generalized additive models for each filtered species
safe_gam <- safely(~ gam(territories ~ s(year,
                                         k = 4),
                         data = ., 
                         family = nb()))

# Add error handling to model
species_models <- species_filtered_clean %>%
  group_by(sitecode, species_4code_IBP) %>%
  group_split() %>%
  map(safe_gam)

# Check model success
successful_models <- species_models %>%
  keep(~ is.null(.x$error))
failed_models <- species_models %>%
  keep(~ !is.null(.x$error))

# Extract summary information from GAMs
model_summaries <- tibble(
  sitecode = species_filtered_clean %>%
    group_by(sitecode, species_4code_IBP) %>%
    group_keys() %>%
    slice(map_lgl(species_models, ~ is.null(.x$error))) %>% 
    pull(sitecode),
  
  species = species_filtered_clean %>%
    group_by(sitecode, species_4code_IBP) %>%
    group_keys() %>%
    slice(map_lgl(species_models, ~ is.null(.x$error))) %>% 
    pull(species_4code_IBP),
  
  model = map(successful_models, ~ .x$result)
)
# Result was error...

# Option 2

# Step 1: Group and nest your cleaned dataset
species_nested <- species_filtered_clean %>%
  group_by(sitecode, species_4code_IBP) %>%
  nest()

# Step 2: Fit models with safely
safe_gam <- safely(~ gam(territories ~ s(year, k = 4), data = ., family = nb()))

species_nested <- species_nested %>%
  mutate(model_result = map(data, safe_gam))


# Create columns for successful models and any errors
species_nested <- species_nested %>%
  mutate(
    model = map(model_result, "result"),
    error = map(model_result, "error")
  )

# Keep only successful models
successful_models <- species_nested %>%
  filter(map_lgl(error, is.null))

model_summaries <- successful_models %>%
  mutate(
    gam_summary   = map(model, summary),
    edf           = map_dbl(gam_summary, ~ .x$s.table[1, "edf"]),
    p_value       = map_dbl(gam_summary, ~ .x$s.table[1, "p-value"]),
    deviance_expl = map_dbl(gam_summary, ~ .x$dev.expl),
    aic           = map_dbl(model, AIC)
  ) %>%
  select(sitecode, species_4code_IBP, edf, p_value, deviance_expl, aic)

model_summaries %>%
  filter(p_value < 0.05, edf > 1.5) %>%
  arrange(desc(deviance_expl))

# Visualize

# Add predictions (fitted values + standard errors) to each model
successful_models <- successful_models %>%
  mutate(
    preds = map2(model, data, ~ augment(.x, newdata = .y, se_fit = TRUE))
  )

plot_data <- successful_models %>%
  select(sitecode, species_4code_IBP, preds) %>%
  unnest(preds)

ggplot(plot_data, aes(x = year, y = .fitted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = .fitted - 1.96 * .se.fit,
                  ymax = .fitted + 1.96 * .se.fit), alpha = 0.3) +
  geom_point(aes(y = territories), size = 1, alpha = 0.5) +
  labs(
    x = "Year",
    y = "Predicted Territories",
    title = "GAM Fitted Trends by Species and Site"
  ) +
  theme_minimal(base_size = 11)


plot_data %>%
  filter(sitecode == "BGGR", species_4code_IBP == "SOSP") %>%
  ggplot(aes(x = year, y = .fitted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = .fitted - 1.96 * .se.fit,
                  ymax = .fitted + 1.96 * .se.fit), alpha = 0.3) +
  geom_point(aes(y = territories), size = 2, alpha = 0.6) +
  labs(
    x = "Year",
    y = "Predicted Territories",
    title = "Song Sparrow at BGGR"
  ) +
  theme_minimal()

# VISUALIZE EACH SPECIES x SITE combination

# Get unique combinations of species and site
unique_combos <- plot_data %>%
  distinct(sitecode, species_4code_IBP)

# Loop over each combination and print the plot
for (i in 1:nrow(unique_combos)) {
  site_i <- unique_combos$sitecode[i]
  sp_i   <- unique_combos$species_4code_IBP[i]
  
  p <- plot_data %>%
    filter(sitecode == site_i, species_4code_IBP == sp_i) %>%
    ggplot(aes(x = year, y = .fitted)) +
    theme_pubr() + # Formats plot to a 'publication ready' theme
    theme(legend.position = "none", # Removes the legend
          panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
          strip.text.x = element_text(size = 8,
                                      face = "bold"), # Formats subtitles within the wrapped plot
          strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
          plot.title = element_text(size = 12,
                                    face = "bold",
                                    hjust = 0.5,
                                    color = "black",
                                    margin = margin(b = 15)), # Formats the plot title
          axis.title.x.bottom = element_text(size = 12,
                                             face = "bold",
                                             colour = "black",
                                             margin = margin(t = 15)), # Formats x-axis title
          axis.title.y.left = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(r = 15)), # Formats y-axis title
          axis.text.x = element_text(size = 8,
                                     color = "black"), # Formats x-axis text
          axis.text.y.left = element_text(size = 8,
                                          color = "black")) + # Formats y-axis text
    geom_line(color = "black") +
    geom_ribbon(aes(ymin = .fitted - 1.96 * .se.fit,
                    ymax = .fitted + 1.96 * .se.fit), alpha = 0.3) +
    geom_point(aes(y = territories), color = "steelblue",
               size = 2, alpha = 0.6) +
    geom_hline(yintercept = 0,
               color = "black") +  # Horizontal line
    labs(
      title = paste(sp_i, "at", site_i),
      x = "Year",
      y = "Predicted Territories"
    ) +
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(breaks = seq(min(plot_data$year),
                                    max(plot_data$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))
  print(p)
  
  # Pause after each plot
  readline(prompt = "Press [Enter] to view the next plot...")
}


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 17.0 Breeding bird territory totals for woodland sites: data visualizations

#-----------------------------------------------------------------------------#
# 17.1 Plot nesting guild trends over time, for individual sites

# Join site information data with breeding bird territory data
site.abund.data <- full_join(bird_territories_with_traits,
                             site_traits,
                             by = c("sitecode"))

unique(site.abund.data$dominant_ELC_site_code)

# Step 1: Filter to shrub guild and FOD sites
shrub.x.fod.density.data <- site.abund.data %>%
  filter(nest_guild_BOTW == "shrub",
         grepl("WOD", dominant_ELC_site_code)) %>%
  
  # Step 2: Calculate territory density per site/species/year
  mutate(density = territories / site_size_ha) %>%
  
  # Step 3: Pool across species and sites per year (sum of densities)
  group_by(year) %>%
  summarise(total_density = sum(density, na.rm = TRUE)) %>%
  ungroup()

# fit GAM
gam_shrub_fod_density <- gam(total_density ~ s(year),
                             data = shrub.x.fod.density.data,
                             family = nb(),
                             method = "REML")

# Add model predictions
pred_data <- shrub.x.fod.density.data %>%
  mutate(predicted = predict(gam_shrub_fod_density, 
                             type = "response"))

# Plot
wod.plot <- ggplot(pred_data,
       aes(x = year,
           y = total_density)) +
  geom_point(color = "darkgreen",
             size = 2,
             alpha = 0.6) +
  geom_line(aes(y = predicted),
            color = "blue",
            size = 1.2) +
  labs(title = "Shrub Guild Territory Density in Woodland (WOD) Sites Over Time",
       x = "Year", y = "Territories per Hectare") +
  theme_pubr()

print(wod.plot)


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 18.0 Breeding bird territory totals for woodland sites: data visualizations

#-----------------------------------------------------------------------------#
# 18.1 Plot nesting guild trends over time, for all WOD sites.

# Get a list of unique guilds (standardized to lowercase)
guilds <- site.abund.data %>%
  mutate(guild = tolower(nest_guild_BOTW)) %>%
  filter(grepl("WOD", 
               dominant_ELC_site_code)) %>%
  distinct(guild) %>%
  pull()

# Create an empty list to store plots
plot_list <- list()

# Create an empty list to store GAM results
model_list <- list()

# Loop through each guild
for (g in guilds) {
  # Step 1: Filter data to current guild and FOD sites
  guild_data <- site.abund.data %>%
    mutate(guild = tolower(nest_guild_BOTW)) %>%
    filter(guild == g, 
           grepl("WOD", 
                 dominant_ELC_site_code)) %>%
    filter(!is.na(site_size_ha), 
           site_size_ha > 0) %>%
    mutate(density = territories / site_size_ha)
  
  # Step 2: Summarize total density per year
  summary_data <- guild_data %>%
    group_by(year) %>%
    summarise(total_density = sum(density,
                                  na.rm = TRUE)) %>%
    ungroup()
  
  # Skip if there's not enough data to model
  if (nrow(summary_data) < 5 || sum(summary_data$total_density) == 0) next
  
  # Step 3: Fit GAM model
  gam_model <- gam(total_density ~ s(year, k = 5),
                   data = summary_data,
                   family = nb(),
                   method = "REML")
  
  # Step 4: Predict with GAM
  preds <- predict(gam_model, 
                   se.fit = TRUE,
                   type = "response")
  summary_data$predicted <- preds$fit
  summary_data$lower <- preds$fit - 1.96 * preds$se.fit
  summary_data$upper <- preds$fit + 1.96 * preds$se.fit
  
  # Step 5: Plot
  p <- ggplot(summary_data,
              aes(x = year,
                  y = predicted)) +
    geom_point(aes(y = total_density),
               color = "darkgreen",
               size = 2,
               alpha = 0.6) +
    geom_line(color = "blue"
              , size = 1.2) +
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                alpha = 0.2,
                fill = "blue") +
    labs(title = paste("Territory Density Trend:", g, "guild"),
         x = "Year", y = "Territories per Hectare") +
    theme_pubr()
  
  # Store the plot in the list
  plot_list[[g]] <- p
  
  # Inside your loop, after fitting the model:
  model_list[[g]] <- gam_model
  
}

for (p in plot_list) print(p)

for (gam_model in model_list) print(gam_model)

# Create a summary table for all guild models
gam_results <- purrr::map_dfr(names(model_list), function(g) {
  mod <- model_list[[g]]
  tidy(mod, parametric = FALSE) %>%
    mutate(guild = g)
})

gam_results



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 19.0 Breeding bird territory totals for Coniferous sites: data visualizations

#-----------------------------------------------------------------------------#
# 19.1 Plot nesting guild trends over time, for all Coniferous Forest sites.

# Get a list of unique guilds (standardized to lowercase)
guilds <- site.abund.data %>%
  mutate(guild = tolower(nest_guild_BOTW)) %>%
  filter(grepl("FOC", 
               dominant_ELC_site_code)) %>%
  distinct(guild) %>%
  pull()

# Create an empty list to store plots
plot_list <- list()

# Create an empty list to store GAM results
model_list <- list()

# Loop through each guild
for (g in guilds) {
  # Step 1: Filter data to current guild and FOD sites
  guild_data <- site.abund.data %>%
    mutate(guild = tolower(nest_guild_BOTW)) %>%
    filter(guild == g, 
           grepl("FOC", 
                 dominant_ELC_site_code)) %>%
    filter(!is.na(site_size_ha), 
           site_size_ha > 0) %>%
    mutate(density = territories / site_size_ha)
  
  # Step 2: Summarize total density per year
  summary_data <- guild_data %>%
    group_by(year) %>%
    summarise(total_density = sum(density,
                                  na.rm = TRUE)) %>%
    ungroup()
  
  # Skip if there's not enough data to model
  if (nrow(summary_data) < 5 || sum(summary_data$total_density) == 0) next
  
  # Step 3: Fit GAM model
  gam_model <- gam(total_density ~ s(year, k = 5),
                   data = summary_data,
                   family = nb(),
                   method = "REML")
  
  # Step 4: Predict with GAM
  preds <- predict(gam_model, 
                   se.fit = TRUE,
                   type = "response")
  summary_data$predicted <- preds$fit
  summary_data$lower <- preds$fit - 1.96 * preds$se.fit
  summary_data$upper <- preds$fit + 1.96 * preds$se.fit
  
  # Step 5: Plot
  p <- ggplot(summary_data,
              aes(x = year,
                  y = predicted)) +
    geom_point(aes(y = total_density),
               color = "darkgreen",
               size = 2,
               alpha = 0.6) +
    geom_line(color = "blue"
              , size = 1.2) +
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                alpha = 0.2,
                fill = "blue") +
    labs(title = paste("Territory Density Trend:", g, "guild"),
         x = "Year", y = "Territories per Hectare") +
    theme_pubr()
  
  # Store the plot in the list
  plot_list[[g]] <- p
  
  # Inside your loop, after fitting the model:
  model_list[[g]] <- gam_model
  
}

for (p in plot_list) print(p)

for (gam_model in model_list) print(gam_model)

# Create a summary table for all guild models
gam_results <- purrr::map_dfr(names(model_list), function(g) {
  mod <- model_list[[g]]
  tidy(mod, parametric = FALSE) %>%
    mutate(guild = g)
})

gam_results



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 20.0 Breeding bird territory totals for ROWB site: data visualizations

#-----------------------------------------------------------------------------#
# 20.1 Plot nesting guild trends over time, for the ROWB site.

# Get a list of unique guilds (standardized to lowercase)
guilds <- site.abund.data %>%
  mutate(guild = tolower(nest_guild_BOTW)) %>%
  filter(grepl("ROWB", 
               sitecode)) %>%
  distinct(guild) %>%
  pull()

# Create an empty list to store plots
plot_list <- list()

# Create an empty list to store GAM results
model_list <- list()

# Loop through each guild
for (g in guilds) {
  # Step 1: Filter data to current guild and FOD sites
  guild_data <- site.abund.data %>%
    mutate(guild = tolower(nest_guild_BOTW)) %>%
    filter(guild == g, 
           grepl("ROWB", 
                 sitecode)) %>%
    filter(!is.na(site_size_ha), 
           site_size_ha > 0) %>%
    mutate(density = territories / site_size_ha)
  
  # Step 2: Summarize total density per year
  summary_data <- guild_data %>%
    group_by(year) %>%
    summarise(total_density = sum(density,
                                  na.rm = TRUE)) %>%
    ungroup()
  
  # Skip if there's not enough data to model
  if (nrow(summary_data) < 5 || sum(summary_data$total_density) == 0) next
  
  # Step 3: Fit GAM model
  gam_model <- gam(total_density ~ s(year, k = 5),
                   data = summary_data,
                   family = nb(),
                   method = "REML")
  
  # Step 4: Predict with GAM
  preds <- predict(gam_model, 
                   se.fit = TRUE,
                   type = "response")
  summary_data$predicted <- preds$fit
  summary_data$lower <- preds$fit - 1.96 * preds$se.fit
  summary_data$upper <- preds$fit + 1.96 * preds$se.fit
  
  # Step 5: Plot
  p <- ggplot(summary_data,
              aes(x = year,
                  y = predicted)) +
    geom_point(aes(y = total_density),
               color = "darkgreen",
               size = 2,
               alpha = 0.6) +
    geom_line(color = "blue"
              , size = 1.2) +
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                alpha = 0.2,
                fill = "blue") +
    labs(title = paste("Territory Density Trend:", g, "guild"),
         x = "Year", y = "Territories per Hectare") +
    theme_pubr()
  
  # Store the plot in the list
  plot_list[[g]] <- p
  
  # Inside your loop, after fitting the model:
  model_list[[g]] <- gam_model
  
}

for (p in plot_list) print(p)

for (gam_model in model_list) print(gam_model)

# Create a summary table for all guild models
gam_results <- purrr::map_dfr(names(model_list), function(g) {
  mod <- model_list[[g]]
  tidy(mod, parametric = FALSE) %>%
    mutate(guild = g)
})

gam_results

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 21.0 Breeding bird territory totals for ROWB site: data visualizations

#-----------------------------------------------------------------------------#
# 21.1 Plot shrub nesting guild trends over time, for the WOD site.

# 1. Get all sitecodes where ELC class is "WOD"
wod_sites <- site.abund.data %>%
  filter(dominant_ELC_site_code == "WOD") %>%
  distinct(sitecode) %>%
  pull()

# 2. Empty lists to store results
plot_list <- list()
model_list <- list()

# 3. Loop over WOD sites, modeling shrub guild only
for (s in wod_sites) {
  
  # Step 1: Filter to shrub guild and WOD site
  site_data <- site.abund.data %>%
    mutate(guild = tolower(nest_guild_BOTW)) %>%
    filter(guild == "shrub",
           sitecode == s,
           dominant_ELC_site_code == "WOD") %>%
    filter(!is.na(site_size_ha), site_size_ha > 0) %>%
    mutate(density = territories / site_size_ha)
  
  # Step 2: Summarize total density by year
  summary_data <- site_data %>%
    group_by(year) %>%
    summarise(total_density = sum(density, na.rm = TRUE)) %>%
    ungroup()
  
  # Skip sites with insufficient data
  if (nrow(summary_data) < 5 || sum(summary_data$total_density) == 0) next
  
  # Step 3: Fit GAM
  gam_model <- gam(total_density ~ s(year, k = 5),
                   data = summary_data,
                   family = nb(),
                   method = "REML")
  
  # Step 4: Predict + confidence intervals
  preds <- predict(gam_model, se.fit = TRUE, type = "response")
  summary_data <- summary_data %>%
    mutate(predicted = preds$fit,
           lower = predicted - 1.96 * preds$se.fit,
           upper = predicted + 1.96 * preds$se.fit)
  
  # Step 5: Plot
  p <- ggplot(summary_data, aes(x = year, y = predicted)) +
    geom_point(aes(y = total_density), color = "darkgreen", size = 2, alpha = 0.6) +
    geom_line(color = "blue", size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
    labs(title = paste("Shrub Guild Trend at", s),
         x = "Year", y = "Territories per Hectare") +
    theme_pubr()
  
  # Store results
  plot_list[[s]] <- p
  model_list[[s]] <- gam_model
}

for (p in plot_list) print(p)

for (gam_model in model_list) print(gam_model)

# Create a summary table for all guild models
gam_results <- purrr::map_dfr(names(model_list), function(g) {
  mod <- model_list[[g]]
  tidy(mod, parametric = FALSE) %>%
    mutate(guild = g)
})

gam_results

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 22.0 Breeding bird territory totals for Yellow Warbler: data visualizations

#-----------------------------------------------------------------------------#
# 22.1 Plot Yellow Warbler trends over time, for all sites.

# 1. Filter for YEWA and compute density
yewa_data <- site.abund.data %>%
  filter(species_4code_IBP == "YEWA",
         !is.na(site_size_ha),
         site_size_ha > 0) %>%
  mutate(density = territories / site_size_ha)

# 2. Summarize total density per year (across all sites)
summary_data <- yewa_data %>%
  group_by(year) %>%
  summarise(total_density = sum(density, na.rm = TRUE)) %>%
  ungroup()

# 3. Fit GAM model (Poisson or Negative Binomial if overdispersion)
gam_model <- gam(total_density ~ s(year, k = 5),
                 data = summary_data,
                 family = nb(),  # Use 'nb()' if overdispersed
                 method = "REML")

# 4. Predict and calculate confidence intervals
preds <- predict(gam_model, se.fit = TRUE, type = "response")
summary_data <- summary_data %>%
  mutate(predicted = preds$fit,
         lower = predicted - 1.96 * preds$se.fit,
         upper = predicted + 1.96 * preds$se.fit)

# 5. Plot
ggplot(summary_data, aes(x = year, y = predicted)) +
  geom_point(aes(y = total_density), color = "darkgreen", size = 2, alpha = 0.6) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  labs(title = "Yellow Warbler (YEWA) Territory Density Trend (All Sites)",
       x = "Year", y = "Territories per Hectare") +
  theme_pubr()

summary(gam_model)

gam.check(gam_model)





#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 22.0 Breeding bird territory totals for Yellow Warbler: data visualizations

#-----------------------------------------------------------------------------#
# 22.1 

bird_territories_with_traits_and_site_characteristics <- full_join(bird_territories_with_traits,
                                                                   site_traits,
                                                                   by = c("sitecode"))

# GAMM for abundance


bird_territories_with_traits_and_site_characteristics$sitecode <- as.factor(
  bird_territories_with_traits_and_site_characteristics$sitecode
)

bird_territories_with_traits_and_site_characteristics$species_4code_LPBBC <- as.factor(
  bird_territories_with_traits_and_site_characteristics$species_4code_LPBBC
)


df_filtered <- bird_territories_with_traits_and_site_characteristics %>%
  filter(dominant_ELC_site_code == "WOD",
         nest_guild_BOTW == "shrub") %>%
  mutate(
    sitecode = as.factor(sitecode),
    species_4code_LPBBC = as.factor(species_4code_LPBBC)
  )

length(unique(df_filtered$species_4code_LPBBC))

length(unique(df_filtered$sitecode))

gamm_density <- gam(
  territories ~ s(year) +
    s(sitecode, bs = "re") + 
    s(species_4code_LPBBC, bs = "re") +
    offset(log(site_size_ha)),
  data = df_filtered,
  family = nb(),
  method = "REML")

# Predict confidence intervals
preds <- predict(gamm_density,
                 se.fit = TRUE,
                 type = "response")

# Calculate confidence intervals
summary_data <- df_filtered %>%
  mutate(predicted = preds$fit,
         lower = predicted - 1.96 * preds$se.fit,
         upper = predicted + 1.96 * preds$se.fit)

# 5. Plot
ggplot(summary_data,
       aes(x = year,
           y = predicted)) +
  geom_point(aes(y = territories),
             color = "darkgreen",
             size = 2,
             alpha = 0.6) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              fill = "blue",
              alpha = 0.2) +
  labs(x = "Year",
       y = "Territory density") +
  theme_pubr()

summary(gamm_density)

gam.check(gamm_density)

#-----------------------------------------------------------------------------#
# OPTION 2:

# Step 1: Filter for shrub-dependent species and WOD sites
shrub_wod_df <- bird_territories_with_traits_and_site_characteristics %>%
  filter(dominant_ELC_site_code == "WOD",
         nest_guild_BOTW == "shrub")

# Step 2: Aggregate totals per site per year
territory_totals <- shrub_wod_df %>%
  group_by(sitecode,
           year,
           site_size_ha) %>%  # include site size for offset
  summarise(
    total_territories = sum(territories, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sitecode = as.factor(sitecode))  # random effect

# GAMM

gamm_totals <- gam(
  total_territories ~ s(year) +
    s(sitecode, bs = "re") +
    offset(log(site_size_ha)),
  data = territory_totals,
  family = nb(),   # negative binomial for overdispersed count data
  method = "REML"
)

summary(gamm_totals)
gam.check(gamm_totals)

# Add observed density (territories per hectare)
territory_totals <- territory_totals %>%
  mutate(observed_density = total_territories / site_size_ha)

# Create newdata with years and reference site
unique_years <- sort(unique(territory_totals$year))
median_site_size <- median(territory_totals$site_size_ha, na.rm = TRUE)

newdata <- data.frame(
  year = unique_years,
  sitecode = factor(NA, levels = levels(territory_totals$sitecode)),
  site_size_ha = median_site_size
)

# Predict (on link scale)
pred <- predict(gamm_totals, newdata = newdata, type = "link", se.fit = TRUE)

# Back-transform to density
newdata <- newdata %>%
  mutate(
    fit = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit)
  )

ggplot() +
  # Raw observed densities (jittered by year to show overlapping points)
  geom_point(data = territory_totals,
             aes(x = year, y = observed_density),
             color = "gray40", size = 2, alpha = 0.6, position = position_jitter(width = 0.2)) +
  
  # Smooth fitted GAM trend
  geom_line(data = newdata, aes(x = year, y = fit), color = "steelblue", size = 1.2) +
  
  # Confidence ribbon
  geom_ribbon(data = newdata, aes(x = year, ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.4) +
  
  labs(title = "GAM-Predicted Density of Shrub-Dependent Birds in WOD Sites",
       subtitle = "Smoothed trend with 95% CI and observed site-year densities",
       x = "Year",
       y = "Territories per Hectare (Density)") +
  theme_minimal(base_size = 14)
















#-----------------------------------------------------------------------------#


plot(gamm_totals, residuals = TRUE)

# Use median site size for standardized predictions
site_size_median <- median(territory_totals$site_size_ha, na.rm = TRUE)

# Create a new data frame to predict over years
newdata <- data.frame(
  year = seq(min(territory_totals$year), max(territory_totals$year)),
  sitecode = NA,  # exclude random effect
  site_size_ha = site_size_median
)

# Predict with SE to get confidence intervals
pred <- predict(gamm_totals, newdata = newdata, type = "link", se.fit = TRUE)

# Back-transform from log scale
newdata <- newdata %>%
  mutate(
    fit_link = pred$fit,
    se_link = pred$se.fit,
    fit_density = exp(fit_link),
    lower_ci = exp(fit_link - 1.96 * se_link),
    upper_ci = exp(fit_link + 1.96 * se_link)
  )


ggplot(newdata, aes(x = year,
                    y = fit_density)) +
  geom_ribbon(aes(ymin = lower_ci,
                  ymax = upper_ci),
              fill = "lightblue",
              alpha = 0.4) +
  geom_line(color = "steelblue",
            size = 1.2) +
  labs(title = "Predicted Density Trend of Shrub-Dependent Birds in WOD Sites",
       x = "Year",
       y = "Predicted Territories per Hectare (Density)") +
  theme_pubr()



#-----------------------------------------------------------------------------#
# OPTION to run GAMM with year as factor:
gamm_totals <- gam(
  total_territories ~ factor(year) +
    s(sitecode, bs = "re") +
    offset(log(site_size_ha)),
  data = territory_totals,
  family = nb(),   # negative binomial for overdispersed count data
  method = "REML"
)

summary(gamm_totals)


library(mgcv)
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# DRAFT: Model Framework
model <- gam(
  count ~ s(year, k=10) + s(site, bs="re"),
  data = bird_data,
  family = nb()  # or poisson()
)

library(purrr)

models <- bird_territories_with_traits %>%
  group_by(species_4code_IBP) %>%
  group_split() %>%
  set_names(unique(bird_territories_with_traits$species_4code_IBP)) %>%
  map(~ gam(
    territories ~ s(year, k = 10) + s(sitecode, bs = "re"),
    data = bird_territories_with_traits,
    family = nb(),
    method = "REML"
  ))

library(mgcv)
library(dplyr)
library(purrr)

models <- bird_territories_with_traits %>%
  group_by(species_4code_IBP) %>%
  group_split() %>%
  set_names(unique(bird_territories_with_traits$species_4code_IBP)) %>%
  map(~ gam(
    territories ~ s(year, k = 10) + s(sitecode, bs = "re"),
    data = .x,  # ✅ Use the subset here
    family = nb(),
    method = "REML"
  ))


# Example: summary of Ovenbird model
summary(models$Ovenbird)


bird_territories_with_traits %>%
  filter(species_4code_IBP == "TRES") %>%
  summary()

# Count usable rows per species (non-NA territories)
species_to_model <- bird_territories_with_traits %>%
  filter(!is.na(territories), !is.na(year), !is.na(sitecode)) %>%
  group_by(species_4code_IBP) %>%
  tally() %>%
  filter(n >= 5)  # set threshold — adjust as needed

filtered_data <- bird_territories_with_traits %>%
  semi_join(species_to_model, by = "species_4code_IBP")

library(mgcv)
library(dplyr)
library(purrr)

models <- filtered_data %>%
  group_by(species_4code_IBP) %>%
  group_split() %>%
  set_names(unique(filtered_data$species_4code_IBP)) %>%
  map(~ gam(
    territories ~ s(year, k = 10) + s(sitecode, bs = "re"),
    data = .x,
    family = nb(),
    method = "REML"
  ))














# DRAFT work through for GAMM:

bird_territories_with_traits <- bird_territories_with_traits %>%
  mutate(year = as.numeric(as.character(year)))

bird_territories_with_traits$year <- as.numeric(bird_territories_with_traits$year)

filtered_data$year <- as.numeric(filtered_data$year)

library(dplyr)

filtered_data <- bird_territories_with_traits %>%
  filter(!is.na(territories), !is.na(year), !is.na(sitecode)) %>%
  group_by(species_4code_IBP) %>%
  filter(n() >= 5) %>%   # Only keep species with ≥30 usable rows
  ungroup()

library(mgcv)
library(purrr)

safe_gam <- safely(function(df) {
  gam(
    territories ~ s(year, k = 5) + s(sitecode, bs = "re"),
    data = df,
    family = nb(),
    method = "REML"
  )
})

models <- filtered_data %>%
  group_by(species_4code_IBP) %>%
  group_split() %>%
  set_names(unique(filtered_data$species_4code_IBP)) %>%
  map(safe_gam)

# Extract only successful model results
successful_models <- map(models, "result") %>% compact()

# Extract species names that failed
failed_species <- map(models, "error") %>%
  keep(~ !is.null(.x)) %>%
  names()

# Check which species failed
failed_species

filtered_data %>% filter(species_4code_IBP == "TRES") %>% summary()





