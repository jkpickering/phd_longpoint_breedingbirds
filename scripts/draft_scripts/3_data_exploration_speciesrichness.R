#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Species Richness
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-01-30
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

# Explore the species richness for sites over time

# Create a bar chart plot to visualize the total species richness for each site
species.richness.site.plot <- bird.abund.data %>%
  group_by(sitecode) %>%
  summarise(richness = n_distinct(species_4code_IBP)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = sitecode, y = richness)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8, face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, color = "black", hjust = 1, angle = 45), # Formats x-axis text
        axis.text.y.left = element_text(size = 8, color = "black")) + # Formats y-axis text
  labs(title = "Species richness for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Site",
       y = "Number of species") +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_discrete(expand = c(0, 0.5))

# Visualize
species.richness.site.plot

# Save plot
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/species_richness_site_plot.png"), plot = species.richness.site.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#

# Create a smoothed trend line wrapped plot (by site) to visualize the species richness for each site over time using the 'facet_wrap' function
species.richness.site.year.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(richness = n_distinct(species_4code_IBP)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = year, y = richness, group = sitecode)) +
  geom_smooth(method = loess, size = 1, colour = "steelblue") +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0, color = "black") +  # Horizontal line
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"), # Provides adequate spacing between subplots within the wrapped plot
        strip.text.x = element_text(size = 8, face = "bold"), # Formats subtitles within the wrapped plot
        strip.background = element_blank(), # Formats background and frame of the subtitles within the wrapped plot
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8, color = "black")) + # Formats y-axis text
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +
  labs(title = "Species richness for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Site",
       y = "Number of species") +
  scale_y_continuous(limits = c(-10, NA), expand = c(0, 0), breaks = seq(0, 50, by = 10), minor_breaks = seq(0, 50, by = 5)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

# Visualize
print(species.richness.site.year.plot)

# Save plot
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/species_richness_site_year_plot.png"), plot = species.richness.site.year.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#









# Apply linear model to species richness data

# Import data from csv file, saved in the 'data' sub-folder of the rproject
bird.richness.data <- read.csv("../data/bird_sprichness.csv")

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird.richness.data) # Summarize key information within the dataset

# Run a linear model for each site (15 total)
site_models <- bird.richness.data %>%
  group_by(sitecode) %>%
  do(tidy(lm(richness ~ year, data = .)))

# View results
print(site_models, n = Inf)

#-----------------------------------------------------------------------------#

# Visualize trend of species richness over time for all sites (15 total) for 
# species richness (y), over year (x)

# Create plots to visualize the species richness for each plot over time using 
# the 'facet_wrap' function
bird.richness.allsitetrends <- ggplot(bird.richness.data, aes(x = year, y = richness, color = sitecode)) +
  geom_smooth(method = "lm") +  # Smoothed line plot
  geom_point(size = 1.2) + # Add points for better visibility
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
  labs(title = "Species richness for monitoring sites at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Species richness") +
  xlim(1991, 2021) # Set x-limit minimum and maximum

bird.richness.allsitetrends

ggsave("D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_richness_allsitetrends.png", 
       plot = bird.richness.allsitetrends, 
       width = 8, height = 6, dpi = 300)

#-----------------------------------------------------------------------------#

# Incomplete function for interactive plots

# Convert ggplot to interactive plot
interactive.bird.richness.allsitetrends <- ggplotly(bird.richness.allsitetrends)


interactive.bird.richness.allsitetrends


  layout(
    title = list(font = list(size = 12, color = "black")),
    xaxis = list(title = "Sampling Date", tickangle = 45),
    yaxis = list(title = "Richness"),
    hoverlabel = list(bgcolor = "lightyellow", font = list(size = 12))
  )
  
#-----------------------------------------------------------------------------#

# Visualize trend of species richness over time for each site (15 total) for 
# species richness (y), over year (x)

# Get unique sites
sites <- unique(bird.richness.data$sitecode)

# Loop through sites and create individual plots with standard error, and zero
# the y scale with 'ylim' function
for (site in sites) {
  bird.richness.plot  <- ggplot(filter(bird.richness.data, sitecode == site), aes(x = year, y = richness)) +
    geom_smooth(method = "lm", colour = "black") +  # Smoothed line plot
    geom_point(size = 1.2, colour = "black") + # Add points for better visibility
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
    labs(title = paste("Species Richness for", site, "at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Species richness") +
    xlim(1991, 2021) + # Set x-limit minimum and maximum
    ylim(0, 40)  # Set minimum y-limit to 0
  
print(bird.richness.plot)  # Display plot in R
}

# Need to find code to export all plots to a PNG file type

#-----------------------------------------------------------------------------#

# Exploring the use of a Generalized Linear Mixed Model (GLMM) in R using 
# the 'glmer' function

# Poisson model with sites nested within regions
model1 <- glmer(richness ~ year + (1 | sitecode), 
                data = bird.richness.data, 
                family = poisson)

summary(model1)

# Predicted values (on the scale of the link function)
bird.richness.data$predicted <- predict(model1, type = "response")

# Extract random effects (sitecode intercepts)
random_effects <- ranef(model1)$sitecode


# Plot of predicted values vs. fixed effect (x)
plot.test <- ggplot(bird.richness.data, aes(x = year, y = predicted)) +
  geom_point(aes(color = sitecode)) +  # Color by sitecode for random effect visualization
  labs(title = "Predicted Values from GLMM", x = "Fixed Effect (x)", y = "Predicted Response")

plot.test
