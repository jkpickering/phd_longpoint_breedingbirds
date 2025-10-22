#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - Species Diversity
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

# Explore the species diversity for sites over time

# Create a site-species matrix (presence-absence)
species.abundance.matrix <- bird.abund.data %>%
  group_by(year, sitecode, species_4code_IBP) %>%
  summarise(total_territories = sum(territories), .groups = "drop") %>%
  spread(species_4code_IBP, total_territories, fill = 0)

# Compute the Shannon index per site and time
species.abundance.matrix$diversity <- diversity(species.abundance.matrix[,3:ncol(species.abundance.matrix)], index = "shannon")

# Identify the average specives diversity (Shannon Index) for each site
mean.diversity.data <- species.abundance.matrix %>%
  group_by(sitecode) %>%
  summarise(mean.diversity = mean(diversity), .groups = "drop")

ggplot(mean.diversity.data, aes(x = sitecode, y = mean.diversity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8, color = "black")) + # Formats y-axis text
  labs(title = "Species diversity for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Site",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(breaks = seq(0, 4, by = .25), limits = c(0, 3.25), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_discrete(expand = c(0, 0.5))

## YEAR

# Identify the average specives diversity (Shannon Index) for each year
mean.diversity.data.year <- species.abundance.matrix %>%
  group_by(year) %>%
  summarise(mean.diversity = mean(diversity), .groups = "drop")

ggplot(mean.diversity.data.year, aes(x = year, y = mean.diversity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8, color = "black")) + # Formats y-axis text
  labs(title = "Species diversity for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(breaks = seq(0, 4, by = .25), limits = c(0, 3.25), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(mean.diversity.data.year$year), max(mean.diversity.data.year$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

print(species.abundance.matrix, n = Inf)

hist(species.abundance.matrix$diversity)

shapiro.test(species.abundance.matrix$diversity)

ggplot(species.abundance.matrix, aes(x = year, y = diversity, group = sitecode)) +
  geom_smooth(size = 1, colour = "steelblue") +
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
  labs(title = "Species diversity for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#




















#-----------------------------------------------------------------------------#

# Import csv file of complete breeding bird census territory data with all zeros

# Import data from csv file, saved in the 'data' sub-folder of the rproject
bird.spdiversity.data <- read.csv("../data/bird_spdiversity.csv")

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird.spdiversity.data) # Summarize key information within the dataset


bird.spdiversity.data[is.na(bird.spdiversity.data)] <- 0

str(bird.spdiversity.data)

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird.spdiversity.data) # Summarize key information within the dataset


diversity.data <- bird.spdiversity.data %>%
  select(-sample, -sitecode, -year)

# Calculate the Shannon Index for each sample
shannon.index <- diversity(diversity.data, index = "shannon")

shannon.index

# Add Shannon Index to the original dataset for further analysis
shannon.data <- cbind(bird.spdiversity.data[, c("sample", "year", "sitecode")], shannon_index = shannon.index)

# View the results
shannon.data


# ----------------------------------------------------------------------------#

# Test assumptions of the data

# Histogram
hist(shannon.data$shannon_index)# Histogram

# Shapiro-Wilk test for normality
shapiro.test(shannon.data$shannon_index)  # Shapiro-Wilk test for normality

# Data is not normal, so cannot run simple linear regression model

# Use a GAM - CODE DOES NOT WORK

str(shannon.data)  # Check column names and data types
summary(shannon.data)  # Look for missing or strange values


gam_model <- gam(shannon_index ~ s(year) + s(sitecode), 
                 data = shannon.data, 
                 family = gaussian())

summary(gam_model)



install.packages("mgcv")
library(mgcv)

# Fit GAM: Allows smoothing of year
model_gam <- gam(shannon_index ~ s(year) + s(sitecode), data = shannon.data, family = gaussian())

summary(model_gam)

# Try GAMMA GLMM, for skewed data

install.packages("glmmTMB")
library(glmmTMB)

# Fit Gamma GLMM
model_gamma <- glmmTMB(shannon_index ~ year + (1 | sitecode), 
                       data = shannon.data, 
                       family = Gamma(link = "log"))

summary(model_gamma)


# Try GLMM (Poisson), for count data (Index)

install.packages("glmmTMB")
library(glmmTMB)

# Fit a Poisson GLMM
model <- glmmTMB(territories ~ year + (1 | sitecode), 
                 data = your_data, 
                 family = poisson)

summary(model)


# ----------------------------------------------------------------------------#

# Apply linear model to species diversity data

# Run a linear model for each site (15 total)
site_models <- shannon.data %>%
  group_by(sitecode) %>%
  do(tidy(lm(shannon_index ~ year, data = .)))

# View results
print(site_models, n = Inf)


#-----------------------------------------------------------------------------#

# Visualize trend of species richness over time for all sites (15 total) for 
# species richness (y), over year (x)

# Create plots to visualize the species richness for each plot over time using 
# the 'facet_wrap' function
bird.diversity.plot <- ggplot(shannon.data, aes(x = year, y = shannon.index, color = sitecode)) +
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
  labs(title = "Species diversity for monitoring sites at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Species diversity (Shannon index)") +
  xlim(1991, 2021) # Set x-limit minimum and maximum

bird.diversity.plot

ggsave("D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_diversity.png", 
       plot = bird.diversity.plot, 
       width = 8, height = 6, dpi = 300)

#-----------------------------------------------------------------------------#

# Visualize and explore plots with 'ggplotly' function

# Convert ggplot to interactive plots
bird.diversity.plotly <- ggplotly(bird.diversity.plot) %>%
  layout(
    yaxis = list(
      tickfont = list(size = 10, color = "black")  # Tick label settings
    ),
    yaxis2 = list(
      tickfont = list(size = 10, color = "black")
    ),
    yaxis3 = list(
      tickfont = list(size = 10, color = "black")
    ),
    yaxis4 = list(
      tickfont = list(size = 10, color = "black")
    ),
    yaxis5 = list(
      tickfont = list(size = 10, color = "black")
    ),
    xaxis = list(
      tickfont = list(size = 10, color = "black")  # Tick label settings
    ),
    xaxis2 = list(
      tickfont = list(size = 10, color = "black")
    ),
    xaxis3 = list(
      tickfont = list(size = 10, color = "black")
    )
  )

# View interactive plots
bird.diversity.plotly

# Save plot
saveWidget(bird.diversity.plotly, file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/interactive_diversity_plot.html", selfcontained = TRUE)

#-----------------------------------------------------------------------------#

# Incomplete summary information code

# Calculate mean Shannon Index by site
mean_shannon_by_site <- shannon.data %>%
  group_by(sitecode) %>%
  summarise(mean_shannon = mean(shannon_index))

# Calculate mean Shannon Index by year
mean_shannon_by_year <- shannon.data %>%
  group_by(year) %>%
  summarise(mean_shannon = mean(shannon_index))

# View the results
print(mean_shannon_by_site)
print(mean_shannon_by_year)

# Plot Mean Shannon Index by Site
ggplot(mean_shannon_by_site, aes(x = sitecode, y = mean_shannon)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Shannon Index by Site", x = "Site", y = "Mean Shannon Index")


#-----------------------------------------------------------------------------#

# INCOMPLETE CODE for linear model:

# Run a linear model for each site (15 total)
site.models <- shannon.data %>%
  group_by(sitecode) %>%
  do(tidy(lm(shannon.index ~ year, data = .)))


# View results
print(site.models, n = Inf)

#-----------------------------------------------------------------------------#

# Visualize trend of species richness over time for all sites (15 total) for 
# species richness (y), over year (x)

# Create plots to visualize the species richness for each plot over time using 
# the 'facet_wrap' function
bird.territories.allsitetrends <- ggplot(bird.territory.data, aes(x = year, y = territories, color = sitecode)) +
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
  labs(title = "Breeding bird territories for monitoring sites at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  xlim(1991, 2021) # Set x-limit minimum and maximum

bird.territories.allsitetrends

#-----------------------------------------------------------------------------#

# Need to start simple, add complexity. This begins for my research with species richness over time. 
# Next complexity is to model species diversity across sites over time.

#Have to learn to format dataset for species richness (through R, not excel)
richness.data <- paste(territory.data$year,territory.data$sitecode, sep="-")

richness.data



# Import csv file with supporting bird species descriptions
speciesdescription.data <- read.csv("bird_spcharacteristics.csv", header = TRUE)


# Review and identify some summary information about the data
summary(speciesdescription.data) # Summarize key information within the dataset
names(speciesdescription.data) # Identify column names within the dataset


# Join species information data "speciesdescription.data" with territory data for analyses
territoryguild.data <- full_join(territory.data, speciesdescription.data, by = c("species_4code_IBP"))





ps<-table(plrich$Species,plrich$Site)

bird.territories.richness <- table(territory.data$year,territory.data$species_4code_IBP)

bird.territories.richness

# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (15) FOR ALL SPECIES ABUNDANCES ACROSS ALL  SITES, GROUPED ON ONE CHART (y = ABUNDANCE, x = YEAR)
# Visualize all territory data across sites, wrapped for multiple plots on one chart with facet_wrap function.


# Plot breeding bird territory data over time by site on line graphs with ggplot function
spabund.plot <- ggplot(data = territory.data,
                       aes(x = year,
                           y = territories, color = sitecode)) +
  geom_point() +  # Add a line plot
  facet_wrap(~ sitecode, scales = "free_y") +  # Facet by site
  labs(x = "Year", y = "Abundance") +  # Label axes and legend
  theme_minimal()  # Optional: choose a theme


# Run plot to visualize breeding bird territory data, grouped by site
spabund.plot


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (15) FOR A SINGLE SPECIES [SONG SPARROW (SOSP)] ACROSS ALL SITES, GROUPED ON ONE CHART (y = ABUNDANCE, x = YEAR)
# Visualize individual species data over time, grouped by site


# Filter breeding bird data by species interested in (or guild for complexity), example here is Song Sparrow (SOSP)
SOSP.sample <- territory.data %>%
  filter(speciescode %in% c("SOSP"))  # Choose SOSP (Song Sparrow) as specific species


# Plot breeding bird territory data over time by site on line graphs
SOSPabund.plot <- ggplot(data = SOSP.sample, aes(x = year,
                                                 y = territories, color = sitecode)) +
  geom_point(aes(group = interaction(sitecode, speciescode))) +  # Line trends per site-species
  geom_smooth() +
  scale_x_continuous(limits = c(1990, 2022)) +  # Set x-axis range
  facet_wrap(~ sitecode, scales = "free_y") +  # Create panels for each site, you can add "scales = "free_y" code to remove standard y-axis limits within plots
  labs(
    x = "Year",
    y = "Abundance",
    color = "Site",
    title = "Song Sparrow (SOSP) Trends Over Time and Across Sites"
  ) +
  theme_pubclean()


# Run plot to visualize breeding bird territory data of individual species, grouped by site
SOSPabund.plot


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (51) FOR A SINGLE SITE [RARO] ACROSS ALL YEARS, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize individual species data for individual sites over time, plotted separately by species


# Filter breeding bird data by site interested in for viewing species, example here is RARO
RARO.data <- territory.data %>%
  filter(sitecode %in% c("RARO"))  # Choose RARO as specific site


# Provide site data as function for split
RARO.species.data <- split(RARO.data, RARO.data$speciescode)


# Check max limit for Y-axis for different sites, can adjust according to site for the "scale_y_continuous" function
max(RARO.data$territories)

# Plot species territory abundance for each site over time
RARO.species.plot <- lapply(names(RARO.species.data), function(species) {
  ggplot(RARO.species.data[[species]], aes(x = year, 
                                           y = territories, color = speciescode)) +
    geom_smooth(aes(group = speciescode)) +
    geom_point(aes(group = speciescode)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for RARO:", species),
      x = "Year",
      y = "Territory Count",
      color = "Species:"
    ) +
    theme_pubclean()
})


# Run Plot to visualize 15 unique plots with all species identified for that site over time
RARO.species.plot


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (100) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide site data as function for split
species.data <- split(bird.territorydata, bird.territorydata$speciescode)


# Plot species territory abundance for each site over time
species.summary.plot <- lapply(names(species.data), function(site) {
  ggplot(species.data[[site]], aes(x = year, y = territories, color = speciescode)) +
    geom_smooth(aes(group = speciescode))+
    geom_point(aes(group = speciescode)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 100 unique plots with all species identified for all sites over time
species.summary.plot


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (15) FOR ALL SITES WITH SPECIES BY YEAR, SEPARATED FOR SITES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide site data as function for split
site.data <- split(bird.territorydata, bird.territorydata$sitecode)


# Plot species territory abundance for each site over time
site.summary.plot <- lapply(names(site.data), function(site) {
  ggplot(site.data[[site]], aes(x = year, y = territories, color = speciescode)) +
    geom_smooth(aes(group = speciescode))+
    geom_point(aes(group = speciescode)) +
    labs(
      title = paste("Trends for Site:", site),
      x = "Year",
      y = "Abundance",
      color = "Species"
    ) +
    theme_minimal()
})


# Run Plot to visualize 15 unique plots with all species identified for that site over time
site.summary.plot


# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#


# Data Exploration: Data setup for Species Guild Interests


# -----------------------------------------------------------------------------#

# Import csv file of complete breeding bird census territory data with all zeros
birdabund.data <- read.csv("bird_spterritories_abund.csv", header = TRUE)


# Review and identify some summary information about the data
summary(birdabund.data) # Summarize key information within the dataset
names(birdabund.data) # Identify column names within the dataset


# Import csv file with supporting bird species descriptions
speciesdescription.data <- read.csv("bird_spcharacteristics.csv", header = TRUE)


# Review and identify some summary information about the data
summary(speciesdescription.data) # Summarize key information within the dataset
names(speciesdescription.data) # Identify column names within the dataset


# Join species information data "speciesdescription.data" with territory data for analyses
detailedbirdabund.data <- full_join(birdabund.data, speciesdescription.data, by = c("species_4code_IBP"))



# SUMMARY PLOTS (7) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
biome.data <- split(detailedbirdabund.data, detailedbirdabund.data$Breeding.Biome)


# Plot species territory abundance for each site over time
biome.plot <- lapply(names(biome.data), function(site) {
  ggplot(biome.data[[site]], aes(x = year, y = territories, color = Breeding.Biome)) +
    geom_smooth(aes(group = Breeding.Biome))+
    geom_point(aes(group = Breeding.Biome)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 7 unique plots with all species identified for all sites over time
biome.plot


# Boreal Forest
# Eastern Forest
# Forest Generalist
# Grassland
# Habitat Generalist
# Introduced
# Wetland

# -----------------------------------------------------------------------------#




# SUMMARY PLOTS (35) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
family.data <- split(detailedbirdabund.data, detailedbirdabund.data$Family)


# Plot species territory abundance for each site over time
family.plot <- lapply(names(family.data), function(site) {
  ggplot(family.data[[site]], aes(x = year, y = territories, color = Family)) +
    geom_smooth(aes(group = Family))+
    geom_point(aes(group = Family)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 35 unique plots with all species identified for all sites over time
family.plot

# Accipitridae
# Alcedinidae
# Anatidae
# Apodidae
# Ardeidae
# Bombycillidae
# Caprimulgidae
# Cardinalidae
# Certhiidae
# Charadriidae
# Columbidae
# Corvidae
# Cuculidae
# Falconidae
# Fringillidae
# Hirundinidae
# Icteridae
# Mimidae
# Paridae
# Parulidae
# Passerellidae
# Phasianidae
# Picidae
# Podicipedidae
# Polioptilidae
# Rallidae
# Scolopacidae
# Sittidae
# Strigidae
# Sturnidae
# Trochilidae
# Troglodytidae
# Turdidae
# Tyrannidae
# Vireonidae



# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (5) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
group.data <- split(detailedbirdabund.data, detailedbirdabund.data$bird.group)


# Plot species territory abundance for each site over time
group.plot <- lapply(names(group.data), function(site) {
  ggplot(group.data[[site]], aes(x = year, y = territories, color = bird.group)) +
    geom_smooth(aes(group = bird.group))+
    geom_point(aes(group = bird.group)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 5 unique plots with all species identified for all sites over time
group.plot

# landbird
# other
# shorebird
# waterbird
# waterfowl


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (3) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
migratory.data <- split(detailedbirdabund.data, detailedbirdabund.data$Migrate)


# Plot species territory abundance for each site over time
migrate.plot <- lapply(names(migratory.data), function(site) {
  ggplot(migratory.data[[site]], aes(x = year, y = territories, color = Migrate)) +
    geom_smooth(aes(group = Migrate))+
    geom_point(aes(group = Migrate)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 3 unique plots with all species identified for all sites over time
migrate.plot

# Introduce
# M, Migratory species
# R, Resident species

# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (9) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
BOTW.habguild.data <- split(detailedbirdabund.data, detailedbirdabund.data$habitat_guild_BOTW)


# Plot species territory abundance for each site over time
BOTW.habguild.plot <- lapply(names(BOTW.habguild.data), function(site) {
  ggplot(BOTW.habguild.data[[site]], aes(x = year, y = territories, color = habitat_guild_BOTW)) +
    geom_smooth(aes(group = habitat_guild_BOTW))+
    geom_point(aes(group = habitat_guild_BOTW)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 9 unique plots with all species identified for all sites over time
BOTW.habguild.plot

# forests
# grasslands
# lakes and ponds
# marshes
# open woodlands
# rivers and streams
# scrub
# shorelines
# towns


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (11) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
BOTW.fooditem.data <- split(detailedbirdabund.data, detailedbirdabund.data$food_item_BOTW)


# Plot species territory abundance for each site over time
BOTW.fooditem.plot <- lapply(names(BOTW.fooditem.data), function(site) {
  ggplot(BOTW.fooditem.data[[site]], aes(x = year, y = territories, color = food_item_BOTW)) +
    geom_smooth(aes(group = food_item_BOTW))+
    geom_point(aes(group = food_item_BOTW)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 11 unique plots with all species identified for all sites over time
BOTW.fooditem.plot


# aquatic invertebrates
# birds
# fish
# fruit
# insects
# mammals
# nectar
# omnivore
# plants
# seeds
# small animals


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (7) FOR ALL SPECIES ACROSS ALL SITES BY YEAR, SEPARATED FOR SPECIES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide species data as function for split
BOTW.nestguild.data <- split(detailedbirdabund.data, detailedbirdabund.data$nest_guild_BOTW)


# Plot species territory abundance for each site over time
BOTW.nestguild.plot <- lapply(names(BOTW.nestguild.data), function(site) {
  ggplot(BOTW.nestguild.data[[site]], aes(x = year, y = territories, color = nest_guild_BOTW)) +
    geom_smooth(aes(group = nest_guild_BOTW))+
    geom_point(aes(group = nest_guild_BOTW)) +
    scale_x_continuous(limits = c(1991, 2021)) +  # Set x-axis range
    labs(
      title = paste("Trends for Species:", site),
      x = "Year",
      y = "Value",
      color = "Species"
    ) +
    theme_minimal()
})


# Run plot to visualize 7 unique plots with all species identified for all sites over time
BOTW.nestguild.plot

# building
# burrow
# cavity
# floating
# ground
# shrub
# tree


# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#


# Data Exploration: Data setup for Species Richness


# -----------------------------------------------------------------------------#


# Import csv file of basic breeding bird census territory data
bird.richnessdata <- read.csv("LPBBC_richnessdata.csv", header = T)


# Identify summary information from the data
summary(bird.richnessdata)


# Identify and review the column names for variables within the data
names(bird.richnessdata)


# -----------------------------------------------------------------------------#


# SUMMARY PLOTS (15) FOR ALL SITES WITH SPECIES BY YEAR, SEPARATED FOR SITES ON MANY CHARTS (y = ABUNDANCE, x = YEAR)
# Visualize all species data over time, plotted separately by site

# Provide site data as function for split
site.richness <- split(bird.richnessdata, bird.richnessdata$sitecode)


# Plot species territory abundance for each site over time
site.richness.plot <- lapply(names(site.richness), function(site) {
  ggplot(site.richness[[site]], aes(x = year, y = richness)) +
    geom_smooth()+
    geom_point() +
    labs(
      title = paste("Trends for Site:", site),
      x = "Year",
      y = "Abundance",
      color = "Species"
    ) +
    theme_minimal()
})


# Run Plot to visualize 15 unique plots with all species identified for that site over time
site.richness.plot
