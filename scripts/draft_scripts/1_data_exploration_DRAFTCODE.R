#-----------------------------------------------------------------------------#
# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Data Exploration - DRAFT CODE
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2025-01-30
# Last Updated:   2025-02-13
# Description:    This script provides data import files in preparation for
#                 analyses for breeding bird census data collected at Long
#                 Point, Ontario, Canada from 1991 - 2021
# Data Sources:   /data/raw/"FILE NAME.csv"
# Outputs:        /results/"FILE NAME.csv"
# R Version:      4.4.2 (2024-10-31 ucrt)
# Dependencies:   "ggplot2", "dplyr", "tidyr", "broom", "plotly", "ggpubr", "lme4"
# NOTES:          Run statistical models and visualize species richness data
#-----------------------------------------------------------------------------#

# Clear all objects from the R Environment prior to data exploration
rm(list = ls())  # Remove all objects

#-----------------------------------------------------------------------------#


# Prepare statistical model for non-normal data, evaluate for use of GLMMs

# Employ a Generalized Linear Mixed Model (GLMM)
bird.abund.glmm <- glmer(territories ~ year + sitecode + (1 | sitecode/year), family = poisson, data = bird.abund.data)

print(bird.abund.glmm)

# Extract results...
summary(bird.abund.glmm)      # Fixed & random effects estimates
ranef(bird.abund.glmm)        # Extract random effects
fixef(bird.abund.glmm)        # Extract fixed effects
coef(bird.abund.glmm)         # Combined fixed & random effects
VarCorr(bird.abund.glmm)      # Variance of random effects

# Results of fixed effects suggests:
# 1. an overall increase of 0.1 % annually over time, (0.1 x 31 years = 3% change)
# 2. there was a significant change (p-value < 0.05) for three of the 15 sites; IDSS, ROWB, and WPWC,  over time

#-----------------------------------------------------------------------------#

# Prepare statistical model for non-normal data, with example data and code for showing process
set.seed(123)

bird_data <- data.frame(
  Year = rep(2000:2020, times = 30),  # 21 years, repeated for 30 species-site combinations
  Site = rep(letters[1:10], each = 630),  # 10 sites, each with 630 observations
  Species = rep(paste0("Species_", 1:3), each = 210, times = 10),  # 3 species per site
  Abundance = rpois(6300, lambda = 10 + sin((2000:2020 - 2000)/3) * 5 + rnorm(6300, 0, 2))  # Counts with a fluctuating trend
)

bird_data$Species <- as.factor(bird_data$Species)
bird_data$Site <- as.factor(bird_data$Site)


gam_model <- gam(Abundance ~ s(Year, bs = "cs") + 
                   s(Species, bs = "re") + 
                   s(Site, bs = "re"), 
                 family = poisson, data = bird_data, method = "REML")

summary(gam_model)

# Ensure Species and Site are factors
bird_data$Species <- as.factor(bird_data$Species)
bird_data$Site <- as.factor(bird_data$Site)

# Create a new data frame for predictions
new_data <- expand.grid(
  Year = seq(min(bird_data$Year), max(bird_data$Year), length.out = 100),
  Species = unique(bird_data$Species),
  Site = unique(bird_data$Site)
)

# Predict abundance while accounting for random effects
preds <- predict(gam_model, newdata = new_data, type = "response", se.fit = TRUE)

# Store predictions in the data frame
new_data$Pred <- preds$fit
new_data$Lower_CI <- preds$fit - 1.96 * preds$se.fit
new_data$Upper_CI <- preds$fit + 1.96 * preds$se.fit

# Visualize results: Predicted abundance over years by Species
ggplot(new_data, aes(x = Year, y = Pred, group = Species, color = Species)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Species), alpha = 0.2) +
  labs(title = "Predicted Bird Abundance Over Years (GAM Model)", 
       x = "Year", y = "Predicted Abundance") +
  theme_minimal()


#-----------------------------------------------------------------------------#


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
