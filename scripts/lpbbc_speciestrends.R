#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Long Point Breeding Bird Census (BBC) Project
# Sub-title:      Data chapter 1 - species and guild trends

# Author:         Joshua Pickering, PhD Candidate
# Affiliation:    University of Waterloo
# Creation Date:  2025-05-06
# Last Updated:   2025-10-09

# Description:    This script includes species trends for breeding bird census 
#                 territory data collected at Long Point, Ontario, Canada from
#                 1991 - 2021

# Data Sources:   "../data/bird_species_territories_v1.csv"
#                 "../data/bird_species_traits_v2.csv"
#                 "../data/site_env_characteristics_v1.csv"

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
                      "gamm4",
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
} # creates new function for new package installs

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

# 3.0 Breeding bird species plots: data visualizations

#-----------------------------------------------------------------------------#
# 3.1 Organize species dataset in preparation for GAMM

# Identify species present in >4 unique years
species_years <- aggregate(year ~ species_4code_IBP, 
                           bird_territories_with_traits, 
                           function(x) length(unique(x)))

# Organize list into kept species
species_to_keep <- species_years$species_4code_IBP[species_years$year > 4]

# Filter data for those species
data_filtered <- subset(bird_territories_with_traits,
                        species_4code_IBP %in% species_to_keep)

# Develop list and dataframe items for models and predictions respectively
species_models <- list() # create empty list
species_predictions <- data.frame() # create empty dataframe
species_list <- unique(data_filtered$species_4code_IBP) # create species list

#-----------------------------------------------------------------------------#
# 3.2 Model with loop through for species to fit GAMM and generate predictions

# loop species to fit GAMM
for (sp in species_list) {
  sp_data <- subset(data_filtered, 
                    species_4code_IBP == sp) # subset for individual species
  
  fit <- tryCatch({
    gamm(territories ~ s(year, k = 10),
         random = list(sitecode = ~1),
         data = sp_data)
  }, error = function(e) {
    message(paste("Error fitting species:",
                  sp,
                  " - skipping."))
    return(NULL)
  }) # Catch errors to avoid stopping loop in fitting GAMM
  
  if (!is.null(fit)) {
    species_models[[sp]] <- fit # if statement for successful model fits
    
    new_years <- seq(min(sp_data$year),
                     max(sp_data$year),
                     length.out = 100) # create predictions for species
    
    newdata <- data.frame(year = new_years,
                          sitecode = NA)  # smoother for sitecode
    
    pred <- predict(fit$gam,
                    newdata = newdata,
                    type = "response",
                    se.fit = TRUE) # predict fitted values and standard errors
    
    sp_pred <- data.frame(
      species_4code_IBP = sp,
      year = new_years,
      fit = pred$fit,
      se = pred$se.fit
    ) # organize predictions into a data frame
    
    species_predictions <- rbind(species_predictions,
                         sp_pred) # group all species predictions
  }
}

#-----------------------------------------------------------------------------#
# 3.3 Plot all species smooth trends with confidence ribbons

# rename species for plotting
species_predictions <- full_join(species_predictions,
                                 bird_territories_with_traits,
                                 by = c("species_4code_IBP"))

# create unique species list
species_list <- unique(species_predictions$sp_common_name_uppercase_IBP)

# loop species to plot GAMM results
for (sp in species_list) {
  sp_pred <- subset(species_predictions,
                    sp_common_name_uppercase_IBP == sp)
  
  p <- ggplot(sp_pred,
              aes(x = year,
                  y = fit)) +
    geom_line(color = "black",
              size = 1) +
    geom_hline(yintercept = 0, 
               color = "black") +  # Horizontal line
    geom_vline(xintercept = 1990,
               color = "black") +
    geom_ribbon(aes(ymin = fit - 1.96*se, 
                    ymax = fit + 1.96*se), 
                alpha = 0.2,
                fill = "lightblue") +
    labs(title = paste(sp),
         x = "Year",
         y = "Predicted number of breeding bird territories, 1991-2021") +
    theme(legend.position = "none", # Removes the legend
          panel.background = element_rect(fill = "white",
                                          color = NA),  # white plot area
          plot.background = element_rect(fill = "white",
                                         color = NA),   # white outer background
          plot.title = element_text(size = 12,
                                    face = "bold",
                                    hjust = 0.95,
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
          axis.text.x = element_text(size = 10,
                                     color = "black"), # Formats x-axis text
          axis.text.y.left = element_text(size = 10,
                                          color = "black")) + # Formats y-axis text
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(limits = c(1990, 2022),
                       expand = c(0, 0))
    theme_pubr()
  
  print(p)  # to display in console
  
  # Optionally save each plot as a PNG
  # ggsave(filename = paste0("GAMM_trend_", sp, ".png"), plot = p, width = 6, height = 4)
  ggsave(filename = paste0("../results/plots/GAMM_trend_", sp, ".png"),
         plot = p,
         width = 9.69,
         height = 7.30)
}

#-----------------------------------------------------------------------------#
# 3.4 Check model results (k-index, p-value, and ehf values)

# loop species to conduct model checks
for (sp in names(species_models)) {
  message(paste("Running gam.check for species:", sp))
  gam.check(species_models[[sp]]$gam)
}

# How to interpret:
# Good fit: k-index ≈ 1 and p-value > 0.05 → no evidence that k is too small.
# Potential underfitting: k-index < 1 and p-value < 0.05 → try increasing k.
# The smoother might be too constrained to capture the shape of the data.

#-----------------------------------------------------------------------------#
# 3.5 Calculate the overall and annual percent change for each species

# Calculate the overall change (%) for the study period for each species
overall_change <- (tail(pred$fit,
                        1) - head(pred$fit,
                                  1)) / head(pred$fit,
                                             1) * 100

# Calculate the annual rate of change (%) for the study period for each species
trend_results <- species_predictions %>%
  group_by(species_4code_IBP) %>%
  arrange(year, .by_group = TRUE) %>% 
  summarise(
    overall_percent_change = (last(fit) / first(fit) - 1) * 100,
    annual_percent_rate = {
      lm_fit <- lm(log(fit) ~ year)
      rate <- coef(lm_fit)[2]
      (exp(rate) - 1) * 100})

# Organize structure of results for increasing, decreasing, and stable trends
trend_results <- trend_results %>%
  mutate(
    trend_category = case_when(
      annual_percent_rate > 1 ~ "Increasing",
      annual_percent_rate < -1 ~ "Decreasing",
      TRUE ~ "Stable"))

# Reorder species by annual trend
trend_results <- trend_results %>%
  mutate(species_4code_IBP = fct_reorder(species_4code_IBP, annual_percent_rate))

#-----------------------------------------------------------------------------#
# 3.5.1 Plot the overall change and annual percent change for each species

# Plot a bar graph for the rates of change for each species
rateofchange_plot <- ggplot(trend_results, # plot rate of change trends
                            aes(x = species_4code_IBP, # specify x-axis values
                                y = annual_percent_rate, # species y-axis
                                fill = trend_category)) + # specify fill values
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Better readability for species names
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Species (4-letter code)",
    y = "Annual rate of change (%)") +
  theme_pubr()

# Visualize bar chart
print(rateofchange_plot)

#-----------------------------------------------------------------------------#
# 3.5.2 Plot the overall change and annual percent change for each species (V2)

# Plot a bar graph for the rates of change for each species
rateofchange_plot <- ggplot(trend_results, # plot rate of change trends
                            aes(x = species_4code_IBP, # specify x-axis values
                                y = annual_percent_rate, # species y-axis
                                fill = trend_category)) + # specify fill values
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Species (4-letter code)",
       y = "Annual percentage change, 1991-2021") +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  theme(legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white",
                                        color = NA),  # white plot area
        plot.background = element_rect(fill = "white",
                                       color = NA),   # white outer background
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.95,
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
        axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 10,
                                        color = "black")) + # Formats y-axis text
  scale_y_continuous(limits = c(-7, 12),
                     expand = c(0, 0))  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales

# Visualize bar chart
print(rateofchange_plot)

#-----------------------------------------------------------------------------#
# 3.5.3 Plot the overall change and annual percent change for each species (V3)

# Plot a scatterplot for the rates of change for each species
rateofchange_plotv2 <- ggplot(trend_results,
                              aes(x = annual_percent_rate,
                                  y = fct_reorder(species_4code_IBP,
                                                  annual_percent_rate),
                                  color = trend_category)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray50") +
  scale_color_manual(values = c("Increasing" = "forestgreen",
                                "Stable" = "gray50",
                                "Decreasing" = "firebrick")) +
  labs(x = "Annual rate of change (%)",
    y = "Species (4-letter code)") +
  theme_pubr()

print(rateofchange_plotv2)

#-----------------------------------------------------------------------------#
# 3.6 Plot the overall percent change for each species

# Organize structure of results for increasing, decreasing, and stable trends
trend_results <- trend_results %>%
  mutate(
    trend_category = case_when(
      overall_percent_change > 5 ~ "Increasing",
      overall_percent_change < -5 ~ "Decreasing",
      TRUE ~ "Stable"))

# Plot a bar graph for the rates of change for each species
overallchange_plot <- ggplot(trend_results, # plot rate of change trends
                            aes(x = species_4code_IBP, # specify x-axis values
                                y = overall_percent_change, # species y-axis
                                fill = trend_category)) + # specify fill values
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Better readability for species names
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Species (4-letter code)",
       y = "Overall rate of change (%)") +
  theme_pubr()

print(overallchange_plot)

# Faceted plot for both metrics
trend_results_long <- trend_results %>%
  select(species_4code_IBP, annual_percent_rate, overall_percent_change, trend_category) %>%
  pivot_longer(cols = c("annual_percent_rate", "overall_percent_change"),
               names_to = "metric", values_to = "percent_change")

ggplot(trend_results_long, aes(x = percent_change, y = fct_reorder(species_4code_IBP, percent_change), color = trend_category)) +
  geom_point(size = 3) +
  facet_wrap(~ metric, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Percent Change",
    y = "Species",
    title = "Annual vs Overall Percent Change by Species"
  ) +
  theme_minimal(base_size = 14)

#-----------------------------------------------------------------------------#
# 3.7 Plot the overall change for each species over time (abundance)

# total_change = predicted abundance in final year - predicted abundance in first year

# Total change = final year prediction - first year prediction
species_abundance_change <- species_predictions %>%
  group_by(species_4code_IBP) %>%
  arrange(year) %>%
  summarise(
    abundance_start = first(fit),
    abundance_end = last(fit),
    total_change = abundance_end - abundance_start)

# Organize structure of results for increasing, decreasing, and stable trends
species_abundance_change <- species_abundance_change %>%
  mutate(
    trend_category = case_when(
      total_change > 1 ~ "Increasing",
      total_change < -1 ~ "Decreasing",
      TRUE ~ "Stable"))

# Reorder species by annual trend
species_abundance_change <- species_abundance_change %>%
  mutate(species_4code_IBP = fct_reorder(species_4code_IBP, total_change))

# Plot a bar graph for the rates of change for each species
rateofchange_plot <- ggplot(species_abundance_change, # plot rate of change trends
                            aes(x = species_4code_IBP, # specify x-axis values
                                y = total_change)) + # specify fill values
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Species (4-letter code)",
       y = "Total abundance (no. of territories) change, 1991-2021") +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  theme(legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white",
                                        color = NA),  # white plot area
        plot.background = element_rect(fill = "white",
                                       color = NA),   # white outer background
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.95,
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
        axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 10,
                                        color = "black")) + # Formats y-axis text
  scale_y_continuous(limits = c(-8, 21),
                     expand = c(0, 0))  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales

# Visualize bar chart
print(rateofchange_plot)

#-----------------------------------------------------------------------------#
# 4.0 Plot the overall percent change for each guild from a calculation of a 
# summarization of total species changes

# Guild breakdown:
# Get unique mapping of species to guild
guild_lookup <- data_filtered %>%
  select(species_4code_IBP, habitat_guild_BOTW) %>%
  distinct()

# Join to trend results
trend_results <- trend_results %>%
  left_join(guild_lookup,
            by = "species_4code_IBP")

guild_trends <- trend_results %>%
  group_by(habitat_guild_BOTW) %>%
  summarise(mean_annual_change = mean(annual_percent_rate,
                                      na.rm = TRUE),
    se_annual_change = sd(annual_percent_rate,
                          na.rm = TRUE) / sqrt(n()),
    mean_overall_change = mean(overall_percent_change,
                               na.rm = TRUE),
    n_species = n())

ggplot(guild_trends, aes(x = reorder(habitat_guild_BOTW, mean_annual_change), y = mean_annual_change)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_annual_change - se_annual_change,
                    ymax = mean_annual_change + se_annual_change),
                width = 0.2) +
  coord_flip() +
  labs(title = "Mean Annual Percent Change by Habitat Guild",
    x = "Habitat Guild",
    y = "Mean Annual Change (%)") +
  theme_pubr()

ggplot(trend_results,
       aes(x = habitat_guild_BOTW,
           y = annual_percent_rate)) +
  geom_boxplot(fill = "lightgray",
               outlier.color = "red") +
  geom_jitter(width = 0.2,
              alpha = 0.5,
              aes(color = trend_category)) +
  labs(title = "Species Annual Trends by Habitat Guild",
    x = "Habitat Guild",
    y = "Annual Percent Change (%)") +
  theme_pubr()


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 5.0 Plot the overall percent change for each habitat guild

# Calculate the total abundance (number of male breeding bird territories), for
# all specified habitat guilds (habitat_guild_BOTW)
guild_data <- data_filtered %>%
  group_by(habitat_guild_BOTW, sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop")

# Specify empty lists, and data frames for model setup
guild_models <- list()
guild_predictions <- data.frame()
guild_list <- unique(guild_data$habitat_guild_BOTW)

for (g in guild_list) {
  g_data <- subset(guild_data, habitat_guild_BOTW == g)
  
  # Fit GAMM
  fit <- tryCatch({
    gamm(total_abundance ~ s(year, k = 10),
         random = list(sitecode = ~1),
         data = g_data)
  }, error = function(e) {
    message(paste("Error fitting guild:", g, "- skipping."))
    return(NULL)
  })
  
  if (!is.null(fit)) {
    guild_models[[g]] <- fit
    
    # Predict over time
    new_years <- seq(min(g_data$year), max(g_data$year), length.out = 100)
    newdata <- data.frame(year = new_years, sitecode = NA)
    
    pred <- predict(fit$gam, newdata = newdata, type = "response", se.fit = TRUE)
    
    g_pred <- data.frame(
      habitat_guild_BOTW = g,
      year = new_years,
      fit = pred$fit,
      se = pred$se.fit
    )
    
    guild_predictions <- rbind(guild_predictions, g_pred)
  }
}

guild_trends <- guild_predictions %>%
  group_by(habitat_guild_BOTW) %>%
  arrange(year) %>%
  summarise(
    abundance_start = first(fit),
    abundance_end = last(fit),
    total_change = abundance_end - abundance_start,
    percent_change = (abundance_end - abundance_start) / abundance_start * 100
  )

guild_trend_directions <- guild_predictions %>%
  group_by(habitat_guild_BOTW) %>%
  do({
    lm_fit <- lm(fit ~ year, data = .)
    coef <- summary(lm_fit)$coefficients
    data.frame(
      slope = coef["year", "Estimate"],
      p_value = coef["year", "Pr(>|t|)"],
      trend = ifelse(coef["year", "Pr(>|t|)"] >= 0.05, "Stable",
                     ifelse(coef["year", "Estimate"] > 0, "Increasing", "Decreasing"))
    )
  })

guild_trend_summary <- left_join(guild_trends, guild_trend_directions, by = "habitat_guild_BOTW")

# Reorder species by annual trend
guild_trend_summary <- guild_trend_summary %>%
  mutate(habitat_guild_BOTW = fct_reorder(habitat_guild_BOTW, total_change))

# Plot a bar graph for the rates of change for each species
rateofchange_plot <- ggplot(guild_trend_summary, # plot rate of change trends
                            aes(x = habitat_guild_BOTW, # specify x-axis values
                                y = total_change)) + # specify fill values
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Habitat guild",
       y = "Total abundance (no. of territories) change, 1991-2021") +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  theme(legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white",
                                        color = NA),  # white plot area
        plot.background = element_rect(fill = "white",
                                       color = NA),   # white outer background
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.95,
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
        axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 10,
                                        color = "black")) + # Formats y-axis text
  scale_y_continuous(limits = c(-6, 21),
                     expand = c(0, 0))  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales

# Visualize bar chart
print(rateofchange_plot)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 6.0 Plot the overall percent change for each preferred food item

# Calculate the total abundance (number of male breeding bird territories), for
# all specified preferred food items (food_item_BOTW)
food_data <- data_filtered %>%
  group_by(food_item_BOTW,
           sitecode,
           year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop")

# Specify empty lists, and data frames for model setup
food_models <- list()
food_predictions <- data.frame()
food_list <- unique(food_data$food_item_BOTW)

for (f in food_list) {
  f_data <- subset(food_data,
                   food_item_BOTW == f)
  
  # Fit GAMM
  fit <- tryCatch({
    gamm(total_abundance ~ s(year, k = 10),
         random = list(sitecode = ~1),
         data = f_data)
  }, error = function(e) {
    message(paste("Error fitting guild:", f, "- skipping."))
    return(NULL)
  })
  
  if (!is.null(fit)) {
    food_models[[f]] <- fit
    
    # Predict over time
    new_years <- seq(min(f_data$year),
                     max(f_data$year),
                     length.out = 100)
    newdata <- data.frame(year = new_years,
                          sitecode = NA)
    
    pred <- predict(fit$gam,
                    newdata = newdata,
                    type = "response",
                    se.fit = TRUE)
    
    f_pred <- data.frame(food_item_BOTW = f,
                         year = new_years,
                         fit = pred$fit,
                         se = pred$se.fit)
    
    food_predictions <- rbind(food_predictions, f_pred)
  }
}

food_trends <- food_predictions %>%
  group_by(food_item_BOTW) %>%
  arrange(year) %>%
  summarise(
    abundance_start = first(fit),
    abundance_end = last(fit),
    total_change = abundance_end - abundance_start,
    percent_change = (abundance_end - abundance_start) / abundance_start * 100
  )

food_trend_directions <- food_predictions %>%
  group_by(food_item_BOTW) %>%
  do({
    lm_fit <- lm(fit ~ year,
                 data = .)
    coef <- summary(lm_fit)$coefficients
    data.frame(
      slope = coef["year",
                   "Estimate"],
      p_value = coef["year",
                     "Pr(>|t|)"],
      trend = ifelse(coef["year",
                          "Pr(>|t|)"] >= 0.05,
                     "Stable",
                     ifelse(coef["year",
                                 "Estimate"] > 0,
                            "Increasing", "Decreasing")))
  })

food_trend_summary <- left_join(food_trends,
                                 food_trend_directions,
                                 by = "food_item_BOTW")

# Reorder species by annual trend
food_trend_summary <- food_trend_summary %>%
  mutate(food_item_BOTW = fct_reorder(food_item_BOTW,
                                      total_change))

# Plot a bar graph for the rates of change for each species
rateofchange_plot <- ggplot(food_trend_summary, # plot rate of change trends
                            aes(x = food_item_BOTW, # specify x-axis values
                                y = total_change)) + # specify fill values
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Preferred food item",
       y = "Total abundance (no. of territories) change, 1991-2021") +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  theme(legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white",
                                        color = NA),  # white plot area
        plot.background = element_rect(fill = "white",
                                       color = NA),   # white outer background
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.95,
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
        axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 10,
                                        color = "black")) + # Formats y-axis text
  scale_y_continuous(limits = c(-6, 21),
                     expand = c(0, 0))  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales

# Visualize bar chart
print(rateofchange_plot)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# 7.0 Plot the overall percent change for each nesting guild

# Calculate the total abundance (number of male breeding bird territories), for
# all specified nesting guilds (nest_guild_BOTW)
nest_guild_data <- data_filtered %>%
  group_by(nest_guild_BOTW,
           sitecode,
           year) %>%
  summarise(total_abundance = sum(territories,
                                  na.rm = TRUE),
            .groups = "drop")

# Specify empty lists, and data frames for model setup
nest_guild_models <- list()
nest_guild_predictions <- data.frame()
nest_guild_list <- unique(nest_guild_data$nest_guild_BOTW)

for (n in nest_guild_list) {
  n_data <- subset(nest_guild_data,
                   nest_guild_BOTW == n)
  
  # Fit GAMM
  fit <- tryCatch({
    gamm(total_abundance ~ s(year,
                             k = 10),
         random = list(sitecode = ~1),
         data = n_data)
  }, error = function(e) {
    message(paste("Error fitting guild:", n, "- skipping."))
    return(NULL)
  })
  
  if (!is.null(fit)) {
    nest_guild_models[[n]] <- fit
    
    # Predict over time
    new_years <- seq(min(n_data$year),
                     max(n_data$year),
                     length.out = 100)
    newdata <- data.frame(year = new_years,
                          sitecode = NA)
    
    pred <- predict(fit$gam,
                    newdata = newdata,
                    type = "response",
                    se.fit = TRUE)
    
    n_pred <- data.frame(nest_guild_BOTW = n,
                         year = new_years,
                         fit = pred$fit,
                         se = pred$se.fit)
    
    nest_guild_predictions <- rbind(nest_guild_predictions,
                                    n_pred)
  }
}

nest_guild_trends <- nest_guild_predictions %>%
  group_by(nest_guild_BOTW) %>%
  arrange(year) %>%
  summarise(abundance_start = first(fit),
            abundance_end = last(fit),
            total_change = abundance_end - abundance_start,
            percent_change = (abundance_end - abundance_start) / abundance_start * 100)

nest_guild_trend_directions <- nest_guild_predictions %>%
  group_by(nest_guild_BOTW) %>%
  do({
    lm_fit <- lm(fit ~ year, data = .)
    coef <- summary(lm_fit)$coefficients
    data.frame(slope = coef["year",
                   "Estimate"],
               p_value = coef["year",
                     "Pr(>|t|)"],
               trend = ifelse(coef["year",
                          "Pr(>|t|)"] >= 0.05,
                     "Stable",
                     ifelse(coef["year",
                                 "Estimate"] > 0,
                            "Increasing", "Decreasing")))
  })

nest_guild_trend_summary <- left_join(nest_guild_trends,
                                      nest_guild_trend_directions,
                                by = "nest_guild_BOTW")

# Reorder species by annual trend
nest_guild_trend_summary <- nest_guild_trend_summary %>%
  mutate(nest_guild_BOTW = fct_reorder(nest_guild_BOTW,
                                      total_change))

# Plot a bar graph for the rates of change for each species
nest_guild_plot <- ggplot(nest_guild_trend_summary, # plot rate of change trends
                            aes(x = nest_guild_BOTW, # specify x-axis values
                                y = total_change)) + # specify fill values
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(x = "Nesting guild",
       y = "Total abundance (no. of territories) change, 1991-2021") +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  theme(legend.position = "none", # Removes the legend
        panel.background = element_rect(fill = "white",
                                        color = NA),  # white plot area
        plot.background = element_rect(fill = "white",
                                       color = NA),   # white outer background
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.95,
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
        axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5,
                                   color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 10,
                                        color = "black")) + # Formats y-axis text
  scale_y_continuous(limits = c(-6, 21),
                     expand = c(0, 0))  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales

# Visualize bar chart
print(nest_guild_plot)








































#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 4.0 Breeding bird guild plots: data visualizations

#-----------------------------------------------------------------------------#
# 4.1 Plot individual guilds trends over time, for individual sites

guild_data <- data %>%
  group_by(sitecode,
           year,
           habitat_guild_BOTW) %>%
  summarise(total_abundance = sum(territories,
                                  na.rm = TRUE),
            .groups = "drop")

models <- list()
predictions <- data.frame()

# Step 1: Aggregate to guild-site-year level
guild_data <- data_filtered %>%
  group_by(habitat_guild_BOTW, sitecode, year) %>%
  summarise(
    total_territories = sum(territories, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Get list of unique nesting guilds
guild_list <- unique(guild_data$habitat_guild_BOTW)

# Step 3: Loop over each nesting guild
for (g in guild_list) {
  
  # Subset data for this guild
  g_data <- subset(guild_data, habitat_guild_BOTW == g)
  
  # Try fitting the GAMM
  fit <- tryCatch({
    gamm(total_territories ~ s(year, k = 10),
         random = list(sitecode = ~1),
         data = g_data,
         family = poisson(link = "log"))  # Adjust if overdispersion (e.g. use 'quasipoisson' or 'nb')
  }, error = function(e) {
    message(paste("Error fitting guild:", g, "- skipping."))
    return(NULL)
  })
  
  # If model fit successfully
  if (!is.null(fit)) {
    models[[g]] <- fit
    
    # Generate sequence of years for prediction
    new_years <- seq(min(g_data$year), max(g_data$year), length.out = 100)
    newdata <- data.frame(year = new_years, sitecode = NA)  # sitecode=NA to get population-level smooth
    
    # Predict smoothed abundance (GAM part only)
    pred <- predict(fit$gam, newdata = newdata, type = "response", se.fit = TRUE)
    
    # Compile prediction data
    g_pred <- data.frame(
      habitat_guild_BOTW = g,
      year = new_years,
      fit = pred$fit,
      se = pred$se.fit
    )
    
    # Combine predictions
    predictions <- rbind(predictions, g_pred)
  }
}


library(ggplot2)

ggplot(predictions,
       aes(x = year,
           y = fit,
           color = habitat_guild_BOTW)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = fit - se,
                  ymax = fit + se,
                  fill = habitat_guild_BOTW),
              alpha = 0.2,
              color = NA) +
  labs(
    title = "Smoothed Abundance Trends by Nesting Guild",
    x = "Year",
    y = "Predicted Total Territories"
  ) +
  theme_pubr()


 1) - head(pred$fit, 1)) / head(pred$fit, 1) * 100


library(dplyr)

trend_results <- predictions %>%
  group_by(species_4code_IBP) %>%
  arrange(year, .by_group = TRUE) %>% 
  summarise(
    overall_percent_change = (last(fit) / first(fit) - 1) * 100,
    annual_percent_rate = {
      lm_fit <- lm(log(fit) ~ year)
      rate <- coef(lm_fit)[2]
      (exp(rate) - 1) * 100
    }
  )

# Example structure
trend_results <- trend_results %>%
  mutate(
    trend_category = case_when(
      annual_percent_rate > 1 ~ "Increasing",
      annual_percent_rate < -1 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )

# Reorder species by annual trend
trend_results <- trend_results %>%
  mutate(species_4code_IBP = fct_reorder(species_4code_IBP, annual_percent_rate))

# Plot
ggplot(trend_results, 
       aes(x = species_4code_IBP,
           y = annual_percent_rate,
           fill = trend_category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Better readability for species names
  scale_fill_manual(values = c("Increasing" = "forestgreen",
                               "Stable" = "gray70",
                               "Decreasing" = "firebrick")) +
  labs(
    title = "Annual Rate of Change in Bird Species Abundance",
    x = "Species",
    y = "Annual Percent Change (%)"
  ) +
  theme_pubr()


# Plot V2:
ggplot(trend_results, aes(x = annual_percent_rate, y = fct_reorder(species_4code_IBP, annual_percent_rate), color = trend_category)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Increasing" = "forestgreen", "Stable" = "gray50", "Decreasing" = "firebrick")) +
  labs(
    title = "Species Annual Percent Change",
    x = "Annual % Change",
    y = "Species"
  ) +
  theme_minimal(base_size = 14)


# Plot V3:

# Faceted plot for both metrics
trend_results_long <- trend_results %>%
  select(species_4code_IBP, annual_percent_rate, overall_percent_change, trend_category) %>%
  pivot_longer(cols = c("annual_percent_rate", "overall_percent_change"),
               names_to = "metric", values_to = "percent_change")

ggplot(trend_results_long, aes(x = percent_change, y = fct_reorder(species_4code_IBP, percent_change), color = trend_category)) +
  geom_point(size = 3) +
  facet_wrap(~ metric, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Percent Change",
    y = "Species",
    title = "Annual vs Overall Percent Change by Species"
  ) +
  theme_minimal(base_size = 14)




# Guild breakdown:
# Get unique mapping of species to guild
guild_lookup <- data_filtered %>%
  select(species_4code_IBP, habitat_guild_BOTW) %>%
  distinct()

# Join to trend results
trend_results <- trend_results %>%
  left_join(guild_lookup, by = "species_4code_IBP")

guild_trends <- trend_results %>%
  group_by(habitat_guild_BOTW) %>%
  summarise(
    mean_annual_change = mean(annual_percent_rate, na.rm = TRUE),
    se_annual_change = sd(annual_percent_rate, na.rm = TRUE) / sqrt(n()),
    mean_overall_change = mean(overall_percent_change, na.rm = TRUE),
    n_species = n()
  )

ggplot(guild_trends, aes(x = reorder(habitat_guild_BOTW, mean_annual_change), y = mean_annual_change)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_annual_change - se_annual_change,
                    ymax = mean_annual_change + se_annual_change),
                width = 0.2) +
  coord_flip() +
  labs(
    title = "Mean Annual Percent Change by Habitat Guild",
    x = "Habitat Guild",
    y = "Mean Annual Change (%)"
  ) +
  theme_minimal(base_size = 14)

ggplot(trend_results, aes(x = habitat_guild_BOTW, y = annual_percent_rate)) +
  geom_boxplot(fill = "lightgray", outlier.color = "red") +
  geom_jitter(width = 0.2, alpha = 0.5, aes(color = trend_category)) +
  labs(
    title = "Species Annual Trends by Habitat Guild",
    x = "Habitat Guild",
    y = "Annual Percent Change (%)"
  ) +
  theme_minimal(base_size = 14)


#-----------------------------------------------------------------------------#

# GAMM for guild:

guild_data <- data %>%
  group_by(sitecode,
           year,
           habitat_guild_BOTW) %>%
  summarise(total_abundance = sum(territories,
                                  na.rm = TRUE),
            .groups = "drop")

models <- list()
predictions <- data.frame()

# Step 1: Aggregate to guild-site-year level
guild_data <- data_filtered %>%
  group_by(habitat_guild_BOTW, sitecode, year) %>%
  summarise(
    total_territories = sum(territories, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Get list of unique nesting guilds
guild_list <- unique(guild_data$habitat_guild_BOTW)

# Step 3: Loop over each nesting guild
for (g in guild_list) {
  
  # Subset data for this guild
  g_data <- subset(guild_data, habitat_guild_BOTW == g)
  
  # Try fitting the GAMM
  fit <- tryCatch({
    gamm(total_territories ~ s(year, k = 10),
         random = list(sitecode = ~1),
         data = g_data,
         family = poisson(link = "log"))  # Adjust if overdispersion (e.g. use 'quasipoisson' or 'nb')
  }, error = function(e) {
    message(paste("Error fitting guild:", g, "- skipping."))
    return(NULL)
  })
  
  # If model fit successfully
  if (!is.null(fit)) {
    models[[g]] <- fit
    
    # Generate sequence of years for prediction
    new_years <- seq(min(g_data$year), max(g_data$year), length.out = 100)
    newdata <- data.frame(year = new_years, sitecode = NA)  # sitecode=NA to get population-level smooth
    
    # Predict smoothed abundance (GAM part only)
    pred <- predict(fit$gam, newdata = newdata, type = "response", se.fit = TRUE)
    
    # Compile prediction data
    g_pred <- data.frame(
      habitat_guild_BOTW = g,
      year = new_years,
      fit = pred$fit,
      se = pred$se.fit
    )
    
    # Combine predictions
    predictions <- rbind(predictions, g_pred)
  }
}


library(ggplot2)

ggplot(predictions,
       aes(x = year,
           y = fit,
           color = habitat_guild_BOTW)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = fit - se,
                  ymax = fit + se,
                  fill = habitat_guild_BOTW),
              alpha = 0.2,
              color = NA) +
  labs(
    title = "Smoothed Abundance Trends by Nesting Guild",
    x = "Year",
    y = "Predicted Total Territories"
  ) +
  theme_minimal(base_size = 14)











# Step 4: Plot all species smooth trends with confidence ribbons

species_list <- unique(predictions$species_4code_IBP)

for (sp in species_list) {
  sp_pred <- subset(predictions,
                    species_4code_IBP == sp)
  
  p <- ggplot(sp_pred,
              aes(x = year,
                  y = fit)) +
    geom_line(color = "black",
              size = 1) +
    geom_hline(yintercept = 0, 
               color = "black") +  # Horizontal line
    geom_vline(xintercept = 1990,
               color = "black") +
    geom_ribbon(aes(ymin = fit - 1.96*se, 
                    ymax = fit + 1.96*se), 
                alpha = 0.2,
                fill = "lightblue") +
    labs(title = paste("GAMM Trend for Species:", sp),
         x = "Year",
         y = "Predicted number of male breeding bird territories") +
    theme(legend.position = "none", # Removes the legend
          panel.background = element_rect(fill = "white",
                                          color = NA),  # white plot area
          plot.background = element_rect(fill = "white",
                                         color = NA),   # white outer background
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
          axis.text.x = element_text(size = 10,
                                     color = "black"), # Formats x-axis text
          axis.text.y.left = element_text(size = 10,
                                          color = "black")) + # Formats y-axis text
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(limits = c(1990, 2022),
                       expand = c(0, 0))
  theme_pubr()
  
  print(p)  # to display in console
  
  # Optionally save each plot as a PNG
  # ggsave(filename = paste0("GAMM_trend_", sp, ".png"), plot = p, width = 6, height = 4)
}

#


























# DRAFTS


#-----------------------------------------------------------------------------#
# 3.1 Plot individual species trends over time, for individual sites

# Ensure correct formats
bird_territories_with_traits <- bird_territories_with_traits %>%
  mutate(
    year = as.numeric(as.character(year)),
    sitecode = as.factor(sitecode),
    species_4code_IBP = as.factor(species_4code_IBP)
  )

# Create species list
species_list <- unique(bird_territories_with_traits$species_4code_IBP)

# Model storage
models <- list()

# Fit GAMMs by species
for (sp in species_list) {
  bird_territories_with_traits_sp <- bird_territories_with_traits %>%
    filter(species_4code_IBP == sp)
  
  model <- tryCatch({
    gamm4(
      territories ~ s(year, k = 10),
      random = ~(1 | sitecode),
      data = bird_territories_with_traits_sp,
      family = nb()
    )
  }, error = function(e) NULL)
  
  models[[as.character(sp)]] <- model
}

# Extract fitted values
predicted_df <- data.frame()

for (sp in names(models)) {
  model <- models[[sp]]
  if (!is.null(model)) {
    df_sp <- bird_territories_with_traits %>%
      filter(species_4code_IBP == sp)
    
    df_sp$fitted <- predict(model$gam, newdata = df_sp, type = "response")
    
    # ✅ Add a NEW column for consistent faceting
    df_sp$species_for_plot <- as.character(sp)
    
    predicted_df <- bind_rows(predicted_df, df_sp)
  }
}

# Convert to factor for plotting
predicted_df$species_for_plot <- factor(predicted_df$species_for_plot)

# Check structure
str(predicted_df)

ggplot(predicted_df, aes(x = year, y = fitted)) +
  geom_line(color = "blue", linewidth = 1) +
  facet_wrap(~ species_for_plot, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "GAMM Trends in Territories Over Time",
    y = "Fitted Territories",
    x = "Year"
  )










#-----------------------------------------------------------------------------#
# 3.1 Plot individual species trends over time, for individual sites

# Clean input data
bird_territories_with_traits <- bird_territories_with_traits %>%
  mutate(
    year = as.numeric(as.character(year)),
    sitecode = as.factor(sitecode),
    species_4code_IBP = as.factor(species_4code_IBP)
  )

# Species list
species_list <- unique(bird_territories_with_traits$species_4code_IBP)
models <- list()
predicted_df <- data.frame()

# Fit models and extract predictions
for (sp in species_list) {
  sp_chr <- as.character(sp)
  
  df_sp <- bird_territories_with_traits %>%
    filter(species_4code_IBP == sp)
  
  if (nrow(df_sp) < 5) next  # Skip if not enough data
  
  model <- tryCatch({
    gamm4(
      territories ~ s(year, k = 10),
      random = ~(1 | sitecode),
      data = df_sp,
      family = nb()
    )
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    models[[sp_chr]] <- model
    
    # Predict safely
    preds <- tryCatch({
      df_sp$fitted <- predict(model$gam, newdata = df_sp, type = "response")
      df_sp$species_facet <- sp_chr  # ✅ Add separate column for faceting
      df_sp
    }, error = function(e) NULL)
    
    if (!is.null(preds) && nrow(preds) > 0) {
      predicted_df <- bind_rows(predicted_df, preds)
    }
  }
}

# Check structure before plotting
str(predicted_df)
summary(predicted_df$species_facet)


ggplot(predicted_df, aes(x = year, y = fitted)) +
  geom_line(color = "blue", linewidth = 1) +
  facet_wrap(~ species_facet, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "GAMM Trends in Territories Over Time",
    x = "Year",
    y = "Fitted Territories"
  )








#-----------------------------------------------------------------------------#
# 3.1 Plot individual species trends over time, for individual sites

# Get list of species to filter data by species
species_list <- unique(bird_territories_with_traits$species_4code_IBP)  

# Loop through each unique species and create separate plots
for (species_codes in species_list) {
  # Filter data for the specific species
  species_by_site_over_time_plots  <- ggplot(filter(bird_territories_with_traits,
                                                  species_4code_IBP == species_codes),
                                           aes(x = year,
                                               y = territories,
                                               color = sitecode,
                                               group = sitecode)) +
    geom_point(size = 1.2) + # Add points for better visibility
    geom_smooth(method = "lm") +
    theme_pubr() +
    theme(legend.position = "none", # Removes the legend
          panel.spacing.x = unit(1.0, "cm"),
          strip.text.x = element_text(size = 8,
                                      face = "bold"),
          strip.background = element_blank(),
          plot.title = element_text(size = 12,
                                    face = "bold",
                                    hjust = 0.5,
                                    color = "black",
                                    margin = margin(b = 15)), # Centers the title
          axis.title.x.bottom = element_text(size = 12,
                                             face = "bold",
                                             colour = "black",
                                             margin = margin(t = 15)), # Customizes x-axis title
          axis.title.y.left = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(r = 15)), # Customizes y-axis title
          axis.text.x = element_text(size = 8, 
                                     color = "black"),
          axis.text.y.left = element_text(size = 8,
                                          color = "black")) +
    facet_wrap(~ sitecode, 
               ncol = 3, 
               nrow = 5,
               scales = "free_y") +  # Separate plots per site
    labs(title = paste("Territories of", species_codes, 
                       "over time at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Number of territories") +
    xlim(1991, 2021) # Set x-limit minimum and maximum
  
  # Print plot in the R console
  print(species_by_site_over_time_plots)
}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 8.0 Breeding bird nesting guild plots: data visualizations

#-----------------------------------------------------------------------------#
# 8.1 Plot nesting guild trends over time, for all sites

# Summarize: total territories per site, year, and nesting guild
total_territories <- bird_territories_with_traits %>%
  filter(dominant_ELC_site_code %in% c("WOD")) %>% 
  group_by(nest_guild_BOTW,
           sitecode,
           year) %>%
  summarise(total_territories = sum(territories, 
                                    na.rm = TRUE), 
            .groups = "drop")

# Get list of guilds (not species)
nest_guild_list <- unique(total_territories$nest_guild_BOTW)

# Loop through each nest guild
for (guild in nest_guild_list) {
  
  territories_by_guild <- filter(total_territories, 
                                 nest_guild_BOTW == guild)
  
  territories_by_guild_plot <- ggplot(territories_by_guild,
                                      aes(x = year,
                                          y = total_territories)) +
    geom_point(color = "steelblue",
               size = 1.2) +
    geom_smooth(method = "lm", 
                color = "steelblue") +
    geom_hline(yintercept = 0, 
               color = "black") +  # Horizontal line
    geom_vline(xintercept = 1990,
               color = "black") +
    theme_pubr() +
    theme(legend.position = "none",
          panel.spacing.x = unit(1.0, "cm"),
          strip.text.x = element_text(size = 8, 
                                      face = "bold"),
          strip.background = element_blank(),
          plot.title = element_text(size = 12, 
                                    face = "bold"),
          axis.title.x.bottom = element_text(size = 12,
                                             face = "bold",
                                             margin = margin(t = 15)),
          axis.title.y.left = element_text(size = 12, 
                                           face = "bold",
                                           margin = margin(r = 15)),
          axis.text.x = element_text(size = 8),
          axis.text.y.left = element_text(size = 8)) +
    labs(title = paste(guild),
         x = "Year",
         y = "Number of male breeding bird territories") +
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(breaks = seq(min(territories_by_guild$year),
                                    max(territories_by_guild$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))
  print(territories_by_guild_plot)
}

#-----------------------------------------------------------------------------#
# 8.2 Plot nesting guild trends over time, for individual sites

# Summarize: total territories per site, year, and nesting guild
total_territories <- bird_territories_with_traits %>%
  filter(dominant_ELC_site_code %in% c("WOD")) %>% 
  group_by(nest_guild_BOTW,
           sitecode,
           year) %>%
  summarise(total_territories = sum(territories, 
                                    na.rm = TRUE), 
            .groups = "drop")

# Get list of guilds (not species)
nest_guild_list <- unique(total_territories$nest_guild_BOTW)

# Loop through each nest guild
for (guild in nest_guild_list) {
  
  territories_by_guild <- filter(total_territories, 
                      nest_guild_BOTW == guild)
  
  territories_by_guild_plot <- ggplot(territories_by_guild,
                                   aes(x = year,
                                       y = total_territories,
                                       group = sitecode)) +
    geom_point(color = "steelblue",
               size = 1.2) +
    geom_smooth(method = "lm", 
                color = "steelblue") +
    geom_hline(yintercept = 0, 
               color = "black") +  # Horizontal line
    geom_vline(xintercept = 1990,
               color = "black") +
    theme_pubr() +
    theme(legend.position = "none",
          panel.spacing.x = unit(1.0, "cm"),
          strip.text.x = element_text(size = 8, 
                                      face = "bold"),
          strip.background = element_blank(),
          plot.title = element_text(size = 12, 
                                    face = "bold"),
          axis.title.x.bottom = element_text(size = 12,
                                             face = "bold",
                                             margin = margin(t = 15)),
          axis.title.y.left = element_text(size = 12, 
                                           face = "bold",
                                           margin = margin(r = 15)),
          axis.text.x = element_text(size = 8),
          axis.text.y.left = element_text(size = 8)) +
    facet_wrap(~ sitecode,
               ncol = 3,
               nrow = 5,
               scales = "free_y") +
    labs(title = paste(guild),
         x = "Year",
         y = "Number of male breeding bird territories") +
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(breaks = seq(min(territories_by_guild$year),
                                    max(territories_by_guild$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))
  print(territories_by_guild_plot)
}

#-----------------------------------------------------------------------------#
# 8.3 Plot shurb-nesting species (12) trends over time, for all WOD sites (5)

# Filter for specific shrub-nesting species (12) that are excluding marsh
# habitat preferred bird species (4). This include 'scrub', 'open woodlands',  
# and 'towns'.

  # Summarize: total territories per site, year, and nesting guild
total_territories <- bird_territories_with_traits %>%
    filter(dominant_ELC_site_code %in% c("WOD")) %>% 
    filter(nest_guild_BOTW %in% c("shrub")) %>%
    filter(habitat_guild_BOTW %in% c("scrub", "open woodlands", "towns")) %>%
    group_by(nest_guild_BOTW,
           sitecode,
           year) %>%
    summarise(total_territories = sum(territories, 
                                    na.rm = TRUE), 
            .groups = "drop")

territories_by_guild_plot <- ggplot(total_territories,
                                      aes(x = year,
                                          y = total_territories)) +
    geom_point(color = "steelblue",
               size = 1.2) +
    geom_smooth(method = "lm", 
                color = "steelblue") +
    geom_hline(yintercept = 0, 
               color = "black") +  # Horizontal line
    geom_vline(xintercept = 1990,
               color = "black") +
    theme_pubr() +
    theme(legend.position = "none",
          panel.spacing.x = unit(1.0, "cm"),
          strip.text.x = element_text(size = 8, 
                                      face = "bold"),
          strip.background = element_blank(),
          plot.title = element_text(size = 12, 
                                    face = "bold"),
          axis.title.x.bottom = element_text(size = 12,
                                             face = "bold",
                                             margin = margin(t = 15)),
          axis.title.y.left = element_text(size = 12, 
                                           face = "bold",
                                           margin = margin(r = 15)),
          axis.text.x = element_text(size = 8),
          axis.text.y.left = element_text(size = 8)) +
    labs(title = "Shrub-nesting",
         x = "Year",
         y = "Number of male breeding bird territories") +
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(breaks = seq(min(total_territories$year),
                                    max(total_territories$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))

print(territories_by_guild_plot)

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





