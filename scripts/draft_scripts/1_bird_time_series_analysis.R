#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Long Point Breeding Bird Census Analysis
# Subheading:     Time Series Analysis - Bird Abundance (territory) Data
# Author:         Joshua Pickering
# Affiliation:    University of Waterloo
# Creation Date:  2024-06-18
# Last Updated:   2025-04-29
# Description:    This script includes results of exploratory summary data and 
#                 visualizations for the species abundance of breeding bird 
#                 census territory data collected at Long Point, Ontario, 
#                 Canada from 1991 - 2021
#
# Data Sources:   "../data/bird_spterritories_abund.csv"
#
# R Version:      4.4.2 (2024-10-31 ucrt)
#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PART 1: 

# Script setup

# Clear all objects from the R Environment prior to data exploration
rm(list = ls())  # Remove all objects

#-----------------------------------------------------------------------------#

# Identify and install necessary R packages for data exploration

# To reduce unnecessary loading, run 'options' function
options(warn = -1)  # Suppress warnings if packages are already loaded

# Identify required packages
list.of.packages <- c("ggplot2", "dplyr", "tidyr", "patchwork", "broom",
                      "plotly", "ggpubr", "lme4", "htmlwidgets", "mgcv",
                      "tidyverse", "pheatmap", "vegan", "purrr")

# Create function to install packages if they are missing from the library
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# Install missing packages
lapply(list.of.packages, install_if_missing)

# Load required packages
lapply(list.of.packages, library, character.only = TRUE) 

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PART 2: 

# Import csv file of breeding bird census abundance (territory) data

# Import data from csv file, saved in the 'data' sub-folder of the rproject
bird.abund.data <- read.csv("../data/bird_spterritories_abund.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PART 3: 

# Explore data and summarize information of sampled variables.

# Identify the structure of the data
glimpse(bird.abund.data)

# Identify the structure of the data
str(bird.abund.data)

# View data table
view(bird.abund.data)

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
summary(bird.abund.data) # Summarize data length, class, mode, minimum, median,
# mean, and maximum

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PART 4: 

# Summarize sampling effort

# Identify number of sampled sites at Long Point, Ontario, Canada
# Visualize with bar chart of sampling effort over time, 1991-2021

### Identify the total number of sites sampled for each year over time
sites.per.year <- bird.abund.data %>%
  group_by(year) %>%
  summarise(sites_sampled = n_distinct(sitecode), .groups = "drop")

print(sites.per.year, n = Inf)

# Plot the total number of sites sampled over time using 'ggplot' function
sites.per.year.plot <- ggplot(sites.per.year, 
                              aes(x = year,
                                  y = sites_sampled)) +
  geom_bar(stat = "identity", 
           fill = "steelblue") +  
  theme_pubr() +
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
                                   color = "black",
                                   angle = 45,
                                   hjust = 0.5,
                                   vjust = 0.5), # Formats x-axis text
        axis.text.y.left = element_text(size = 8,
                                        color = "black")) + # Formats y-axis text
  labs(title = "Number of breeding bird census sites sampled per year
       at Long Point, Ontario, Canada, 1991-2021", 
       x = "Year", 
       y = "Number of sites sampled") +
  scale_x_continuous(breaks = seq(min(sites.per.year$year),
                                  max(sites.per.year$year),
                                  by = 1),
                     expand = c(0,0.5)) + # Show all years on x-axis
  scale_y_continuous(breaks = seq(min(0),
                                  max(sites.per.year$sites_sampled),
                                  by = 1),
                     expand = c(0,0))

# Visualize the total number of sites sampled over time
print(sites.per.year.plot)

#-----------------------------------------------------------------------------#

## 2. Visualize with heat map of sampling effort over time, 1991-2021

# Plot the total number of sites sampled over time using 'ggplot' function
sites.per.year.heatmap <- ggplot(sites.per.year, aes(x = year, 
                                                     y = sites_sampled)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("TRUE" = "steelblue",
                               "FALSE" = "grey90")) +
  labs(title = "Sampling Effort Over Time by Site",
       x = "Year",
       y = "Site",
       fill = "Sampled?") +
  theme_pubr()

# Visualize the total number of sites sampled over time
print(sites.per.year.heatmap)

#-----------------------------------------------------------------------------#

# Save ggplot file in results folder with 'ggsave' function
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/sites_per_year_plot.png"), 
       plot = sites.per.year.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#

# Count number of years sampled per site
years.per.site <- bird.abund.data %>%
  group_by(sitecode) %>%
  summarise(years_sampled = n_distinct(year)) %>%
  arrange(desc(years_sampled))

# Print the count
print(years.per.site)

# Visualization: Number of years sampled per site
ggplot(years.per.site, aes(x = sitecode, y = years_sampled)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.9, fill = "steelblue") +
  coord_polar(start = 0) +  # Convert to circular format
  labs(x = NULL, y = NULL, title = "Number of Years Sampled per Site") +
  geom_text(aes(label = years_sampled, y = years_sampled / 2), color = "white", size = 3) +
  theme_pubr() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),    # Remove y-axis text
    axis.ticks.y = element_blank(),   # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
  )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# PART 4:

# Test and visualize the distribution of the dependent variable

# Identify data normality

# Histogram check for normality using 'ggplot' function and 'geom_histogram' function
bird.abund.data %>% # Pipe operator to pass the output of one function into another function
  ggplot(aes(x = territories)) + # identify x-value as abundance (territories) for ggplot function
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) + # plot as a histogram, with customization options
  theme_pubr() + # Formats plot to a 'publication ready' theme
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 15)), # Formats the plot title
        axis.title.x.bottom = element_text(size = 12, face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
        axis.title.y.left = element_text(size = 12, face = "bold", colour = "black", margin = margin(r = 15)), # Formats y-axis title
        axis.text.x = element_text(size = 8, color = "black"), # Formats x-axis text
        axis.text.y.left = element_text(size = 8, color = "black")) + # Formats y-axis text
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021", # Title label for plot 
       x = "Number of territories", # x-axis label for plot
       y = "Sample frequency") + # y-axis label for plot
  scale_y_continuous(expand = c(0, 0)) +  # Set fixed y-axis limits for continuous data
  scale_x_continuous(expand = c(0, 0.5)) # Set fixed x-axis limits for continuous data

# Histogram shows a right tail, skewed by high outlier values, and many low values (0.25 - 5 territories)

# Shapiro-Wilk test for normality
shapiro.test(bird.abund.data$territories) 

# Shapiro-Wilk test p-value is < 0.05, suggests non-normal data

# Assess normality by employing a Q-Q (quantile-quantile) plot 
qqnorm(bird.abund.data$territories) # plot abundance data (territories) on a Q-Q plot

# plot normal distribution line on Q-Q plot
qqline(bird.abund.data$territories, col = "red") # plot normal distribution line on Q-Q plot

# Quantiles of the observed data do not follow the expected quantiles of a theoretical normal distribution shown by the red line

# Results of the histogram, Shapiro-Wilk test, and Q-Q plot suggest data is not normally distributed
# Dependent variable is not normally distributed

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Aggregate total territories per site per year
site_data <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_territories = sum(territories, na.rm = TRUE)) %>%
  ungroup()

site_data

model.set1 <- glm(total_territories ~ year, family = poisson, data = site_data)

summary(model.set1)

overdispersion_factor <- model.set1$deviance / model.set1$df.residual

overdispersion_factor

#If the overdispersion factor > 1 (especially > 2), this indicates overdispersion. 
# This is overdispersed (value = 30)

# consider negative binomial if sum = >2

library(MASS)
model.set2 <- glm.nb(total_territories ~ year, data = site_data)

summary(model.set2)

pearson_residuals <- residuals(model.set2, type = "pearson")
sum(pearson_residuals^2) / model.set2$df.residual

plot(model.set2$fitted.values, residuals(model.set2, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Residuals vs. Fitted Values")

abline(h = 0, col = "red", lty = 2)  # Add reference line

qqnorm(residuals(model.set2, type = "deviance"))

qqline(residuals(model.set2, type = "deviance"), col = "red")

plot(site_data$year, site_data$total_territories, col = "black", pch = 16, 
     xlab = "Year", ylab = "Total Territories")
lines(site_data$year, predict(model.set2, type = "response"), col = "grey")

# good model fit, and interpretation of results suggests a positive result
# (but not significant) of time (year) on breeding bird abundance (total territories)

install.packages("caret")
library(caret)

# Set up k-fold cross-validation (e.g., 5-fold)
train_control <- trainControl(method = "cv", number = 5)

# Fit the Negative Binomial model using cross-validation
cv_model <- train(total_territories ~ year, data = site_data, 
                  method = "glm.nb", trControl = train_control)

# Print the cross-validation results
cv_model

# An RMSE of ~48, this means that, on average, 
# the model’s predictions deviate from the actual values by 5 units.

# Lower R² indicates that the model does not explain much of the variance 
# (i.e., 0.03 means 3% of the variance is explained) and model is not a good fit.

# MAE = 40.1 means that the model, on average has an error of 40.1 units 
# between its predictions and the actual observed total territories.

# Print cross-validation summary
summary(cv_model)

# Plot cross-validation results (e.g., RMSE)
boxplot(cv_model$resamples$RMSE, 
        main = "Cross-Validation RMSE for Negative Binomial Model",
        ylab = "RMSE")

# Extract predictions for the last fold
predictions <- predict(cv_model, newdata = site_data)

# Plot observed vs predicted values
ggplot(site_data, aes(x = total_territories, y = predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Observed vs Predicted Values", 
       x = "Observed Total Territories", 
       y = "Predicted Total Territories")


# Plot the observed vs predicted
predictions <- predict(model.set2, type = "response")
ggplot(site_data, aes(x = year, y = total_territories)) +
  geom_point() +
  geom_line(aes(y = predictions), color = "red") +
  labs(title = "Observed vs. Predicted Territories", x = "Year", y = "Total Territories")















#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Identify site-level data normality with histogram visuals and Shapiro-Wilk tests

# Create histograms for each sample site (sitecode) with the 'facet_wrap' function
bird.abund.data %>%
  ggplot(aes(x = territories)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free") +  # Separate plots per site, and format for grid layout (i.e., 3 subplots x 5 subplots)
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Total number of territories",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(expand = c(0, 0.5))

# Histograms show a right tail, skewed by high outlier values, and many low values (0.25 - 5 territories)

# Additional Shapiro-Wilk test for normality on all data within specific sites
shapiro.test <- bird.abund.data %>%
  group_by(sitecode) %>%
  summarise(shapiro_p = list(shapiro.test(territories)$p.value)) %>%
  unnest(shapiro_p)

# View results
print(shapiro.test)

# Shapiro-Wilk test p-value is < 0.05, suggests non-normal data

# Results of the site histograms and individual Shapiro-Wilk site tests suggest data is not normally distributed

#-----------------------------------------------------------------------------#

# Visualize trend of abundance (territories) over time (1991-2021) for all sites (15 total)

# Create a wrapped plot (by site) to visualize the abundance of all sampled species over time using the 'facet_wrap' function
bird.abund.scatrplot <- ggplot(bird.abund.data, aes(x = year, y = territories, color = species_4code_IBP)) +
  geom_point(size = 1.2) + # Add points for better visibility
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +  # Separate plots per site, and format for grid layout (i.e., 3 subplots x 5 subplots)
  labs(title = "Breeding bird territories of all sampled species for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0))

# Visualize all breeding bird census species' territories sampled over time (1991-2021) for each site (15 total)
bird.abund.scatrplot

#-----------------------------------------------------------------------------#

# Visualize and explore plots with 'ggplotly' function
# Note these are imperfect visuals and lack many format customization options, but use for data exploration

# Convert ggplot to interactive plots
bird.abund.plotly <- ggplotly(bird.abund.scatrplot) %>%
  layout(yaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for y-axis text
         yaxis2 = list(tickfont = list(size = 10, color = "black")), 
         yaxis3 = list(tickfont = list(size = 10, color = "black")), 
         yaxis4 = list(tickfont = list(size = 10, color = "black")), 
         yaxis5 = list(tickfont = list(size = 10, color = "black")), 
         yaxis6 = list(tickfont = list(size = 10, color = "black")), 
         yaxis7 = list(tickfont = list(size = 10, color = "black")), 
         yaxis8 = list(tickfont = list(size = 10, color = "black")), 
         yaxis9 = list(tickfont = list(size = 10, color = "black")), 
         yaxis10 = list(tickfont = list(size = 10, color = "black")), 
         yaxis11 = list(tickfont = list(size = 10, color = "black")), 
         yaxis12 = list(tickfont = list(size = 10, color = "black")), 
         yaxis13 = list(tickfont = list(size = 10, color = "black")), 
         yaxis14 = list(tickfont = list(size = 10, color = "black")), 
         yaxis15 = list(tickfont = list(size = 10, color = "black")), 
         xaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for x-axis text
         xaxis2 = list(tickfont = list(size = 10, color = "black")), 
         xaxis3 = list(tickfont = list(size = 10, color = "black")))

# View interactive plots
print(bird.abund.plotly)

# Save plot
saveWidget(bird.abund.plotly, file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_abund_plotly.html", selfcontained = TRUE)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Identify data normality with histogram visuals and Shapiro-Wilk tests for abundance (territories) totals sampled over the study period (1991-2021)

# Histogram check for normality using 'ggplot' function and 'geom_histogram' function
bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = total_abundance)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
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
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Total number of territories",
       y = "Frequency") +
  scale_y_continuous(limits = c(0, 15),expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(0, 250), expand = c(0, 0))

# Histogram shows a right tail, skewed by high outlier values, and many low values (10 - 100 territories)

# Compute total abundance and assess for normality with Shapiro-Wilk test
shapiro.total.territories <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop") %>%
  summarise(shapiro_p = list(shapiro.test(total_abundance)$p.value)) %>%
  unnest(shapiro_p)

# View results
print(shapiro.total.territories)

# Shapiro-Wilk test p-value is < 0.05, suggests non-normal data

# Results of the histogram and individual Shapiro-Wilk test suggest data is not normally distributed

#-----------------------------------------------------------------------------#

# Histogram check for normality using 'ggplot' function and 'geom_histogram' function for each site sample (sitecode x year)
bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  print(n = Inf) %>% # View total recorded abundance (territories) data for each sampling site across years
  ggplot(aes(x = total_abundance)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free") +
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Total number of territories",
       y = "Frequency") +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(0, 250), expand = c(0, 0.5))

# Histograms show normal data distribution

# Compute total abundance per site-year and test for normality for each site
shapiro.total.site.territories <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop") %>%
  group_by(sitecode) %>%
  summarise(shapiro_p = list(shapiro.test(total_abundance)$p.value)) %>%
  unnest(shapiro_p)

# View results
print(shapiro.total.site.territories)

# Shapiro-Wilk test p-value is < 0.05, suggests normal data except for DCSD site

# Results of the site histograms and individual Shapiro-Wilk site tests suggest all site's data is normally distributed except for site 'DCSD' (p = 0.03)

#-----------------------------------------------------------------------------#

# Create a linear trend line wrapped plot (by site) to visualize the total abundance (territories) for each site over time using the 'facet_wrap' function
bird.total.abund.line.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_abundance, group = sitecode)) +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
  geom_smooth(method = "lm", colour = "steelblue") +
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +  # Separate plots per site
  labs(title = "Total breeding bird territories for monitoring sites over time
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

# Visualize
print(bird.total.abund.line.plot)

# Save plot
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_line_plot.png"),
       plot = bird.total.abund.line.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#

# Visualize and explore plots with 'ggplotly' function
# Note these are imperfect visuals and lack many format customization options, but use for data exploration

# Convert ggplot to interactive plots
bird.total.abund.plotly <- ggplotly(bird.total.abund.line.plot) %>%
  layout(yaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for y-axis text
         yaxis2 = list(tickfont = list(size = 10, color = "black")), 
         yaxis3 = list(tickfont = list(size = 10, color = "black")), 
         yaxis4 = list(tickfont = list(size = 10, color = "black")), 
         yaxis5 = list(tickfont = list(size = 10, color = "black")), 
         yaxis6 = list(tickfont = list(size = 10, color = "black")), 
         yaxis7 = list(tickfont = list(size = 10, color = "black")), 
         yaxis8 = list(tickfont = list(size = 10, color = "black")), 
         yaxis9 = list(tickfont = list(size = 10, color = "black")), 
         yaxis10 = list(tickfont = list(size = 10, color = "black")), 
         yaxis11 = list(tickfont = list(size = 10, color = "black")), 
         yaxis12 = list(tickfont = list(size = 10, color = "black")), 
         yaxis13 = list(tickfont = list(size = 10, color = "black")), 
         yaxis14 = list(tickfont = list(size = 10, color = "black")), 
         yaxis15 = list(tickfont = list(size = 10, color = "black")), 
         xaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for x-axis text
         xaxis2 = list(tickfont = list(size = 10, color = "black")), 
         xaxis3 = list(tickfont = list(size = 10, color = "black")))

# View interactive plots
print(bird.total.abund.plotly)

# Save plot
saveWidget(bird.total.abund.plotly, file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_plotly.html", selfcontained = TRUE)

#-----------------------------------------------------------------------------#

# Create a smoothed trend line wrapped plot (by site) to visualize the total abundance (territories) for each site over time using the 'facet_wrap' function
bird.total.abund.smooth.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_abundance, group = sitecode)) +
  geom_smooth(method = "loess", size = 1, colour = "steelblue") +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +  # Separate plots per site
  labs(title = "Total breeding bird territories for monitoring sites over time
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

# Visualize the total abundance (territories) sampled over time for each site (site x year)
bird.total.abund.smooth.plot

# Save plot
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_smooth_plot.png"),
       plot = bird.total.abund.smooth.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#

# Visualize and explore plots with 'ggplotly' function
# Note these are imperfect visuals and lack many format customization options, but use for data exploration

# Convert ggplot to interactive plots
bird.total.abund.smooth.plotly <- ggplotly(bird.total.abund.smooth.plot) %>%
  layout(yaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for y-axis text
         yaxis2 = list(tickfont = list(size = 10, color = "black")), 
         yaxis3 = list(tickfont = list(size = 10, color = "black")), 
         yaxis4 = list(tickfont = list(size = 10, color = "black")), 
         yaxis5 = list(tickfont = list(size = 10, color = "black")), 
         yaxis6 = list(tickfont = list(size = 10, color = "black")), 
         yaxis7 = list(tickfont = list(size = 10, color = "black")), 
         yaxis8 = list(tickfont = list(size = 10, color = "black")), 
         yaxis9 = list(tickfont = list(size = 10, color = "black")), 
         yaxis10 = list(tickfont = list(size = 10, color = "black")), 
         yaxis11 = list(tickfont = list(size = 10, color = "black")), 
         yaxis12 = list(tickfont = list(size = 10, color = "black")), 
         yaxis13 = list(tickfont = list(size = 10, color = "black")), 
         yaxis14 = list(tickfont = list(size = 10, color = "black")), 
         yaxis15 = list(tickfont = list(size = 10, color = "black")), 
         xaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for x-axis text
         xaxis2 = list(tickfont = list(size = 10, color = "black")), 
         xaxis3 = list(tickfont = list(size = 10, color = "black")))

# View interactive plots
print(bird.total.abund.smooth.plotly)

#-----------------------------------------------------------------------------#

# Save plot
saveWidget(bird.total.abund.smooth.plotly, 
           file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_smooth_plotly.html",
           selfcontained = TRUE)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Create a smoothed trend line wrapped plot (by site) to visualize the total abundance (territories) for each site over time using the 'facet_wrap' function
bird.total.abund.smooth.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_abundance, group = sitecode)) +
  geom_smooth(method = "loess", size = 1, colour = "steelblue") +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
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
  facet_wrap(~ sitecode, ncol = 3, nrow = 5, scales = "free_y") +  # Separate plots per site
  labs(title = "Total breeding bird territories for monitoring sites over time
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year), max(bird.abund.data$year), by = 5), limits = c(1990, 2022), expand = c(0, 0))

# Visualize the total abundance (territories) sampled over time for each site (site x year)
bird.total.abund.smooth.plot

#-----------------------------------------------------------------------------#

# Optional step to save plot
# Save plot
ggsave(filename = paste0("D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_smooth_plot.png"),
       plot = bird.total.abund.smooth.plot, width = 8, height = 6)

#-----------------------------------------------------------------------------#

# Visualize and explore plots with 'ggplotly' function
# Note these are imperfect visuals and lack many format customization options, but use for data exploration

# Convert ggplot to interactive plots
bird.total.abund.smooth.plotly <- ggplotly(bird.total.abund.smooth.plot) %>%
  layout(yaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for y-axis text
         yaxis2 = list(tickfont = list(size = 10, color = "black")), 
         yaxis3 = list(tickfont = list(size = 10, color = "black")), 
         yaxis4 = list(tickfont = list(size = 10, color = "black")), 
         yaxis5 = list(tickfont = list(size = 10, color = "black")), 
         yaxis6 = list(tickfont = list(size = 10, color = "black")), 
         yaxis7 = list(tickfont = list(size = 10, color = "black")), 
         yaxis8 = list(tickfont = list(size = 10, color = "black")), 
         yaxis9 = list(tickfont = list(size = 10, color = "black")), 
         yaxis10 = list(tickfont = list(size = 10, color = "black")), 
         yaxis11 = list(tickfont = list(size = 10, color = "black")), 
         yaxis12 = list(tickfont = list(size = 10, color = "black")), 
         yaxis13 = list(tickfont = list(size = 10, color = "black")), 
         yaxis14 = list(tickfont = list(size = 10, color = "black")), 
         yaxis15 = list(tickfont = list(size = 10, color = "black")), 
         xaxis = list(tickfont = list(size = 10, color = "black")), # Tick label settings for x-axis text
         xaxis2 = list(tickfont = list(size = 10, color = "black")), 
         xaxis3 = list(tickfont = list(size = 10, color = "black")))

# View interactive plots
print(bird.total.abund.smooth.plotly)

#-----------------------------------------------------------------------------#

# Optional step to save plot
# Save plot
saveWidget(bird.total.abund.smooth.plotly, 
           file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_smooth_plotly.html",
           selfcontained = TRUE)
