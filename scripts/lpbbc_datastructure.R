#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Long Point Breeding Bird Census (BBC) Project
# Sub-title:      Exploring Meta Data & Data Structure

# Author:         Joshua K. Pickering, PhD Candidate
# Affiliation:    University of Waterloo, Birds Canada
# Creation Date:  2024-06-18
# Last Updated:   2025-07-23

# Description:    This script includes results of exploratory summary data and 
#                 visualizations for breeding bird census territory data
#                 collected at Long Point, Ontario, Canada from 1991 - 2021

# Data Sources:   "../data/bird_spterritories_abund.csv"

# R Version:      4.4.2 (2024-10-31 ucrt)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 1.0 Script setup

#-----------------------------------------------------------------------------#
# 1.1 Check for updates for R and R Studio

install.packages("installr")  # Only for Windows
library(installr)
updateR()


R.version # check R version

#-----------------------------------------------------------------------------#
# 1.2 Clear all objects from the R Environment prior to data exploration

# Remove all existing data from the Environment with 'rm' function
rm(list = ls())  # remove all objects

# Identify the working directory for analyses
getwd() # view working directory filepath

#-----------------------------------------------------------------------------#
# 1.3 Identify and install necessary R packages

# To reduce unnecessary loading, run 'options' function
options(warn = -1)  # suppress warnings if packages are already loaded

# List required packages (sort alphabetically for ease of use/revision)
list.of.packages <- c("broom",
                      "dplyr",
                      "forcats",
                      "ggplot2",
                      "ggpubr",
                      "ggrepel",
                      "grid",
                      "htmlwidgets",
                      "lme4",
                      "mgcv",
                      "patchwork",
                      "pheatmap",
                      "plotly",
                      "purrr",
                      "stringr",
                      "tibble",
                      "tidyr",
                      "tidytext",
                      "tidyverse",
                      "vegan")

# Create function to install packages if they are missing from the library
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# Install missing packages
lapply(list.of.packages, install_if_missing)

# Load required packages
lapply(list.of.packages, library, character.only = TRUE) 

# Update packages if needed

# To check for available package updates
old.packages()

# To update all out-of-date packages
update.packages(ask = FALSE, checkBuilt = TRUE)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 2.0 Import required data files (csv format required)

# Import data from csv file
bird.abund.data <- read.csv("../data/bird_spterritories_abund.csv") # sub-folder

# Check for missing values within the data
any(is.na(bird.abund.data)) # identify if any data are 'n.a.' or missing

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 3.0 Explore data structure with summaries

#-----------------------------------------------------------------------------#
# 3.1 Identify the data structure and summary information

# View structure of the data
glimpse(bird.abund.data) # identifies structure of columns and extent of data
# can also use 'str' function instead of 'glimpse' function for similar results

# Identify summary information about the data (e.g., mean, median, min and max)
summary(bird.abund.data) # view summary data

# Note column names for considerations of further data exploration
colnames(bird.abund.data) # identify column names

#-----------------------------------------------------------------------------#
# 3.2 View raw data frame

# View data table
view(bird.abund.data) # calls data frame for viewing in source editor window

#-----------------------------------------------------------------------------#
# 3.3 Identify data totals and data independence

# Check unique counts for identified variables
bird.abund.data %>% # link data to the 'summarise' function
  summarise( # identifies count for each specified column
    Total_species_samples = n_distinct(uniqueID), # count of total samples
    Total_sites_sampled_over_time = n_distinct(sample), # count of site samples
    Total_years_sampled = n_distinct(year), # count of years sampled
    Unique_sites = n_distinct(sitecode), # unique number of sites
    Unique_species = n_distinct(species_4code_IBP)) # unique number of species

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 4.0 Summarize sampling effort

#-----------------------------------------------------------------------------#
# 4.1 Identify number of sampled sites at Long Point, Ontario, Canada

# Summarize the total number of sites sampled for each year over time
sites.per.year <- bird.abund.data %>% # link data to summary functions
  group_by(year) %>% # link data years to summary function
  summarise(sites_sampled = n_distinct(sitecode), # create list of unique sites
            .groups = "drop") # manipulate data grouping for years without site

print(sites.per.year, # view summary of site samples over time
      n = Inf) # view all results of tibble

#-----------------------------------------------------------------------------#
# 4.2 Plot sampling effort over time with bar chart

# Plot the total number of sites sampled over time using 'ggplot' function
sampling.effort.barplot <- ggplot(sites.per.year, # specify site data
                              aes(x = year, # specify year as x-value
                                  y = sites_sampled)) + # specify sites y-value
  geom_bar(stat = "identity", # create bar chart values
           fill = "steelblue") +  # customize colour of bars
  theme_pubr() + # specify publication ready theme for chart
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12, # specify text size
                                  face = "bold", # specify text format
                                  hjust = 0.5, # specify justification as center
                                  color = "black", # specify text colour
                                  margin = margin(b = 15)), # specify margin
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # margin
        axis.title.y.left = element_text(size = 12, # specify text size
                                         face = "bold", # specify text format
                                         colour = "black", # specify text colour
                                         margin = margin(r = 15)), # margin
        axis.text.x = element_text(size = 8, # specify text size
                                   color = "black", # specify text colour
                                   angle = 45, # specify text angle
                                   hjust = 0.5, # specify horizontal spacing
                                   vjust = 0.5), # specify vertical spacing
        axis.text.y.left = element_text(size = 8, # specify text size
                                        color = "black")) + # specify txt colour
  labs(title = "Number of breeding bird census sites sampled per year
       at Long Point, Ontario, Canada, 1991-2021", # specify title label
       x = "Year", # specify x-axis label
       y = "Number of sites sampled") + # specify y-axis label
  scale_x_continuous(breaks = seq(min(sites.per.year$year), # x-axis min limits
                                  max(sites.per.year$year), # x-axis max limits
                                  by = 1), # x-axis spacing
                     expand = c(0,0.5)) + # x-axis spacing
  scale_y_continuous(breaks = seq(min(0), # y-axis min limits
                                  max(sites.per.year$sites_sampled), # max limit
                                  by = 1), # y-axis spacing
                     expand = c(0,0)) # y-axis spacing

# Visualize the total number of sites sampled over time
print(sampling.effort.barplot)

#-----------------------------------------------------------------------------#
# 4.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/sampling_effort_barplot.png"), # specify file location
  # plot = sampling.effort.barplot, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 4.3 Plot sampling effort over time with heat map

# Plot the total number of sites sampled over time using 'ggplot' function
sampling.effort.heatmap <- ggplot(sites.per.year, 
                                  aes(x = year,
                                      y = sites_sampled)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("TRUE" = "steelblue",
                               "FALSE" = "grey90")) +
  theme_pubr() + # specify publication ready theme for chart
  theme(legend.position = "none", # Removes the legend
        plot.title = element_text(size = 12, # specify text size
                                  face = "bold", # specify text format
                                  hjust = 0.5, # specify justification as center
                                  color = "black", # specify text colour
                                  margin = margin(b = 15)), # specify margin
        axis.title.x.bottom = element_text(size = 12,
                                           face = "bold",
                                           colour = "black",
                                           margin = margin(t = 15)), # margin
        axis.title.y.left = element_text(size = 12, # specify text size
                                         face = "bold", # specify text format
                                         colour = "black", # specify text colour
                                         margin = margin(r = 15)), # margin
        axis.text.x = element_text(size = 8, # specify text size
                                   color = "black", # specify text colour
                                   angle = 45, # specify text angle
                                   hjust = 0.5, # specify horizontal spacing
                                   vjust = 0.5), # specify vertical spacing
        axis.text.y.left = element_text(size = 8, # specify text size
                                        color = "black")) + # specify txt colour
  labs(title = "Number of breeding bird census sites sampled per year
       at Long Point, Ontario, Canada, 1991-2021", # specify title label
       x = "Year", # specify x-axis label
       y = "Number of sites sampled") + # specify y-axis label
  scale_x_continuous(breaks = seq(min(sites.per.year$year), # x-axis min limits
                                  max(sites.per.year$year), # x-axis max limits
                                  by = 1), # x-axis spacing
                     expand = c(0,0.5)) + # x-axis spacing
  scale_y_continuous(breaks = seq(min(0), # y-axis min limits
                                  max(sites.per.year$sites_sampled), # max limit
                                  by = 1), # y-axis spacing
                     expand = c(0,1.5)) # y-axis spacing

# Visualize the total number of sites sampled over time
print(sampling.effort.heatmap)

#-----------------------------------------------------------------------------#
# 4.3.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/sampling_effort_heatmap.png"), # specify file location
  # plot = sampling.effort.heatmap, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 4.4 Plot sampling effort over time with circular bar plot

# Count number of years sampled per site
years.per.site <- bird.abund.data %>% # link data to summary functions
  group_by(sitecode) %>% # link data to summary functions
  summarise(years_sampled = n_distinct(year)) %>% # link summary data
  arrange(desc(years_sampled)) # organize plot order

# Print the count
print(years.per.site)

# Visualization of number of years sampled per site
sampling.effort.circularbar <- ggplot(years.per.site, # specify data
       aes(x = sitecode, # identify x-variable
           y = years_sampled)) + # identify y-variable
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:15)),
    color = "lightgrey") + 
  geom_bar(stat = "identity", # specify bar plot
           show.legend = FALSE, # specify no legend
           width = 0.9, # specify bar width
           fill = "steelblue") + # specify bar colour
  coord_polar(start = 0) +  # convert bars to circular bar plot format
  geom_text(aes(label = years_sampled, # specify data labels
                y = years_sampled / 2), # specify y-variable
            color = "white", # specify text colour
            size = 5) + # specify text size
  theme_pubr() +
  theme(
    plot.title = element_text(size = 14,
                              face = "bold",
                              color = "black",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12,
                               vjust = 0.5,
                               hjust = 0.5),
    axis.line = element_blank(),
    axis.text.y = element_blank(), # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    panel.grid.major.y = element_blank(),) + # Remove y-axis grid lines
  labs(
    title = "\nSampling effort per site", # specify title label
    subtitle = paste(
      "\nThis visualisation shows the frequency of sampling at each of the,",
      "fifteen monitoring sites at Long Point, Ontario, Canada\n",
      sep = "\n"
    ),
    caption = paste("\n\nData Visualisation by Joshua K. Pickering\n",
    "Source: Birds Canada, Canadian Wildlife Service\n",
    "Link to Data: github.com/",
    sep = "\n"
    ))

print(sampling.effort.circularbar)

#-----------------------------------------------------------------------------#
# 4.4.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/sampling_effort_circularbar.png"), # specify file location
  # plot = sampling.effort.circularbar, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 5.0 Dependent data visualizations

#-----------------------------------------------------------------------------#
# 5.1 Plot dependent variable data as histogram

# Histogram check for normality using 'ggplot' function
bird.abund.hist <- ggplot(bird.abund.data,
       aes(x = territories)) + # ggplot function
  geom_histogram(binwidth = 1, # identify width of bins for histogram
                 fill = "steelblue", # specify bar colour
                 colour = "black", # specify bar outline
                 alpha = 1) + # specify bar transparency
  theme_pubr() + # format ggplot with 'publication ready' theme
  theme(legend.position = "none", # remove legend
        plot.title = element_text(size = 12, # specify text size
                                  face = "bold", # specify text style
                                  hjust = 0.5, # specify text location
                                  color = "black", # specify text colour
                                  margin = margin(b = 15)), # specify margin
        axis.title.x.bottom = element_text(size = 12, # specify text size
                                           face = "bold", # specify text style
                                           hjust = 0.5, # specify text location
                                           color = "black"), # specify margin
        axis.title.y.left = element_text(size = 12, # specify text size
                                         face = "bold", # specify text style
                                         hjust = 0.5, # specify text location
                                         vjust = 1.0, # specify text location
                                         color = "black"), # specify colour
        axis.text.x = element_text(size = 8, # specify text size
                                   color = "black"), # specify margin
        axis.text.y.left = element_text(size = 8, # specify text size
                                        color = "black")) + # specify margin
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021", # title label for plot 
       x = "Number of territories", # x-axis label for plot
       y = "Sample frequency") + # y-axis label for plot
  scale_y_continuous(expand = c(0, 0)) +  # set y-axis limits
  scale_x_continuous(expand = c(0, 0.5)) # set x-axis limits

print(bird.abund.hist)

# Histogram shows a right tail, but fairly normal distribution

#-----------------------------------------------------------------------------#
# 5.2 Plot grouped data in boxplot

# Explore boxplot using 'ggplot' function
bird.abund.boxplot <- ggplot(bird.abund.data,
                          aes(x = sitecode,
                              y = territories)) + # ggplot function
  geom_boxplot() +
  theme_pubr() + # format ggplot with 'publication ready' theme
  theme(legend.position = "none", # remove legend
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
  labs(x = " ", # x-axis label for plot
       y = "Number of male breeding bird territories") # y-axis label

print(bird.abund.boxplot) # view plot

#-----------------------------------------------------------------------------#
# 5.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/bird_abund_boxplot.png"), # specify file location
  # plot = bird.abund.boxplot, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 5.2.2 Plot grouped data in boxplot with facet_wrap function

all_years <- as.character(seq(1991, 2021, by = 1))  # all years as characters

bird.abund.data$year <- factor(bird.abund.data$year, levels = all_years)

# Get all site-year combinations
site_year_grid <- expand.grid(
  sitecode = unique(bird.abund.data$sitecode),
  year = all_years
)

# Merge with your real data
bird.abund.full <- merge(site_year_grid, bird.abund.data, all.x = TRUE)


# Explore boxplot using 'ggplot' function
bird.abund.boxplot.wrapped <- ggplot(bird.abund.full, aes(x = year, y = territories)) +
  geom_boxplot(color = "grey20", 
               na.rm = TRUE, 
               outlier.size = 1, 
               outlier.alpha = 1) + # Add points for better visibility
  geom_hline(yintercept = 0,
             color = "black") +  # Horizontal line +
  facet_wrap(~ sitecode, scales = "free_y", ncol = 3, nrow = 5) +
  scale_x_discrete(
    breaks = c("1991", "1996", "2001", "2006", "2011", "2016", "2021"),
    expand = c(0, 0)
  ) +
  theme_pubr() +
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
  labs(x = "Year", 
       y = "Number of male breeding bird territories")

print(bird.abund.boxplot.wrapped) # view plot

#-----------------------------------------------------------------------------#
# 5.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/bird_abund_boxplot_wrapped.png"), # specify file location
  # plot = bird.abund.boxplot.wrapped, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 5.3 Plot data in scatterplot

# Explore scatterplot using 'ggplot' function
bird.abund.scatterplot <- ggplot(bird.abund.data, 
                                 aes(x = year,
                                     y = territories)) +
  geom_point() +
  theme_pubr() + # format ggplot with 'publication ready' theme
  theme(legend.position = "none", # remove legend
        plot.title = element_text(size = 12, # specify text size
                                  face = "bold", # specify text style
                                  hjust = 0.5, # specify text location
                                  color = "black", # specify text colour
                                  margin = margin(b = 15)), # specify margin
        axis.title.x.bottom = element_text(size = 12, # specify text size
                                           face = "bold", # specify text style
                                           hjust = 0.5, # specify text location
                                           vjust = -1.8, # specify text location
                                           color = "black"), # specify margin
        axis.title.y.left = element_text(size = 12, # specify text size
                                         face = "bold", # specify text style
                                         hjust = 0.5, # specify text location
                                         vjust = 2.8, # specify text location
                                         color = "black"), # specify colour
        axis.text.x = element_text(size = 8, # specify text size
                                   color = "black"), # specify margin
        axis.text.y.left = element_text(size = 8, # specify text size
                                        color = "black")) + # specify margin
  labs(title = "Scatterplot for breeding bird territories sampled
       at Long Point, Ontario, Canada, 1991-2021", # title label for plot 
       x = "Year", # x-axis label for plot
       y = "Breeding male abundance (number of territories)") + # y-axis label
  scale_x_continuous(breaks = seq(min(1991), # x-axis min limits
                                  max(2021), # x-axis max limits
                                  by = 3), # x-axis spacing
                     expand = c(0,1.0)) + # x-axis spacing
  scale_y_continuous(limits = c(0, 50), # set limits of data
                     breaks = seq(min(0), # y-axis min limits
                                  max(50), # max limit
                                  by = 5), # y-axis spacing
                     expand = c(0,0.5)) # y-axis spacing

print(bird.abund.scatterplot) # view plot

#-----------------------------------------------------------------------------#
# 5.3.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/bird_abund_scatterplot.png"), # specify file location
  # plot = bird.abund.scatterplot, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 5.4 Plot data in scatterplot and facet_wrap function

# Visualize trend of abundance (territories) over time (1991-2021) for all sites (15 total)

# Create a wrapped plot (by site) to visualize the abundance of all sampled species over time using the 'facet_wrap' function
bird.abund.scatrplot <- ggplot(bird.abund.data, aes(x = year, y = territories, color = species_4code_IBP)) +
  geom_boxplot(outlier.size = 0.8, 
               outlier.alpha = 0.5) + # Add points for better visibility
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
                                           face = "bold", colour = "black", margin = margin(t = 15)), # Formats x-axis title
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
             scales = "free_y") +  # Separate plots per site, and format for grid layout (i.e., 3 subplots x 5 subplots)
  labs(title = "Breeding bird territories of all sampled species for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Year",
       y = "Number of territories") +
  scale_y_continuous(limits = c(0, NA), 
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(1991, NA), 
                     expand = c(0, 0))

# Visualize all breeding bird census species' territories sampled over time (1991-2021) for each site (15 total)
print(bird.abund.scatrplot)

#-----------------------------------------------------------------------------#
# 5.4.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/bird_abund_scatrplot.png"), # specify file location
  # plot = bird.abund.scatrplot, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 6.0 Breeding bird abundance totals: exploration and data visualizations

#-----------------------------------------------------------------------------#
# 6.1 Plot site and year summary data

# Organize bird territory data for site and year with 'summarise' function
bird.total.abund <- bird.abund.data %>% # link new data label to next line
  group_by(sitecode, year) %>% # group territories by site and year, and link
  summarise(tot_territories = sum(territories, na.rm = TRUE)) # sum territories

# Review and control for potential missing data
bird.total.abund <- na.omit(bird.total.abund) # remove any rows or columns containing zeros
sum(is.na(bird.total.abund)) # checks that no rows or columns contain zero values

# Histogram check for normality using 'ggplot' function
totals_hist <- ggplot(bird.total.abund,
                      aes(x = tot_territories)) + # ggplot function
  geom_histogram(binwidth = 10, # identify width of bins for histogram
                 fill = "steelblue", # specify bar colour
                 colour = "black", # specify bar outline
                 alpha = 1) + # specify bar transparency
  theme_pubr() + # format ggplot with 'publication ready' theme
  theme(legend.position = "none", # remove legend
        plot.title = element_text(size = 12, # specify text size
                                  face = "bold", # specify text style
                                  hjust = 0.5, # specify text location
                                  color = "black", # specify text colour
                                  margin = margin(b = 15)), # specify margin
        axis.title.x.bottom = element_text(size = 12, # specify text size
                                           face = "bold", # specify text style
                                           hjust = 0.5, # specify text location
                                           color = "black"), # specify margin
        axis.title.y.left = element_text(size = 12, # specify text size
                                         face = "bold", # specify text style
                                         hjust = 0.5, # specify text location
                                         vjust = 1.0, # specify text location
                                         color = "black"), # specify colour
        axis.text.x = element_text(size = 8, # specify text size
                                   color = "black"), # specify margin
        axis.text.y.left = element_text(size = 8, # specify text size
                                        color = "black")) + # specify margin
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021", # title label for plot 
       x = "Number of territories", # x-axis label for plot
       y = "Sample frequency") + # y-axis label for plot
  scale_y_continuous(limits = c(0, 15), # set y-axis limits
                     expand = c(0, 0)) +  # set fixed limits
  scale_x_continuous(limits = c(0, 250), # set x-axis limits
                     expand = c(0, 0)) # set fixed limits

print(totals_hist)

# Histogram shows a right tail, but fairly normal distribution

#-----------------------------------------------------------------------------#
# 6.1.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
# "../results/plots/totals.hist.png"), # specify file location
# plot = totals_hist, # specify plot to save
# width = 8, # specify plot width
# height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 6.2 Test for normality

# Shapiro-Wilk test for normality
shapiro.test(bird.total.abund$tot_territories) # run Shapiro-Wilk text function

# Shapiro-Wilk test p-value is < 0.05, suggests non-normal data

#-----------------------------------------------------------------------------#
# 6.3 Q-Q plot

# Assess normality further by employing a Q-Q (quantile-quantile) plot 
qqnorm(bird.total.abund$tot_territories) # plot territory data on a Q-Q plot
qqline(bird.total.abund$tot_territories, col = "red") # plot normal distribution

#-----------------------------------------------------------------------------#
# 6.4 Plot site and year summary data with facet_wrap function

# Histogram check for normality using 'ggplot' function and 'geom_histogram' function for each site sample (sitecode x year)
wrapped_total_plot <- ggplot(bird.total.abund,
                             aes(x = tot_territories)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
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
             scales = "free") +
  labs(title = "Histogram for breeding bird territories for monitoring sites
       at Long Point, Ontario, Canada, 1991-2021",
       x = "Total number of territories",
       y = "Frequency") +
  scale_y_continuous(limits = c(0, 5),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(0, 250),
                     expand = c(0, 0.5))
# view
print(wrapped_total_plot)

# Histograms show normal data distribution

#-----------------------------------------------------------------------------#
# 6.4.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
# "../results/plots/wrapped.total.plot.png"), # specify file location
# plot = wrapped_total_plot, # specify plot to save
# width = 8, # specify plot width
# height = 6) # specify plot height

#-----------------------------------------------------------------------------#

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
#-----------------------------------------------------------------------------#

# END of script

