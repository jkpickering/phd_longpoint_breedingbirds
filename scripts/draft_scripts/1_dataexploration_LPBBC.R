#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Title:          Data Exploration Basics - Meta Data / Data Structure
# Sub-title:      Long Point Breeding Bird Census (BBC) Project

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
# 4.2.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
  # "../results/plots/totals.hist.png"), # specify file location
  # plot = totals_hist, # specify plot to save
  # width = 8, # specify plot width
  # height = 6) # specify plot height

#-----------------------------------------------------------------------------#
# 4.3 Test for normality

# Shapiro-Wilk test for normality
shapiro.test(bird.total.abund$tot_territories) # run Shapiro-Wilk text function

# Shapiro-Wilk test p-value is < 0.05, suggests non-normal data

#-----------------------------------------------------------------------------#
# 4.4 Q-Q plot

# Assess normality further by employing a Q-Q (quantile-quantile) plot 
qqnorm(bird.total.abund$tot_territories) # plot territory data on a Q-Q plot
qqline(bird.total.abund$tot_territories, col = "red") # plot normal distribution

#-----------------------------------------------------------------------------#
# 6.2 Plot site and year summary data with facet_wrap function

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
# 6.2.1 Save plot for record (OPTIONAL)
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
# 6.3 Plot data in scatterplot and facet_wrap function

# Visualize trend of abundance (territories) over time (1991-2021)
# for all sites (15 total)

# Create a linear trend line wrapped plot (by site) to visualize the total abundance (territories) for each site over time using the 'facet_wrap' function
bird.total.abund.line.plot <- ggplot(bird.total.abund,
         aes(x = year,
             y = tot_territories,
             group = sitecode)) +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
  geom_smooth(method = "lm", colour = "steelblue") +
  geom_hline(yintercept = 0,
             color = "black") +  # Horizontal line
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
  labs(x = "Year",
       y = "Number of male breeding bird territories") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year),
                                  max(bird.abund.data$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

# Visualize
print(bird.total.abund.line.plot)

#-----------------------------------------------------------------------------#
# 6.3.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save ggplot file in results folder with 'ggsave' function
# ggsave(filename = paste0( # specify file name
# "../results/plots/lpbbc_territories_wrap_plot.png"), # specify file location
# plot = bird.total.abund.line.plot, # specify plot to save
# width = 8.18, # specify plot width
# height = 6.78) # specify plot height

#-----------------------------------------------------------------------------#
# 6.3.2 (OPTIONAL) Visualize and explore plots with 'ggplotly' function
# Remove written line function (i.e., '#') to run optional code

# Note these are imperfect visuals and lack many format customization options, 
# but can be useful for data exploration

# Convert ggplot to interactive plots
# bird.total.abund.plotly <- ggplotly(bird.total.abund.line.plot) %>%
  # layout(yaxis = list(tickfont = list(size = 10, # y-axis label text size
                                      # color = "black")), # y-axis label color
         # yaxis2 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis3 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis4 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis5 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis6 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis7 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis8 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis9 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis10 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis11 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis12 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis13 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis14 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # yaxis15 = list(tickfont = list(size = 10, # y-axis label text size
                                       # color = "black")), # y-axis label color
         # xaxis = list(tickfont = list(size = 10, # x-axis label text size
                                       # color = "black")), # x-axis label color
         # xaxis2 = list(tickfont = list(size = 10, # x-axis label text size
                                       # color = "black")), # x-axis label color
         # xaxis3 = list(tickfont = list(size = 10, # x-axis label text size
                                       # color = "black"))) # x-axis label color

# View interactive plots
# print(bird.total.abund.plotly)

#-----------------------------------------------------------------------------#
# 6.3.1.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# saveWidget(bird.total.abund.plotly,
           # file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_plotly.html",
           # selfcontained = TRUE)

#-----------------------------------------------------------------------------#
# 6.4 Plot data in scatterplot and facet_wrap function

# Visualize trend of abundance (territories) over time (1991-2021)
# for all sites (15 total) with a smoothed trend line wrapped plot (by site)
bird.total.abund.smooth.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_abundance, group = sitecode)) +
  geom_smooth(method = "loess", size = 1, colour = "steelblue") +
  geom_point(size = 1.2, colour = "steelblue") + # Add points for better visibility
  geom_hline(yintercept = 0,
             color = "black") +  # Horizontal line
  theme_pubr() +
  theme(legend.position = "none", # Removes the legend
        panel.spacing.x = unit(1.0, "cm"),
        strip.text.x = element_text(size = 8, face = "bold"),
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
  labs(x = "Year",
       y = "Number of male breeding bird territories") +
  scale_y_continuous(limits = c(NA, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year),
                                  max(bird.abund.data$year), 
                                  by = 5), 
                     limits = c(1990, 2022), 
                     expand = c(0, 0))

# Visualize the total abundance (territories) sampled over time for each site (site x year)
bird.total.abund.smooth.plot

#-----------------------------------------------------------------------------#
# 6.4.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# ggsave(filename = paste0(
  # "../results/plots/bird_total_abund_smooth_plot.png"),
  # plot = bird.total.abund.smooth.plot,
  # width = 8,
  # height = 6)

#-----------------------------------------------------------------------------#
# 6.4.2 (OPTIONAL) Visualize and explore plots with 'ggplotly' function
# Remove written line function (i.e., '#') to run optional code

# Note these are imperfect visuals and lack many format customization options, 
# but can be useful for data exploration

# Convert ggplot to interactive plots
# bird.total.abund.smooth.plotly <- ggplotly(bird.total.abund.line.plot) %>%
  # layout(yaxis = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis2 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis3 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis4 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis5 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis6 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis7 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis8 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis9 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis10 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis11 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis12 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis13 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis14 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # yaxis15 = list(tickfont = list(size = 10, # y-axis label text size
                                    # color = "black")), # y-axis label color
        # xaxis = list(tickfont = list(size = 10, # x-axis label text size
                                    # color = "black")), # x-axis label color
        # xaxis2 = list(tickfont = list(size = 10, # x-axis label text size
                                    # color = "black")), # x-axis label color
        # xaxis3 = list(tickfont = list(size = 10, # x-axis label text size
                                    # color = "black"))) # x-axis label color

#-----------------------------------------------------------------------------#
# 6.4.1.1 Save plot for record (OPTIONAL)
# Remove written line function (i.e., '#') to run optional code

# Save plot
# saveWidget(bird.total.abund.smooth.plotly, 
           # file = "D:/r_projects/phd_longpoint_breedingbirds/results/plots/bird_total_abund_smooth_plotly.html",
           # selfcontained = TRUE)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 7.0 Breeding bird species richness: data visualizations

#-----------------------------------------------------------------------------#
# 7.1 Plot the species richness for sites over time

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
print(species.richness.site.plot)

#-----------------------------------------------------------------------------#
# 7.2 Plot the species richness for sites over time using facet_wrap

# Create a smoothed trend line wrapped plot (by site) to visualize the species richness for each site over time using the 'facet_wrap' function
species.richness.site.year.plot <- bird.abund.data %>%
  group_by(sitecode, year) %>%
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
  scale_x_continuous(breaks = seq(min(bird.abund.data$year),
                                  max(bird.abund.data$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

# Visualize
print(species.richness.site.year.plot)

#-----------------------------------------------------------------------------#
# 7.2.1 Save plot for record (OPTIONAL)

# Save plot
ggsave(filename = paste0("../results/plots/lpbbc_richness_wrap_line_plot.png"),
       plot = species.richness.site.year.plot, 
       width = 8.18, 
       height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 8.0 Breeding bird species diversity (Shannon Index): data visualizations

#-----------------------------------------------------------------------------#
# 8.1 Plot the species diversity for sites

# Create a site-species matrix (presence-absence)
species.abundance.matrix <- bird.abund.data %>%
  group_by(year, sitecode, species_4code_IBP) %>%
  summarise(total_territories = sum(territories), .groups = "drop") %>%
  spread(species_4code_IBP, total_territories, fill = 0)

# Compute the Shannon index per site and time
species.abundance.matrix$diversity <- diversity(species.abundance.matrix[,3:ncol(species.abundance.matrix)],
                                                index = "shannon")

# Identify the average species diversity (Shannon Index) for each site
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

#-----------------------------------------------------------------------------#
# 8.2 Plot the species diversity over time for each site with facet_wrap
species.diversity.site.year.plot <- ggplot(species.abundance.matrix,
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
             scales = "fixed") +
  labs(x = "Year",
       y = "Species diversity (Shannon index)") +
  scale_y_continuous(limits = c(NA, 3.5),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(breaks = seq(min(bird.abund.data$year),
                                  max(bird.abund.data$year),
                                  by = 5),
                     limits = c(1990, 2022),
                     expand = c(0, 0))

#Visualize
print(species.diversity.site.year.plot)

#-----------------------------------------------------------------------------#
# 8.2.1 Save plot for record (OPTIONAL)

# Save plot
ggsave(filename = paste0("../results/plots/lpbbc_diversity_wrap_plot.png"),
       plot = species.diversity.site.year.plot, 
       width = 8.18, 
       height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 9.0 Breeding bird species plots: data visualizations

#-----------------------------------------------------------------------------#
# 9.1 Plot individual species trends over time, for individual sites

# Get list of species to filter data by species
species_list <- unique(territoryguild.data$species_4code_IBP)  

# Loop through each unique species and create separate plots
for (sp in species_list) {
  # Filter data for the specific species
  bird.species.territories.plot  <- ggplot(filter(territoryguild.data,
                                                  species_4code_IBP == sp),
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
    labs(title = paste("Territories of", sp, 
                       "over time at Long Point, Ontario, Canada, 1991-2021"),
         x = "Year",
         y = "Number of territories") +
    xlim(1991, 2021) # Set x-limit minimum and maximum
  
  # Print plot in the R console
  print(bird.species.territories.plot)
}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 10.0 Breeding bird nesting guild plots: data visualizations

#-----------------------------------------------------------------------------#
# 10.1 Plot nesting guild trends over time, for individual sites

# Import data from csv file, saved in the 'data' sub-folder of the rproject
speciesdescription.data <- read.csv("../data/bird_spcharacteristics.csv", header = T)

# Identify summary information about the data (e.g., mean, median, min and max)
summary(speciesdescription.data) # Summarize key information within the dataset

view(speciesdescription.data)

# Join species information data "speciesdescription.data" with territory data for analyses
territoryguild.data <- full_join(bird.abund.data,
                                 speciesdescription.data,
                                 by = c("species_4code_IBP"))

# Review column names for reference in analyses
names(territoryguild.data) # Identify column names within the dataset

# Summarize: total territories per site, year, and nesting guild
total_territories <- territoryguild.data %>%
  group_by(nest_guild_BOTW, sitecode, year) %>%
  summarise(total_territories = sum(territories, 
                                    na.rm = TRUE), 
            .groups = "drop")

# Get list of guilds (not species)
nest_guild_list <- unique(total_territories$nest_guild_BOTW)

# Loop through each nest guild
for (guild in nest_guild_list) {
  
  plot_data <- filter(total_territories, 
                      nest_guild_BOTW == guild)
  
  total_territories_plot <- ggplot(plot_data,
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
                                    face = "bold",
                                    hjust = 0.5),
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
    labs(x = "Year",
         y = paste("Number of male breeding bird territories", guild, "nest guild")) +
    scale_y_continuous(limits = c(NA, NA),
                       expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
    scale_x_continuous(breaks = seq(min(plot_data$year),
                                    max(plot_data$year),
                                    by = 5),
                       limits = c(1990, 2022),
                       expand = c(0, 0))
  print(total_territories_plot)
}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 11.0 Bird species accumulation curves: data visualizations

#-----------------------------------------------------------------------------#
# 11.1 Plot nesting guild trends over time, for individual sites

# Complete a species accumulation curve for years using the 'speaccum' function
sac.year.matrix <- bird.abund.data %>%
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
site_year_species <- bird.abund.data %>%
  group_by(sitecode, year, species_4code_IBP) %>%
  summarize(present = any(territories > 0), .groups = "drop") %>%
  filter(present)  # keep only rows where species was present

# Accumulate species over time per site
sac_time <- site_year_species %>%
  arrange(sitecode, year) %>%
  group_by(sitecode) %>%
  mutate(
    year = as.integer(year),
    species_seen = map2(year, sitecode, ~ {
      species_so_far <- site_year_species %>%
        filter(sitecode == .y, year <= .x) %>%
        pull(species_4code_IBP) %>%
        unique()
      length(species_so_far)
    })
  ) %>%
  unnest(species_seen) %>%
  distinct(sitecode, year, .keep_all = TRUE)

# Plot
sac_wrapped <- ggplot(sac_time, aes(x = year,
                     y = species_seen,
                     group = sitecode)) +
  geom_smooth(method = "loess",
              color = "steelblue",
              size = 1) +
  geom_point(color = "steelblue",
             size = 2) +
  geom_hline(yintercept = 0, 
             color = "black") +  # Horizontal line
  facet_wrap(~sitecode,
             scales = "free_y") +
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
  labs(x = "Year",
       y = "Number of species") +
  scale_y_continuous(limits = c(0, NA),
                     expand = c(0, 0)) +  # Set fixed y-axis limits, remove y-axis limits to allow 'free_y' function in 'facet_wrap' function to produce varying y-axis scales
  scale_x_continuous(limits = c(NA, NA),
                     expand = c(0, 0))

print(sac_wrapped)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 12.0 Exploring dominant and rare species

#-----------------------------------------------------------------------------#
# 12.1 Plot dominant species

# Summarize total abundance per species across all years and sites
top_species <- bird.abund.data %>%
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
site_species_totals <- bird.abund.data %>%
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
species_turnover <- bird.abund.data %>%
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

row_ann <- data.frame(Guild = territoryguild.data$nest_guild_BOTW)
rownames(row_ann) <- territoryguild.data$species_4code_IBP

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
presence_df <- bird.abund.data %>%
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
community_data_subset <- bird.abund.data %>%
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
community_data_subset <- bird.abund.data %>%
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
community_data_subset <- bird.abund.data %>%
  filter(year %in% c(1991, 2016, 2021)) %>%
  group_by(species_4code_IBP) %>%
  filter(sum(territories, na.rm = TRUE) >= 1.0) %>%
  ungroup()

# Step 2: Create site-year-species matrix (allows per-site NMDS)
community_matrix <- community_data_subset %>%
  group_by(sitecode, year, species_4code_IBP) %>%
  summarise(total_abundance = sum(territories, na.rm = TRUE), .groups = "drop") %>%
  unite("site_year", sitecode, year, remove = FALSE) %>%
  pivot_wider(names_from = species_4code_IBP, values_from = total_abundance, values_fill = 0)

# Step 3: Extract metadata
meta_data <- community_matrix %>% select(sitecode, year, site_year)
comm_matrix <- community_matrix %>% select(-sitecode, -year, -site_year)

# Step 4: Run NMDS with Bray-Curtis dissimilarity
set.seed(123)
nmds_result <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)

# Step 5: Extract NMDS coordinates
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$site_year <- meta_data$site_year
site_scores$year <- meta_data$year
site_scores$sitecode <- meta_data$sitecode

# Step 6: Extract species scores and calculate vector lengths
species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$species <- rownames(species_scores)
species_scores$vector_length <- sqrt(species_scores$NMDS1^2 + species_scores$NMDS2^2)

# Step 7: Select top N contributing species
top_species <- species_scores %>%
  arrange(desc(vector_length)) %>%
  slice_head(n = 15)  # Top 15 species

# Step 8: Create convex hulls per year
hull_data <- site_scores %>%
  group_by(year) %>%
  slice(chull(NMDS1, NMDS2))

# Ensure 'year' is treated as a discrete factor
site_scores$year <- as.factor(site_scores$year)
hull_data$year <- as.factor(hull_data$year)


# Step 9: Plot NMDS with polygons, sites, arrows, and top species labels
ggplot() +
  # Polygons for each year's community spread
  geom_polygon(data = hull_data,
               aes(x = NMDS1, y = NMDS2, fill = year, group = year),
               alpha = 0.2, color = "black", linewidth = 0.5) +
  
  # Site-level NMDS points
  geom_point(data = site_scores,
             aes(x = NMDS1, y = NMDS2, color = year),
             size = 2, alpha = 0.8) +
  
  # Arrows for species vectors
  geom_segment(data = top_species,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "gray30", linewidth = 0.6) +
  
  # Labels for top species
  geom_text_repel(data = top_species,
                  aes(x = NMDS1, y = NMDS2, label = species),
                  size = 3, color = "black",
                  segment.color = "gray50", max.overlaps = 50) +
  
  scale_color_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  scale_fill_manual(values = c("1991" = "#1b9e77", "2016" = "#d95f02", "2021" = "#7570b3")) +
  
  labs(
    title = "NMDS of Bird Community Composition (1991, 2016, 2021)",
    subtitle = "Top 15 contributing species shown as arrows\nPolygons represent community spread per year",
    x = "NMDS Axis 1",
    y = "NMDS Axis 2",
    color = "Year",
    fill = "Year"
  ) +
  theme_pubr() +
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
species.diversity.matrix <- bird.abund.data %>%
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

total_abund <- bird.abund.data %>%
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
# 15.2.1 Save plot for record (OPTIONAL)

# Save plot
ggsave(filename = paste0("../results/plots/lpbbc_GAM_abundance_wrap_plot.png"),
       plot = gam_model, 
       width = 8.18, 
       height = 6.78)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# 16.0 GAMs: Exploring non-linear trends in abundance

#-----------------------------------------------------------------------------#
# 16.1 Individual species trends by year-site

# Filter for species with >5 records
species_filtered <- bird.abund.data %>%
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

# Import data from csv file within subfolder 'data'
site.data <- read.csv("../data/site_characteristics_dd.csv", # sub-folder select
                      header = T) # confirm columns have headers

# Check for missing values within the data
any(is.na(site.data)) # identify if any data are 'n.a.' or missing

# Identify summary information about the data (e.g., mean, median, min and max)
summary(site.data) # Summarize key information within the dataset

# Remove written line function(#) to run optional code
# view(site.data)

# Join site information data with breeding bird territory data
site.abund.data <- full_join(bird.abund.data,
                             site.data,
                             by = c("sitecode"))

# Remove written line function(#) to run optional code
# view(site.abund.data)

# Review column names for reference in analyses
names(site.abund.data) # Identify column names within the dataset







































