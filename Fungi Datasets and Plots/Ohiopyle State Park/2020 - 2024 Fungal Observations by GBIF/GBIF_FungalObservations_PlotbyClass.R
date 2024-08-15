# Introduction ----

# Calculating species richness in Ohiopyle State Park 
# Data from 2020 - August 13, 2024, sourced from GBIF
# GBIF.org (13 August 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.469ntm


# Hailey Wittkopp for MycroMaps project
# 08/14/2024


# Libraries ----
# Install needed libraries
install.packages("dplyr") # Species richness calculation
install.packages("vegan") # Further species richness
install.packages("ggplot2") # Viusalize and Plot data
install.packages("extrafont")

library(dplyr)
library(vegan)
library(ggplot2)
library(extrafont)

# Load Fonts into R
loadfonts()

# Load Data ----

# Set the working directory
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF")

# Load the .csv file
OhiopyleData <- read.csv("GBIF_FungalObservations_Cleaned.csv")

# Analyze Data ----
# Group data by family and summarize species richness
species_richness_by_class <- OhiopyleData %>%
  group_by(class) %>%
  summarise(richness = n_distinct(species))

print(species_richness_by_class)

# Plot Data ----

# Calculate Y-axis breaks
max_count <- max(species_richness_by_class$richness)
y_breaks <- seq(0, ceiling(max_count / 2) * 2, by = 10)  

fungi_colors_vibrant <- c(
  "#FF5722", "#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", 
  "#2196F3", "#03A9F4", "#4CAF50", "#8BC34A", "#FFC107", "#FFEB3B", "#00BCD4"
)

# Define text sizing
axis_text_size <- 12
axis_title_size <- 14
title_size <- 16
legend_title_size <- 12
legend_text_size <- 10

ggplot(species_richness_by_class, aes(x = reorder(class, -richness), y = richness, fill = class)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Species Richness by Class in Ohiopyle State Park",
    x = "Class",
    y = "Species Richness"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size, family = "Cambria"),
    axis.text.y = element_text(size = axis_text_size, family = "Cambria"),
    plot.title = element_text(hjust = 0.5, size = title_size, face = "bold", family = "Cambria"),
    axis.title.x = element_text(size = axis_title_size, family = "Cambria"),
    axis.title.y = element_text(size = axis_title_size, family = "Cambria"),
    legend.title = element_text(size = legend_title_size, family = "Cambria"),
    legend.text = element_text(size = legend_text_size, family = "Cambria"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5)
  ) +
  scale_fill_manual(values = fungi_colors_vibrant) +
  scale_y_continuous(
    breaks = y_breaks, 
    limits = c(0, ceiling(max_count / 2) * 2)
  )