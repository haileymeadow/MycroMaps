# Introduction ----

# Plotting Fungi and Protozoa abundance by orders in Ohiopyle State Park
# Data from July 1 2024 - August 1 2024, sourced from iNaturalist

# Hailey Wittkopp for MycroMaps project
# 08/13/2024


# Libraries ----

# Install these packages if not already installed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("extrafont")

library(ggplot2) # Visualize Results
library(dplyr) # Manipulate Data
library(RColorBrewer) # Color Palette
library(extrafont) # Fonts

# More Fonts (this is optional, but I prefer utilizing fonts on my system)
 # font_import() # Only do this for initial set-up, you should then be able to simply loadfonts()
loadfonts()


# Load Data ----
# Set working directory on Windows
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycoMaps/Fungi Datasets and Plots/Ohiopyle State Park/July 2024 Data by iNaturalist")

# Read the CSV file
OhiopyleData <- read.csv("iNaturalist-July2024(Ohiopyle).csv")

# Analyze Data ----

# Calculate counts per order
order_counts <- OhiopyleData %>%
  group_by(taxon_order_name) %>%
  summarize(count = n())

# Check all variables are correct 
str(OhiopyleData)

# Plotting the Data ----

# Y-axis Limit
max_count <- max(order_counts$count)


# Font Sizes
title_size <- 16
axis_title_size <- 14
axis_text_size <- 12
legend_title_size <- 14
legend_text_size <- 12

# Plotting by Order using Colors 

png("Ohiopyle-July2024Abundance.png")

ggplot(order_counts, aes(x = reorder(taxon_order_name, -count), y = count, fill = taxon_order_name)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Abundance of Observations by Taxon Order",
    x = "Taxon Order",  # x-axis label
    y = "Count",
    fill = "Taxon Order"  # Legend title
  ) +
  theme_minimal(base_family = "Cambria") +  # Base Font
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size, family = "Cambria"),
    axis.text.y = element_text(size = axis_text_size, family = "Cambria"),
    plot.title = element_text(hjust = 0.5, size = title_size, face = "bold", family = "Cambria"),
    axis.title.x = element_text(size = axis_title_size, family = "Cambria"),
    axis.title.y = element_text(size = axis_title_size, family = "Cambria"),
    legend.title = element_text(size = legend_title_size, family = "Cambria"),
    legend.text = element_text(size = legend_text_size, family = "Cambria"),
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add subtle major grid lines
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    breaks = y_breaks, 
    limits = c(0, ceiling(max_count / 2) * 2)  # Set y-axis limits
  )

dev.off()