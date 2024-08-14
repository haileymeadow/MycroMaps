# Introduction ----

# Plotting Fungi from Ohiopyle State Park observations
# iNaturalist Data from 2020-2024 (August)

# Hailey Wittkopp for MycoMaps project
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
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycoMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF")

# Read the CSV file
OhiopyleData <- read.csv("iNaturalist Export.csv")

# Analyze Data ----

# Calculate counts per class
order_counts <- OhiopyleData %>%
  group_by(taxon_class_name) %>%
  summarize(count = n())

# Check all variables are correct 
str(OhiopyleData)

# Filter out Agaricomycetes (Agaricomycetes is extremely abundant is this area)
# If this is not filtered out, the other species abundance would be difficult to visualize
filtered_data <- order_counts %>%
  filter(taxon_class_name != "Agaricomycetes")

# Recalculate max count for the new filtered data
max_count_filtered <- max(filtered_data$count)

# Plotting the Data ----

# Define Y-Breaks
y_breaks_filtered <- seq(0, ceiling(max_count_filtered / 5) * 5, by = 5)

# Create a fun color palette
fungi_colors_vibrant <- c(
  "#FF5722", "#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", 
  "#2196F3", "#03A9F4", "#4CAF50", "#8BC34A", "#FFC107"
)

# Save the plot to a PNG file
png("Taxonomic_Class_Distribution.png", width = 1800, height = 1200, res = 150)

# Plotting by Class using Colors
ggplot(filtered_data, aes(x = reorder(taxon_class_name, -count), y = count, fill = taxon_class_name)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Taxonomic Class Distribution: An Exploration of Observational Data",
    subtitle = "Data from Ohiopyle State Park, 2020 to 2024",
    x = "Taxon Class",
    y = "Organisms Observed",
    fill = "Taxon Class"
  ) +
  theme_minimal(base_family = "Cambria") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Cambria"),
    axis.text.y = element_text(size = 12, family = "Cambria"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Cambria"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, family = "Cambria"),
    axis.title.x = element_text(size = 14, family = "Cambria"),
    axis.title.y = element_text(size = 14, family = "Cambria"),
    legend.title = element_text(size = 14, family = "Cambria"),
    legend.text = element_text(size = 12, family = "Cambria"),
    legend.background = element_rect(color = "black", linewidth = 0.5),
    legend.key = element_rect(color = "black", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 20, 10)
  ) +
  scale_fill_manual(values = fungi_colors_vibrant) +
  scale_y_continuous(
    limits = c(0, ceiling(max_count_filtered / 5) * 5),
    breaks = y_breaks_filtered
  )

dev.off()