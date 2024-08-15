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
install.packages("ggplot2") # Visualize and Plot data
install.packages("extrafont") # Add additional font styles
install.packages("tibble") 
install.packages("tidyr") # Tidying data for analysis
install.packages("patchwork")


library(dplyr)
library(vegan)
library(ggplot2)
library(extrafont)
library(tibble)
library(tidyr)
library(patchwork)

# Load Fonts into R
loadfonts()

# Load Data ----

# Set the working directory
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF")

# Load the .csv file
OhiopyleData <- read.csv("GBIF_FungalObservations_Cleaned.csv")

# Analyze Data ----

# Learning to use "vegan" package

# Create community matrix
community_matrix <- OhiopyleData %>%
  select(class, scientific_name, number_of_occurrences) %>%
  pivot_wider(names_from = scientific_name, values_from = number_of_occurrences, values_fill = 0) %>%
  column_to_rownames("class")

# Check community matrix has been created properly
head(community_matrix)

# Calculate Shannon diversity 
shannon_div <- diversity(community_matrix, index = "shannon")
print(shannon_div)

# Calculate Simpson diversity 
simpson_div <- diversity(community_matrix, index = "simpson")
print(simpson_div)

# Calculate species richness
species_richness <- specnumber(community_matrix)
print(species_richness)

# Plot Species Index ----

# Convert to data frame for plotting
diversity_df <- data.frame(
  class = rownames(community_matrix),
  Shannon = shannon_div,
  Simpson = simpson_div
)

# Check the column names and the first few rows
str(diversity_df)
head(diversity_df)

# Ensure "class" is a factor with proper levels
diversity_df$class <- factor(diversity_df$class, levels = unique(diversity_df$class))

# Remove rows where "class" is mistakenly set as "class"
diversity_df <- diversity_df %>%
  filter(class != "class")

color_palette <- c(
  "Agaricomycetes" = "#8B4513",
  "Pezizomycetes" = "#D2691E", 
  "Lecanoromycetes" = "#FF6347", 
  "Sordariomycetes" = "#FFD700", 
  "Leotiomycetes" = "#32CD32", 
  "Ascomycetes" = "#6A5ACD",  
  "Basidiomycetes" = "#FF4500", 
  "Eurotiomycetes" = "#DAA520", 
  "Dothideomycetes" = "#FF1493", 
  "Saccharomycetes" = "#00FA9A", 
  "Mucoromycetes" = "#BA55D3",  
  "Mortierellomycetes" = "#FF8C00", 
  "Tremellomycetes" = "#4B0082"   
)

# Create individual plots for a combined plot using patchwork

# Shannon Diversity Index Plot
plot_shannon <- ggplot(diversity_df, aes(x = reorder(class, Shannon), y = Shannon, fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(title = "Shannon Diversity Index by Class",
       x = "Class",
       y = "Shannon Diversity Index",
       fill = "Classes") + 
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_family = "Cambria") +
  theme(axis.text.x = element_text(size = 12, family = "Cambria"),
        axis.text.y = element_text(size = 12, family = "Cambria"),
        plot.title = element_text(size = 14, face = "bold", family = "Cambria"),
        axis.title.x = element_text(size = 12, family = "Cambria"),
        axis.title.y = element_text(size = 12, family = "Cambria"))

# Simpson Diversity Index Plot
plot_simpson <- ggplot(diversity_df, aes(x = reorder(class, Simpson), y = Simpson, fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Simpson Diversity Index by Class",
       x = "Class",
       y = "Simpson Diversity Index",
       fill = "Classes") +  
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_family = "Cambria") +
  theme(axis.text.x = element_text(size = 12, family = "Cambria"),
        axis.text.y = element_text(size = 12, family = "Cambria"),
        plot.title = element_text(size = 14, face = "bold", family = "Cambria"),
        axis.title.x = element_text(size = 12, family = "Cambria"),
        axis.title.y = element_text(size = 12, family = "Cambria"))

# Combine plots using patchwork
combined_plot <- (plot_shannon + plot_simpson) +  # Arrange plots side by side
  plot_layout(ncol = 2, widths = c(1, 1)) +  # Adjust the widths of the plots
  plot_annotation(title = "Diversity Indices by Class",
                  subtitle = "Comparison of Shannon and Simpson Diversity Indices",
                  caption = "Source: Ohiopyle State Park GBIF Data from 2020-2024",
                  theme = theme(plot.title = element_text(size = 16, face = "bold"),
                                plot.subtitle = element_text(size = 14, face = "italic"),
                                plot.caption = element_text(size = 10, face = "italic"))) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))  # Set margins around the plot

# Save the combined plot
ggsave("combined_diversity_plot.png", combined_plot, width = 12, height = 6, dpi = 300)

# Display the combined plot
combined_plot