# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)  # For color palettes

# Read the CSV file
df <- read.csv("observations-468134.csv")

# Calculate counts per order
order_counts <- df %>%
  group_by(taxon_order_name) %>%
  summarize(count = n(), .groups = 'drop')

# Determine the maximum count value for setting y-axis limits
max_count <- max(order_counts$count)

# Create a sequence of even numbers for y-axis breaks, extending slightly beyond max_count
y_breaks <- seq(0, ceiling(max_count / 2) * 2, by = 2)  # Ensure even number intervals

# Define consistent font sizes
title_size <- 16
axis_title_size <- 14
axis_text_size <- 12
legend_title_size <- 14
legend_text_size <- 12

# Plot by order with colors
ggplot(order_counts, aes(x = reorder(taxon_order_name, -count), y = count, fill = taxon_order_name)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Abundance of Observations by Taxon Order",
    x = "Taxon Order",  # x-axis label
    y = "Count",
    fill = "Taxon Order"  # Legend title
  ) +
  theme_minimal(base_family = "Arial") +  # Set base font family
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size, family = "Arial"),
    axis.text.y = element_text(size = axis_text_size, family = "Arial"),
    plot.title = element_text(hjust = 0.5, size = title_size, face = "bold", family = "Arial"),
    axis.title.x = element_text(size = axis_title_size, family = "Arial"),
    axis.title.y = element_text(size = axis_title_size, family = "Arial"),
    legend.title = element_text(size = legend_title_size, family = "Arial"),
    legend.text = element_text(size = legend_text_size, family = "Arial"),
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Add subtle major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.25),  # Add subtle minor grid lines
    panel.border = element_blank()  # Remove border
  ) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    breaks = y_breaks, 
    limits = c(0, ceiling(max_count / 2) * 2)  # Set y-axis limits
  )
