# Introduction ----
# Plotting Chytridmycota richness in the U.S. using Leaflet
# Data sourced from rGBIF

# Hailey Wittkopp for MycroMaps project
# 08/20/2024

# Load the libraries ----
install.packages("gganimate")
install.packages("transformr") # For animation support
install.packages("plotly")

library(gganimate)
library(transformr)
library(plotly)
library(dplyr)
library(ggplot2)
library(extrafont)
library(htmlwidgets)


loadfonts()

# Set working directory and load data ----
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/docs/Fungi Datasets and Plots/Chytridmycota/Human Observation within U.S")
Chytrid_Data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/docs/Fungi Datasets and Plots/Chytridmycota/Human Observation within U.S/Chytrid_Download.csv")


# Analyze the Data ----

# Convert 'eventDate' column to Date type
Chytrid_Data$eventDate <- as.Date(Chytrid_Data$eventDate, format = "%Y-%m-%d")

# Count the number of entries per date
entries_per_date <- Chytrid_Data %>%
  group_by(eventDate) %>%
  summarise(entries = n(), .groups = 'drop')

# View the result
head(entries_per_date)

# Aggregate by month
entries_per_month <- entries_per_date %>%
  mutate(year_month = format(eventDate, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(total_entries = sum(entries), .groups = 'drop')

# View the result
head(entries_per_month)

# Aggregate by year
entries_per_year <- entries_per_date %>%
  mutate(year = year(eventDate)) %>%
  group_by(year) %>%
  summarise(total_entries = sum(entries), .groups = 'drop')

# View the result
head(entries_per_year)

# Plot monthly trends ----
ggplot(entries_per_month, aes(x = as.Date(paste0(year_month, "-01")), y = total_entries)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Entries of Chytrids",
       x = "Date",
       y = "Total Entries") +
  theme_minimal()

# Convert 'year_month' to Date format for plotting
entries_per_month <- entries_per_month %>%
  mutate(date = as.Date(paste0(year_month, "-01"))) # Create a date column for plotting

# View the result
head(entries_per_month)

head(entries_per_month)

str(entries_per_month)

# Define the custom chytrid color palette
chytrid_palette <- c("Line" = "#5e4fa2", "Points" = "#93a24f")

# Create the static ggplot plot with updated aesthetics and custom colors
static_plot <- ggplot(entries_per_month, aes(x = date, y = total_entries)) +
  geom_line(color = chytrid_palette["Line"], linewidth = 1.2) +
  geom_point(aes(text = paste("Entries on this Date:", total_entries, "<br>Date:", format(date, "%B %d, %Y"))),
             color = chytrid_palette["Points"], size = 3) +
  labs(title = "Monthly Entries of Chytrids",
       subtitle = "Visualizing the total number of chytrid entries over time",
       x = "Date",
       y = "Total Entries") +
  theme_minimal(base_family = "Arial") + # Use Arial font for simplicity
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, face = "italic", color = "#34495e"),
    axis.title = element_text(size = 12, color = "#2c3e50"),
    axis.text = element_text(size = 10, color = "#34495e"),
    panel.grid.major = element_line(color = "#ecf0f1"),
    panel.grid.minor = element_line(color = "#bdc3c7")
  ) +
  coord_cartesian(clip = 'off') + # Ensure points are not clipped
  ylim(0, max(entries_per_month$total_entries, na.rm = TRUE) * 1.1) # Extend y-axis to avoid clipping

# Convert ggplot to plotly
interactive_plot <- ggplotly(static_plot, tooltip = "text")

# Print the interactive plot
interactive_plot

htmlwidgets::saveWidget(interactive_plot, "chytrids_by_month.html", selfcontained = TRUE)


# Plot yearly trends ----
ggplot(entries_per_year, aes(x = year, y = total_entries)) +
  geom_bar(stat = "identity") +
  labs(title = "Yearly Entries of Chytrids",
       x = "Year",
       y = "Total Entries") +
  theme_minimal()