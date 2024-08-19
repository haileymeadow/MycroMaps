# Introduction ----
# Calculating species richness in Ohiopyle State Park 
# Data from 2020 - August 13, 2024, sourced from GBIF
# GBIF.org (13 August 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.469ntm

# Hailey Wittkopp for MycroMaps project
# 08/14/2024

# Libraries ----
# List of required packages
packages <- c("ggplot2", "vegan", "shiny", "maps", "ggmap", "sf", "osmdata", "RColorBrewer")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(ggplot2)
library(vegan)
library(shiny)
library(maps)
library(ggmap)
library(sf)
library(osmdata)
library(RColorBrewer)

# Load Data ----
# Function to load data and handle errors
load_data <- function(file_path) {
  if (file.exists(file_path)) {
    return(read.csv(file_path))
  } else {
    stop(paste("File not found:", file_path))
  }
}

# Set the working directory
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF")

# Load datasets
OhiopyleData <- load_data("GBIF_FungalObservations_Cleaned.csv")
OhiopyleDataiNat <- load_data("iNaturalist Export.csv")

# Remove rows with missing values
OhiopyleData <- na.omit(OhiopyleData)
OhiopyleDataiNat <- na.omit(OhiopyleDataiNat)

# Convert categorical variables to factors
OhiopyleData$species <- as.factor(OhiopyleData$species)
OhiopyleData$genus <- as.factor(OhiopyleData$genus)
OhiopyleData$family <- as.factor(OhiopyleData$family)
OhiopyleData$class <- as.factor(OhiopyleData$class)

OhiopyleDataiNat$taxon_genus_name <- as.factor(OhiopyleDataiNat$taxon_genus_name)
OhiopyleDataiNat$taxon_family_name <- as.factor(OhiopyleDataiNat$taxon_family_name)
OhiopyleDataiNat$taxon_order_name <- as.factor(OhiopyleDataiNat$taxon_order_name)
OhiopyleDataiNat$taxon_class_name <- as.factor(OhiopyleDataiNat$taxon_class_name)

# Frequency counts GBIF
genus_freq <- table(OhiopyleData$genus)
family_freq <- table(OhiopyleData$family)
class_freq <- table(OhiopyleData$class)

print(genus_freq)
print(family_freq)
print(class_freq)

# Frequency counts iNat
genus_freqiNat <- table(OhiopyleDataiNat$taxon_genus_name)
family_freqiNat <- table(OhiopyleDataiNat$taxon_family_name)
order_freqiNat <- table(OhiopyleDataiNat$taxon_order_name)
class_freqiNat <- table(OhiopyleDataiNat$taxon_class_name)

print(class_freqiNat)

# Bar chart of species frequency
ggplot(OhiopyleData, aes(x = class, fill = class)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Frequency of Class Observations")

# Pie chart of genus distribution
ggplot(OhiopyleData, aes(x = "", fill = genus)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Distribution of Genera")

# Define Unique Classes
unique_classes <- unique(OhiopyleDataiNat$taxon_class_name)

# Create a color palette
num_classes <- length(unique_classes)
color_palette <- brewer.pal(n = num_classes, name = "Set3")
names(color_palette) <- unique_classes

# Define the bounding box for Ohiopyle State Park
bbox <- c(left = -81, bottom = 39, right = -79, top = 41)

# Fetch map data from OpenStreetMap
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'landuse') %>%
  osmdata_sf()


# Extract polygons and points
polygons <- osm_data$osm_polygons
points <- osm_data$osm_points

if (nrow(polygons) == 0) {
  stop("No relevant polygons found. Adjust the bounding box or query parameters.")
}

print(unique_classes)

unique_classes <- c("Agaricomycetes", "Pezizomycetes", "Lecanoromycetes", 
                    "Eurotiomycetes", "Leotiomycetes", "Sordariomycetes", 
                    "Entomophthoromycetes", "Pucciniomycetes", "Atractiellomycetes", 
                    "Dothideomycetes", "Tremellomycetes", "Dacrymycetes")



# Example data for Ohiopyle State Park (Replace with your actual data)
OhiopyleDataiNat <- data.frame(
  longitude = c(-80.080, -80.070, -80.060, -80.050),
  latitude = c(40.075, 40.065, 40.055, 40.045),
  Class = c(unique_classes <- c("Agaricomycetes", "Pezizomycetes", "Lecanoromycetes", 
                                "Eurotiomycetes", "Leotiomycetes", "Sordariomycetes", 
                                "Entomophthoromycetes", "Pucciniomycetes", "Atractiellomycetes", 
                                "Dothideomycetes", "Tremellomycetes", "Dacrymycetes") )
)

# Convert the example data to an sf object
OhiopyleData_sf <- st_as_sf(OhiopyleDataiNat, coords = c("longitude", "latitude"), crs = 4326)

# Create the plot
ggplot() +
  geom_sf(data = polygons, fill = "lightgreen", color = "black") +
  geom_sf(data = OhiopyleData_sf, aes(color = Class), size = 3) +
  scale_color_brewer(palette = "Set3", name = "Fungal Genus") +
  ggtitle("Geospatial Distribution of Fungal Observations in Ohiopyle State Park") +
  theme_void()