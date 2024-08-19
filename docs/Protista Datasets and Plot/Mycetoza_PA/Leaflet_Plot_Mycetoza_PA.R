# Introduction ----

# GBIF Observations of Mycetoza within PA - following a leaflet tutorial
# GBIF.org (16 August 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.ftd7bh


# Hailey Wittkopp for MycroMaps project
# 08/18/2024

# Load and Install Libraries ----
install.packages("tidyverse")
install.packages("leaflet")

library(tidyverse)
library(leaflet)

# Load data into R ----
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA")

mycetoza_data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/mycetoza_data.csv")


# Analyze Data ----
mycetoza_map <- mycetoza_data %>%
  #We will change NA values in both columns to 0
  mutate(individualCount = replace_na(individualCount, 0),
         organismQuantity =  replace_na(organismQuantity, 0),
         #Then we will create a new column with the sum of these two columns
         count = rowSums(across(individualCount:organismQuantity))) %>% 
  #Now we will change any rows with zero values to one because we selected
  #to include presence data only in our search. This means that each row
  #represents one individual
  mutate(count = case_when(count == 0 ~ 1,
                           T ~ count)) %>% 
  #Let's select only the columns we are interested in plotting
  select(count, decimalLatitude, decimalLongitude) %>% 
  #We will rename the latitude and longitude columns so leaflet can
  #recognise them as coordinates
  rename(latitude = decimalLatitude, longitude = decimalLongitude) %>% 
  #Finally, if observations have the same latitude and longitude, we will
  #add up their counts
  group_by(latitude, longitude) %>% 
  summarise(n = sum(count))

# Checking for issues ----
mycetoza_data %>% 
  #We select the "issue" column
  select(issue) %>% 
  #Only include unique entries
  distinct()

# Mapping data using leaflet ----
#Let's create a smaller sample of our data to test our map
leaflet(mycetoza_map) %>% 
  #We add a base map
  addTiles() %>% 
  #We add markers to our observations
  addMarkers(~longitude, ~latitude, 
             #We show some information in the pop up. We will include
             #the counts, but we must transform them to strings first
             popup = as.character(mycetoza_map$n))

mycetoza_data %>% 
  #We filter rows with the smallest longitude value
  filter(decimalLongitude == min(decimalLongitude)) %>% 
  #We select only a few columns
  select(decimalLatitude, decimalLongitude, issue)


mycetoza_map %>% 
  filter(longitude != min(.$longitude)) %>% 
  leaflet() %>% 
  #We add a base map
  addTiles() %>% 
  #We add markers to our observations
  addMarkers(~longitude, ~latitude, 
             #We show some information in the pop up. We will include
             #the counts, but we must transform them to strings first
             popup = as.character(mycetoza_map$n))


# Define the longitude range for Pennsylvania
min_longitude <- -80.5
max_longitude <- -74.5

# Filter out data from Ohio
mycetoza_map_filtered <- mycetoza_map %>%
  filter(longitude >= min_longitude & longitude <= max_longitude)

# Mapping data using leaflet with filtered data ----
leaflet(mycetoza_map_filtered) %>% 
  # Add a base map
  addTiles() %>% 
  # Add markers to our observations
  addMarkers(~longitude, ~latitude, 
             # Show counts in the pop-up
             popup = as.character(mycetoza_map_filtered$n))

#We start up the map as before
mycetoza_map_filtered <- mycetoza_map_filtered %>% 
  mutate(num = 1) %>% 
  leaflet() %>% 
  #Add a base layer
  addTiles() %>% 
  #We will use circles instead of markers
  addCircleMarkers(~longitude, ~latitude, popup = as.character(mycetoza_map_filtered$n), 
                   #We set the radius and opacity of our circles
                   radius = 1, fillOpacity = 0.5, 
                   #We calculate the clusters
                   clusterOptions = markerClusterOptions())
# Show map
mycetoza_map_filtered

#Check if Map folder exists, otherwise create a new one
if(!dir.exists("Map")){
  dir.create("Map")}

#Save map
htmlwidgets::saveWidget(mycetoza_map_filtered, file = "Map/Mycetoza_Map_PA.html")