# Introduction ----

# GBIF Observations of Mycetoza within PA - following a leaflet tutorial
# GBIF.org (16 August 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.ftd7bh
# Icon attribution <a href="https://www.flaticon.com/free-icons/protozoa" title="protozoa icons">Protozoa icons created by Paul J. - Flaticon</a>

# Hailey Wittkopp for MycroMaps project
# 08/18/2024

# Load and Install Libraries ----
install.packages("tidyverse")
install.packages("leaflet")
install.packages("leaflet.extras")


library(tidyverse)
library(leaflet)
library(leaflet.extras)

# Load data into R ----
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA")

mycetoza_data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/mycetoza_data.csv")

names(mycetoza_data)


# Filter necessary columns for mapping ----
mycetoza_data_filtered <- mycetoza_data %>%
  select(decimalLatitude, decimalLongitude, scientificName, taxonRank, taxonKey)

# Create a leaflet map
# Create a leaflet map
map <- leaflet(mycetoza_data_filtered) %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add base map
  addMarkers(
    ~decimalLongitude, ~decimalLatitude, 
    popup = ~paste(
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank
    ),
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = TRUE,
      zoomToBoundsOnClick = TRUE
    )
  ) %>%
  setView(lng = mean(mycetoza_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(mycetoza_data_filtered$decimalLatitude, na.rm = TRUE), 
          zoom = 7)  # Adjust zoom level as needed

map

# remove ohio instance
# Filter out Ohio data based on latitude and longitude
mycetoza_data_filtered <- mycetoza_data %>%
  filter(
    !(decimalLatitude >= 38.4034 & decimalLatitude <= 41.9773 &
        decimalLongitude >= -85.5136 & decimalLongitude <= -80.5182)
  )

# Create a leaflet map
map <- leaflet(mycetoza_data_filtered) %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add base map
  addMarkers(
    ~decimalLongitude, ~decimalLatitude, 
    popup = ~paste(
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank
    ),
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(mycetoza_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(mycetoza_data_filtered$decimalLatitude, na.rm = TRUE), 
          zoom = 7)  # Adjust zoom level as needed

# Print map
map

icon_path <- "C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/protozoa.png"

# Create a custom icon
my_icon <- makeIcon(
  iconUrl = icon_path,
  iconWidth = 32,  # Adjust width to fit the map view
  iconHeight = 32  # Adjust height to fit the map view
)

# New map with icon
# Create a leaflet map with custom icon
map <- leaflet(mycetoza_data_filtered) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addMarkers(
    ~decimalLongitude, ~decimalLatitude,
    icon = my_icon,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif;'>",
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank, "<br>",
      "<b>Coordinates:</b> (", decimalLatitude, ", ", decimalLongitude, ")",
      "</div>"
    ),
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(mycetoza_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(mycetoza_data_filtered$decimalLatitude, na.rm = TRUE), 
          zoom = 7)

# Add fullscreen
map <- map %>%
  addFullscreenControl()

# Add search
map <- map %>%
  addSearchOSM()

map <- map %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Define base map layers
street_map <- providers$OpenStreetMap
satellite_map <- providers$Esri.WorldImagery


library(leaflet)

# Define base map layers
street_map <- providers$OpenStreetMap
satellite_map <- providers$Esri.WorldImagery


# Create a leaflet map with custom icon and layers
map <- leaflet(mycetoza_data_filtered) %>%
  addProviderTiles(street_map, group = "Street Map") %>%  # Add base map layer for street map
  addProviderTiles(satellite_map, group = "Satellite") %>%  # Add base map layer for satellite imagery
  addMarkers(
    ~decimalLongitude, ~decimalLatitude,
    icon = my_icon,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif;'>",
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank, "<br>",
      "<b>Coordinates:</b> (", decimalLatitude, ", ", decimalLongitude, ")",
      "</div>"
    ),
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = TRUE,
      zoomToBoundsOnClick = TRUE
    )
  ) %>%
  setView(lng = mean(mycetoza_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(mycetoza_data_filtered$decimalLatitude, na.rm = TRUE), 
          zoom = 7) %>%
  addLayersControl(
    baseGroups = c("Street Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addFullscreenControl() %>%  # Add fullscreen control
  addSearchOSM()  # Add search control

map


# Create a leaflet map with professional enhancements
map <- leaflet(mycetoza_data_filtered) %>%
  addProviderTiles(street_map, group = "Street Map") %>%
  addProviderTiles(satellite_map, group = "Satellite") %>%
  addMarkers(
    ~decimalLongitude, ~decimalLatitude,
    icon = my_icon,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif; color: #333;'>",
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank, "<br>",
      "<b>Coordinates:</b> (", decimalLatitude, ", ", decimalLongitude, ")",
      "</div>"
    ),
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = TRUE,
      zoomToBoundsOnClick = TRUE,
      removeOutsideVisibleBounds = TRUE
    )
  ) %>%
  setView(lng = mean(mycetoza_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(mycetoza_data_filtered$decimalLatitude, na.rm = TRUE), 
          zoom = 7) %>%
  addLayersControl(
    baseGroups = c("Street Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addFullscreenControl() %>%
  addSearchOSM() %>%
  addScaleBar(position = "bottomleft") %>%
  addEasyButton(
    easyButton(
      icon = "fa-crosshairs",
      title = "Locate me",
      onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 16}); }")
    )
  )

# Print map
map