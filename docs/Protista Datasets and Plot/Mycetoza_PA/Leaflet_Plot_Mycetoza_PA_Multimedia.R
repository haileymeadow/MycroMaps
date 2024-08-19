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
install.packages("dplyr")
install.packages("htmlwidgets")

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmlwidgets)


# Load data into R ----
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA")

mycetoza_data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/mycetoza_data.csv")
multimedia_data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/multimedia.txt", sep = "\t")

# Merge the datasets based on a common identifier
merged_data <- mycetoza_data %>%
  left_join(multimedia_data, by = "gbifID")

head(mycetoza_data)
head(multimedia_data)

# Check if multimedia URLs are included
head(merged_data[, c("gbifID", "identifier")])

# Filter out Ohio data based on latitude and longitude
filtered_data <- merged_data %>%
  filter(
    !(decimalLatitude >= 38.4034 & decimalLatitude <= 41.9773 &
        decimalLongitude >= -85.5136 & decimalLongitude <= -80.5182)
  )

# Create a custom icon
icon_path <- "C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/Mycetoza_PA/protozoa.png"

my_icon <- makeIcon(
  iconUrl = icon_path,
  iconWidth = 32,  # Adjust width to fit the map view
  iconHeight = 32  # Adjust height to fit the map view
)


# Create a leaflet map with iNaturalist occurrence page links
map <- leaflet(filtered_data) %>%
  addProviderTiles(street_map, group = "Street Map") %>%
  addProviderTiles(satellite_map, group = "Satellite") %>%
  addMarkers(
    ~decimalLongitude, ~decimalLatitude,
    icon = my_icon,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif; color: #333;'>",
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank, "<br>",
      "<b>Coordinates:</b> (", decimalLatitude, ", ", decimalLongitude, ")<br>",
      "<b>Link to iNaturalist:</b> <a href='https://www.inaturalist.org/observations/", catalogNumber, "' target='_blank'>View Observation</a>",
      "</div>"
    ),
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = TRUE,
      zoomToBoundsOnClick = TRUE,
      removeOutsideVisibleBounds = TRUE
    )
  ) %>%
  setView(lng = mean(filtered_data$decimalLongitude, na.rm = TRUE), 
          lat = mean(filtered_data$decimalLatitude, na.rm = TRUE), 
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

# Save the map as an HTML file
saveWidget(map, "mycetoza_map.html")