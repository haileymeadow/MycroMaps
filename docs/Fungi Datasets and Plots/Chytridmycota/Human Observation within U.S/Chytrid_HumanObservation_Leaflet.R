# Introduction ----
# Plotting Chytridmycota observations in the U.S. using Leaflet
# Data sourced from rGBIF

# Hailey Wittkopp for MycroMaps project
# 08/20/2024


# Load and Install Libraries ----
install.packages("tidyverse")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("htmlwidgets")



library(tidyverse)
library(leaflet)
library(leaflet.extras)


# Load Data ----
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/docs/Fungi Datasets and Plots/Chytridmycota/Human Observation within U.S")

Chytrid_Data <- read.csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/docs/Fungi Datasets and Plots/Chytridmycota/Human Observation within U.S/Chytrid_Download.csv")

# View and analyze data ----
names(Chytrid_Data)

# Filter columns for mapping
chytrid_data_filtered <- Chytrid_Data %>%
  select(decimalLatitude, decimalLongitude, scientificName, taxonRank, taxonKey, gbifID, catalogNumber) %>%
  mutate(
    GBIF_Link = paste0("https://www.gbif.org/occurrence/", gbifID),
    iNaturalist_Link = paste0("https://www.inaturalist.org/observations/", catalogNumber)
  )

# Map the data ----

# Icon Attribution: <a href="https://www.flaticon.com/free-icons/fungi" title="Fungi icons">Fungi icons created by Vectors Tank - Flaticon</a>
icon_path <- "C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/docs/Fungi Datasets and Plots/Chytridmycota/fungi.png"

# Create a custom icon
my_icon <- makeIcon(
  iconUrl = icon_path,
  iconWidth = 32,  # Adjust width to fit
  iconHeight = 32  # Adjust height to fit
)

# Define base map layers for options
street_map <- providers$OpenStreetMap
satellite_map <- providers$Esri.WorldImagery

# Create a leaflet map
map <- leaflet(chytrid_data_filtered) %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addMarkers(
    ~decimalLongitude, ~decimalLatitude,
    icon = my_icon,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif; color: #333;'>",
      "<b>Species:</b> ", scientificName, "<br>",
      "<b>Taxon Rank:</b> ", taxonRank, "<br>",
      "<b>Coordinates:</b> (", decimalLatitude, ", ", decimalLongitude, ")<br>",
      "<b>iNaturalist:</b> <a href='", iNaturalist_Link, "' target='_blank'>View on iNaturalist</a><br>",
      "<b>GBIF:</b> <a href='", GBIF_Link, "' target='_blank'>View on GBIF</a>",
      "</div>"
    ),
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = TRUE,
      zoomToBoundsOnClick = TRUE,
      removeOutsideVisibleBounds = TRUE
    )
  ) %>%
  setView(lng = mean(chytrid_data_filtered$decimalLongitude, na.rm = TRUE), 
          lat = mean(chytrid_data_filtered$decimalLatitude, na.rm = TRUE), 
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

# Print the map
map

# Save the map as an HTML file
saveWidget(map, "chytrid_U.S._observation.html")