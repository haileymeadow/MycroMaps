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

