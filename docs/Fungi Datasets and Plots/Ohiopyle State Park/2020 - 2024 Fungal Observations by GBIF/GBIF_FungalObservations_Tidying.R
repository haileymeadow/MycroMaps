# Introduction ----

# Tidying Data from GBIF Fungal Observations
# GBIF Data from 2020-2024 (August) of Ohiopyle State Park
# GBIF.org (13 August 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.469ntm

# Hailey Wittkopp for MycroMaps project
# 08/13/2024


# Libraries ----

# Load  the necessary libraries for cleaning
install.packages("tidyr")
install.packages("dplyr")
install.packages("readr")

library(readr)
library(dplyr)
library(tidyr)

# Load Data ----

# Read the file as a single column
raw_data <- read_csv("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycoMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF/GBIF_FungalObservations_Untidy.csv", 
                     col_names = FALSE, 
                     locale = locale(encoding = "UTF-8"))

# Manually separate the columns
data_split <- raw_data %>%
  separate(X1, into = c(
    "taxon_key", "scientific_name", "accepted_taxon_key", "accepted_scientific_name",
    "number_of_occurrences", "taxon_rank", "taxonomic_status", "kingdom", "kingdom_key",
    "phylum", "phylum_key", "class", "class_key", "order", "order_key",
    "family", "family_key", "genus", "genus_key", "species", "species_key",
    "iucn_red_list_category"
  ), sep = "\t", fill = "right")

# Clean and Convert 'number_of_occurrences' ----

# Remove non-numeric characters and handle empty strings
data_cleaned <- data_split %>%
  mutate(
    number_of_occurrences = gsub("[^0-9]", "", number_of_occurrences),  # Remove non-numeric characters
    number_of_occurrences = ifelse(number_of_occurrences == "", NA, number_of_occurrences),  # Handle empty strings
    number_of_occurrences = as.numeric(number_of_occurrences)  # Convert to numeric
  )

# Check for remaining values
summary(data_cleaned$number_of_occurrences)

# Replace remaining NA values with zero or another value
data_cleaned <- data_cleaned %>%
  mutate(
    number_of_occurrences = ifelse(is.na(number_of_occurrences), 0, number_of_occurrences)
  )

# Data Types ----
data_cleaned <- data_cleaned %>%
  mutate(
    taxon_key = as.character(taxon_key),
    scientific_name = as.character(scientific_name),
    accepted_taxon_key = as.character(accepted_taxon_key),
    accepted_scientific_name = as.character(accepted_scientific_name),
    number_of_occurrences = as.numeric(number_of_occurrences),
    taxon_rank = as.factor(taxon_rank),
    taxonomic_status = as.factor(taxonomic_status),
    kingdom = as.character(kingdom),
    kingdom_key = as.character(kingdom_key),
    phylum = as.character(phylum),
    phylum_key = as.character(phylum_key),
    class = as.character(class),
    class_key = as.character(class_key),
    order = as.character(order),
    order_key = as.character(order_key),
    family = as.character(family),
    family_key = as.character(family_key),
    genus = as.character(genus),
    genus_key = as.character(genus_key),
    species = as.character(species),
    species_key = as.character(species_key),
    iucn_red_list_category = as.character(iucn_red_list_category)
  )

# Write Cleaned Data to CSV ----
write_csv(data_cleaned, "C:/Users/wittk/OneDrive/Documents/MycoMaps/MycoMaps/Fungi Datasets and Plots/Ohiopyle State Park/2020 - 2024 Fungal Observations by GBIF/GBIF_FungalObservations_Cleaned.csv")
