# Introduction ----

# Tidying Data from GBIF Protista Observations
# GBIF.org (16 August 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.ftd7bh


# Hailey Wittkopp for MycroMaps project
# 08/18/2024


# Libraries ----

# Load  the necessary libraries for cleaning
install.packages(c("readr", "tidyverse", "xml2", "finch"))
library(readr)
library(tidyverse)
library(xml2)
library(finch)


# Load Data ----

# Set the working directory
setwd("C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot")


# Define the path to the Darwin Core Archive
dwca_file <- "C:/Users/wittk/OneDrive/Documents/MycoMaps/MycroMaps/Protista Datasets and Plot/0069515-240626123714530.zip"

unzip(dwca_file, exdir = tempdir())

dwc_data <- dwca_read(tempdir())

# List all data files in the archive
data_files <- names(dwc_data)
print(data_files)

data_df <- dwc_data[["data"]]  # Replace "occurrence" with the actual file name if different

head(data_df)


# Define the paths to the data files
occurrence_file <- "C:/Users/wittk/AppData/Local/Temp/Rtmp04woM0/occurrence.txt"
verbatim_file <- "C:/Users/wittk/AppData/Local/Temp/Rtmp04woM0/verbatim.txt"
multimedia_file <- "C:/Users/wittk/AppData/Local/Temp/Rtmp04woM0/multimedia.txt"

occurrence_data <- read_delim(occurrence_file, delim = "\t")

# Read the occurrence file with fill and quote parameters
occurrence_data <- read.table(occurrence_file, sep = "\t", header = TRUE, fill = TRUE, quote = "\"")

# Read the verbatim file
verbatim_data <- read.csv(verbatim_file, sep = "\t", header = TRUE)

# Read the multimedia file
multimedia_data <- read.csv(multimedia_file, sep = "\t", header = TRUE)

# Inspect the occurrence data
head(occurrence_data)

# Inspect the verbatim data
head(verbatim_data)

# Inspect the multimedia data
head(multimedia_data)


# Read the occurrence file with error handling
occurrence_data <- tryCatch({
  read_delim(occurrence_file, delim = "\t", col_types = cols(.default = "c"))  # Read as character initially
}, error = function(e) {
  message("Error reading occurrence file: ", e$message)
  NULL
})

# Read the verbatim file
verbatim_data <- tryCatch({
  read_delim(verbatim_file, delim = "\t", col_types = cols(.default = "c"))
}, error = function(e) {
  message("Error reading verbatim file: ", e$message)
  NULL
})

# Read the multimedia file
multimedia_data <- tryCatch({
  read_delim(multimedia_file, delim = "\t", col_types = cols(.default = "c"))
}, error = function(e) {
  message("Error reading multimedia file: ", e$message)
  NULL
})


# Inspect the structure of each dataset
str(occurrence_data)
str(verbatim_data)
str(multimedia_data)

# Summary of each dataset
summary(occurrence_data)
summary(verbatim_data)
summary(multimedia_data)

# Cleaning of Occurance ----

# Clean occurrence data: Remove rows with missing scientific names
cleaned_occurrence <- occurrence_data %>%
  filter(!is.na(scientificName))  # Replace `scientificName` with relevant column

# Identify and remove rows with an incorrect number of columns
expected_cols <- ncol(occurrence_data)  # Replace with the expected number of columns
occurrence_data <- occurrence_data[ncol(occurrence_data) == expected_cols, ]

# Remove rows with missing values in critical columns
cleaned_occurrence <- occurrence_data %>%
  filter(!is.na(scientificName))  # Replace `scientificName` with the actual column name

# Impute missing values with a placeholder (e.g., "Unknown")
occurrence_data$scientificName[is.na(occurrence_data$scientificName)] <- "Unknown"

# Remove rows with any missing values
cleaned_occurrence <- na.omit(occurrence_data)
