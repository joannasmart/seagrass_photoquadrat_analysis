# ===========================================================================
# ReefCloud Data Cleaning and Processing Script
# ===========================================================================
# This script performs the following operations:
# 1. Reads in ReefCloud data and standardizes image codes
# 2. Reads in and formats coordinate data from multiple years
# 3. Joins the ReefCloud data with coordinate information
# 4. Calculates totals by category using a data dictionary
# 5. Exports clean data in multiple formats
# ===========================================================================

# ------------------ Load Required Packages ------------------
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files
library(plyr)       # For additional data manipulation functions
library(lubridate)  # For handling date and time data

# ------------------ Load Main Data ------------------
# Read the main ReefCloud data file
reefcloud <- read.csv("01_data/20250131_ReefCloud_ALL_DATA.csv")

# ------------------ Clean and Format Image Codes ------------------
# Standardize image codes by removing file extensions and unnecessary prefixes
reefcloud$image_code <- reefcloud$image_name
reefcloud$image_code <- gsub(".JPG", "", reefcloud$image_code)
reefcloud$image_code <- gsub(".jpg", "", reefcloud$image_code)
reefcloud$image_code <- gsub(" \\(", "_", reefcloud$image_code)
reefcloud$image_code <- gsub("\\)", "", reefcloud$image_code, perl = TRUE)
reefcloud$image_code <- sub("Ebanks_", "", reefcloud$image_code)
reefcloud$image_code <- sub("MB", "", reefcloud$image_code)
reefcloud$image_code <- sub("EB", "", reefcloud$image_code)
reefcloud$image_code <- sub("T", "", reefcloud$image_code)
reefcloud$image_code <- gsub("^\\d{7,8}_", "", reefcloud$image_code)
reefcloud$image_code  <- gsub("^([A-Za-z]+)_(\\d)", "\\1\\2", reefcloud$image_code)

# Split image code into transect and image number components
reefcloud <- reefcloud %>%
  tidyr::separate(image_code, into = c("transect", "image_number"), sep = "_", remove = FALSE)

# Convert date strings to proper date format
reefcloud <- reefcloud %>%
  dplyr::mutate(date = as.POSIXct(date..UTC., format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))

reefcloud$date <- as.Date(reefcloud$date)

# Extract year from date
reefcloud$year <- lubridate::year(reefcloud$date)

# Convert image number to numeric
reefcloud$image_number <- as.numeric(reefcloud$image_number)

# Remove unnecessary columns
reefcloud <- reefcloud %>%
  select(-c(date..UTC., survey_title, site, project, survey_id, site_id, unique_id, depth_m))

# Reorder columns to place key identifiers first
reefcloud <- reefcloud %>%
  select((ncol(.)-3):ncol(.), everything())

# Remove rows with no data in the measurement columns (columns 8-48)
reefcloud <- reefcloud[rowSums(reefcloud[,8:48])>0,]

# Ensure date is in proper format
reefcloud <- reefcloud %>%
  dplyr::mutate(date = lubridate::ymd(date))

# Create year-month column for grouping
reefcloud$year_month <- format(as.Date(reefcloud$date, "%Y-%m-%d"), "%Y-%m")

# Convert year_month to factor and standardize sampling periods
reefcloud$year_month <- as.factor(reefcloud$year_month) 
reefcloud$year_month <- recode_factor(reefcloud$year_month, 
                                      "2022-05" = "2022-06", 
                                      "2012-06" = "2012-07", 
                                      "2013-02" = "2013-01", 
                                      "2004-07" = "2004-08", 
                                      "2021-06" = "2021-07") 
# Note: Updating year-months where sampling crossed month boundaries

# Move the year-month column after the second column
reefcloud <- reefcloud %>%
  relocate(year_month, .after = 2)

# Create a subset of 2013 data
data13 <- reefcloud %>% filter(year == "2013")

# ------------------ Create and Clean Bank Column ------------------
# Extract bank information from transect codes
reefcloud$bank <- reefcloud$transect
reefcloud$bank <- sub("[0-9]+", "", reefcloud$bank)  # Remove numbers
# Standardize bank codes
reefcloud$bank <- sub("MOA", "MO", reefcloud$bank)   # Change MOA to MO
reefcloud$bank <- sub("CHE", "CH", reefcloud$bank)   # Change CHE to CH
reefcloud$bank <- sub("CHO", "CH", reefcloud$bank)   # Change CHO to CH
reefcloud$bank <- sub("AMA", "AM", reefcloud$bank)   # Change AMA to AM
reefcloud$bank <- sub("MAB", "MA", reefcloud$bank)   # Change MAB to MA

# Move bank column after year_month
reefcloud <- reefcloud %>%
  relocate(bank, .after = 2)

# ------------------ Clean Transect Names ------------------
# Standardize transect naming format
reefcloud$transect <- gsub("([A-Z]{2})0*(\\d+)", "\\1\\2", reefcloud$transect)
reefcloud$transect <- gsub("CHO1", "CH1", reefcloud$transect)
reefcloud$transect <- gsub("CH1E", "CH1", reefcloud$transect)

# Check results
unique(reefcloud$transect)
unique(reefcloud$year_month)

# ------------------ Read and Format Coordinate Data ------------------
# Load coordinate data files from different years
coords07 <- read.csv("01_Data/01_coordinates/2007_EasternBanks_Seagrass.csv")
coords11 <- read.csv("01_Data/01_coordinates/2011_EasternBanks_Seagrass.csv")
coords12 <- read.csv("01_Data/01_coordinates/2012_EasternBanks_Seagrass.csv")
coords13a <- read.csv("01_Data/01_coordinates/2013_EasternBanks_Seagrass_Feb.csv")
coords13b <- read.csv("01_Data/01_coordinates/2013_EasternBanks_Seagrass.csv")
coords14 <- read_excel("01_Data/01_coordinates/2014_EasternBanks_Seagrass.xlsx")
coords15 <- read.csv("01_Data/01_coordinates/2015_EasternBanks_Seagrass.csv")
coords21 <- read.csv("01_Data/01_coordinates/2021_EasternBanks_Seagrass.csv")
coords22 <- read.csv("01_Data/01_coordinates/2022_EasternBanks_Seagrass.csv")
coords23 <- read.csv("01_Data/01_coordinates/2023_EasternBanks_Seagrass.csv")
coords24a <- read.csv("01_Data/01_coordinates/2024_07_EasternBanks_Seagrass.csv")
coords24b <- read.csv("01_Data/01_coordinates/2024_12_EasternBanks_Seagrass.csv")

# ------------------ Standardize Coordinate Data Formats ------------------
# Standardize 2013 coordinate data
coords13a <- coords13a %>% select(NAME, Latitude, Longitude)
coords13b <- coords13b %>% select(NAME, Latitude, Longitude)
coords13 <- rbind(coords13a, coords13b)
coords13 <- coords13 %>%
  rename(c("NAME" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON"))
coords13$PHOTO_NAME <- gsub("Ebanks_", "", coords13$PHOTO_NAME) 

# Standardize 2012 coordinate data
coords12 <- coords12 %>%
  rename(c("PhotoName" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON"))

# Standardize 2014 coordinate data
coords14 <- coords14 %>%
  rename(c("PhotoName" = "PHOTO_NAME")) %>%
  relocate(LAT, .after = 1)

# Standardize 2015 coordinate data
coords15 <- coords15 %>%
  rename(c("PhotoName" = "PHOTO_NAME")) %>%
  relocate(LAT, .after = 1)

# Standardize 2022 coordinate data
coords22 <- coords22 %>%
  rename(c("Name" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON")) %>%
  select(PHOTO_NAME, LAT, LON) %>%
  relocate(LAT, .after = 1)

# Standardize 2023 coordinate data
coords23 <- coords23 %>%
  rename(c("Name" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON")) %>%
  relocate(LAT, .after = 1)

# Standardize 2024 July coordinate data
coords24a <- coords24a %>%
  rename(c("Name" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON")) %>%
  select(PHOTO_NAME, LAT, LON) %>%
  relocate(LAT, .after = 1)

# Standardize 2024 December coordinate data
coords24b <- coords24b %>%
  rename(c("Name" = "PHOTO_NAME", 
           "Latitude" = "LAT", 
           "Longitude" = "LON")) %>%
  select(PHOTO_NAME, LAT, LON) %>%
  relocate(LAT, .after = 1)

# ------------------ Combine All Coordinate Data ------------------
# Combine all coordinate datasets
all_coordinates <- rbind(coords07, coords11, coords12, coords13, coords14, coords15, coords21, coords22, coords23, coords24a, coords24b)

# Standardize column names
all_coordinates <- all_coordinates %>%
  rename(c("PHOTO_NAME" = "image_name", 
           "LAT" = "Latitude", 
           "LON" = "Longitude"))

# ------------------ Standardize Image Names for Joining ------------------
# Clean up image names in coordinate data
all_coordinates$image_name <- gsub(".JPG", "", all_coordinates$image_name)
all_coordinates$image_name <- gsub("EBanks", "", all_coordinates$image_name)
all_coordinates$image_name <- gsub(".jpg", "", all_coordinates$image_name)
all_coordinates$image_name <- gsub("-", "", all_coordinates$image_name)

# Clean up image names in ReefCloud data
reefcloud$image_name <- gsub(".JPG", "", reefcloud$image_name)
reefcloud$image_name <- gsub("Ebanks_", "", reefcloud$image_name)
reefcloud$image_name <- gsub(".jpg", "", reefcloud$image_name)

# ------------------ Join Data and Coordinates ------------------
# Merge the ReefCloud data with coordinates
compiled_clean_data <- left_join(reefcloud, all_coordinates, by = "image_name", relationship = "many-to-many")

# Reorganize columns to place latitude and longitude after bank
compiled_clean_data <- compiled_clean_data %>%
  relocate(Latitude, .after = 4) %>%
  relocate(Longitude, .after = 4)

# ------------------ Calculate Category Totals ------------------
# Load the data dictionary for categorization
dictionary <- read.csv("01_Data/Dictionary.csv") 

# Get unique categories
categories <- unique(dictionary$category)

# Loop over each category and create total columns
for (cat in categories) {
  # Get the species (column names) that belong to the current category
  species_in_category <- dictionary$name[dictionary$category == cat]
  
  # Ensure the species exist in compiled_clean_data
  species_in_category <- intersect(species_in_category, names(compiled_clean_data))
  
  # Create a new column summing the values of the species in this category
  compiled_clean_data[[paste0("total_", tolower(cat))]] <- 
    rowSums(compiled_clean_data[, species_in_category, drop = FALSE])
}

# ------------------ Remove Incomplete Sampling Events ------------------
# Filter out data from incomplete sampling periods
compiled_clean_data <- compiled_clean_data %>% 
  filter(year_month != "2007-08") %>% # blurry images
  filter(year_month != "2011-03") %>% # only 6 transects
  filter(year_month != "2011-04") %>%
  filter(year_month != "2011-09")

# ------------------ Save Processed Data ------------------
# Save the cleaned wide-format data
write_csv(compiled_clean_data, "03_outputs/compiled_cleaned_data.csv")

# Create and save long-format data
long_data <- compiled_clean_data %>%
  pivot_longer(cols = c(12:61), 
               names_to = "category_name", 
               values_to = "value")

write_csv(long_data, "03_outputs/compiled_cleaned_data_long.csv")

# ------------------ Create Summary Table ------------------
# Generate a summary table of sampling effort by time period
summary_table <- reefcloud %>%
  group_by(as.numeric(year), year_month) %>% 
  dplyr::summarise(
    Number_of_transects = n_distinct(transect),  # Count unique transects
    Photocount = n()                             # Count number of photos (rows)
  ) %>%
  ungroup() %>%  # Ungroup to avoid issues in further operations
  arrange(year_month)  # Ensure chronological order

# Save the summary table
write_csv(summary_table, "03_outputs/survey_summary.csv")

