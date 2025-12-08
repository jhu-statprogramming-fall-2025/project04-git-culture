# Load required libraries
library(dplyr)
library(readr)
library(lubridate)

# Read the data files
nuforc_data <- read_csv("nuforc_county_weather.csv")
bfro_data <- read_csv("bfro_reports_geocoded.csv")

# Filter BFRO data for California and years 2004-2014
bfro_filtered <- bfro_data %>%
  filter(state == "California" | state == "CA",
         year(date) >= 2004,
         year(date) <= 2014) %>%
  select(latitude, longitude, state, date, county) %>%
  mutate(country = "US") %>%
  # Ensure consistent state abbreviation
  mutate(state = "CA") %>%
  # Rename columns to match nuforc naming convention
  rename(
    lat = latitude,
    lon = longitude,
    sighted_date = date,
    County = county  # Capitalize to match nuforc
  ) %>%
  # Convert date to character to match nuforc format (if needed)
  mutate(sighted_date = as.character(sighted_date))

nuforc_data <- nuforc_data %>%
  mutate(sighted_date = as.Date(sighted_date))

bfro_filtered <- bfro_filtered %>%
  mutate(sighted_date = as.Date(sighted_date))

# Add a 'sighting_type' column to each dataset before merging
nuforc_data <- nuforc_data %>%
  mutate(sighting_type = "UFO")

bfro_filtered <- bfro_filtered %>%
  mutate(sighting_type = "Bigfoot")

simple_merged <- bind_rows(nuforc_data, bfro_filtered)

# CLEAN UP DUPLICATE COLUMNS
simple_merged <- simple_merged %>%
  mutate(
    Location.State = ifelse(is.na(Location.State), state, Location.State),
    Location.Country = ifelse(is.na(Location.Country), country, Location.Country),
    # Capitalize all county names
    County = toupper(County)
  ) %>%
  # Remove the redundant state and country columns
  select(-state, -country)

# Write to CSV
write_csv(simple_merged, "sightings_combined_simple.csv")

