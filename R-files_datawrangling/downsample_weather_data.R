# Load libraries
library(dplyr)
library(readr)
library(lubridate)

setwd("C:/Users/Curtis/Downloads/archive (1)")
# File path to original CSV
file_path <- "C:/Users/Curtis/Downloads/archive (1)/WeatherEvents_Jan2016-Dec2022.csv"

# Read the CSV
weather <- read_csv(file_path, show_col_types = FALSE)

# Convert StartTime to date-time (ignores failures)
weather$StartTime <- ymd_hms(weather$`StartTime(UTC)`, quiet = TRUE)

# Filter for 2016-2018 and remove rows with failed dates
subset_weather <- weather %>%
  filter(!is.na(StartTime)) %>%
  filter(year(StartTime) >= 2016 & year(StartTime) <= 2018)

# Determine number of rows to sample (max 100k)
n_sample <- min(100000, nrow(subset_weather))

# Random sample ~100k rows
set.seed(42)
sample_weather <- subset_weather %>%
  slice_sample(n = n_sample)

# Save compressed CSV directly to Downloads
write_csv(sample_weather, "C:/Users/Curtis/Downloads/WeatherEvents_2016-2018_sample.csv.gz")
