# practice for dashboard - health stuff

caliYPLL <- read.csv("county_health_data/combined_california_ypll.csv")
nuforc <- read.csv("nuforc_county_weather_bigfoot.csv")

## can use ggplot2 and have mutliple separate layers per plot and pull from different dataframes
library(ggplot2)
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

options(tigris_use_cache = TRUE)

# 1. Load US county shapefiles
counties_sf <- counties(cb = TRUE, year = 2023)  # cb = cartographic boundaries

# 2. Make sure FIPS codes match format (zero-padded 5-digit strings)
caliYPLL$FIPS <- sprintf("%05d", as.numeric(caliYPLL$FIPS))

# 3. Join your data to the map data using FIPS
map_df <- counties_sf %>%
  mutate(FIPS = GEOID) %>%
  left_join(caliYPLL, by = "FIPS")

# 4. Plot choropleth
YPLL_plot <- ggplot(map_df %>% filter(STATEFP == "06")) +
  geom_sf(aes(fill = YPLL_Rate), color = NA) +     # NA removes borders for cleaner heat map
  scale_fill_viridis(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Years of Potential Life Lost Rate by County",
    fill = "YPLL Rate"
  ) +
  theme_minimal()

## format ufo_data for same visualization
## clean data so names match
ufo_data <- ufo_data %>%
  mutate(
    County = str_to_title(County),     # "ALAMEDA" → "Alameda"
    County = paste0(County, " County"),
    State = str_to_title(Location.State)        # if you have a State column
  )

## prepare shapefile names
counties_sf <- counties(cb = TRUE, year = 2023) %>%
  mutate(
    County = paste0(NAME, " County"),
    State = str_to_title(STATE_NAME)
  )

ufo_map_df <- counties_sf %>%
  left_join(ufo_data, by = c("State", "County"))

# Reproject shapefile (fixes warning #1)
counties_sf_proj <- st_transform(counties_sf, 5070)

# plot
ggplot(
  counties_sf_proj %>%
    filter(STUSPS == "CA") %>%   # only California
    left_join(
      ufo_data %>%
        filter(State == "CA") %>%
        count(State, County, name = "sightings"),   # count inline
      by = c("STUSPS" = "State", "NAME" = "County")
    )
) +
  geom_sf(aes(fill = sightings), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Sightings"
  ) +
  labs(
    title = "UFO Sightings per County in California",
    subtitle = "Number of sightings per county"
  ) +
  theme_minimal()

sightings_heatmap <- ggplot(ufo_map_df %>% filter(STATEFP == "06"))+
  geom_sf(aes(fill="County"), color = NA)+
  scale_fill_viridis(option="magma", direction = -1, na.value = "grey90")+
  labs(
    title = "Sightings by county",
    fill = "County")+
  theme_minimal()
)
YPLL_plot <- ggplot(map_df %>% filter(STATEFP == "06")) +
  geom_sf(aes(fill = YPLL_Rate), color = NA) +     # NA removes borders for cleaner heat map
  scale_fill_viridis(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Years of Potential Life Lost Rate by County",
    fill = "YPLL Rate"
  ) +
  theme_minimal()

# make new datatable with counts for heatmap viewing
library(purrr)

columns_to_count<- c(
  "City",
  "County",
  "Data.Shape",
  "Seasons",
  "Dates.Sighted.Year",
  "Date.Sighted.Day",
  "Dates.Sighted.Month"
)

ufo_data_counts <- map(
  set_names(columns_to_count),
  ~ ufo_data %>%
    count(.data[[.x]], name = "count") %>%
    rename(value = 1)
)

#visualizations
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)    # for county shapefiles
library(viridis)
options(tigris_use_cache = TRUE)

# Load all US counties, then filter to California
counties_sf <- counties(cb = TRUE, year = 2023) %>%
  filter(STATEFP == "06")  # CA FIPS code

# rename counts tibble with helpful column names
county_counts <- ufo_data_counts$County %>%
  rename(County = value, sightings = count)

#join counts to shapefile
ca_map <- counties_sf %>%
  left_join(county_counts, by = c("NAME" = "County"))

# make county names match = "county added into counts not helping"
library(stringr)

county_counts <- county_counts %>%
  mutate(County = str_remove(County, " County$"))

#rejoin 
ca_map <- counties_sf %>%
  left_join(county_counts, by = c("NAME" = "County"))

# plot

ggplot(ca_map) +
  geom_sf(aes(fill = sightings), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  labs(
    title = "UFO Sightings per County (California)",
    fill = "Sightings"
  ) +
  theme_minimal()

# plot with numbers
# Plot heatmap with labels
ggplot(ca_map) +
  geom_sf(aes(fill = sightings), color = "white", size = 0.2) +
  geom_sf_text(aes(label = sightings), size = 3, color = "pink") +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  labs(
    title = "UFO Sightings per County (California)",
    fill = "Sightings"
  ) +
  theme_minimal()
