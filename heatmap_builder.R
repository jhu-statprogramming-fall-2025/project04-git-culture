#2025.12.09 
#plot builder for heatmaps

 # libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggridges)
library(wordcloud)
library(tm)
library(stringr)
library(tigris)
library(sf)
library(viridis)

caliYPLL <- read.csv("county_health_data/combined_california_ypll.csv")
nuforc <- read.csv("nuforc_county_weather_bigfoot.csv")

options(tigris_use_cache = TRUE)

# 1. Load US county shapefiles
counties_sf <- counties(cb = TRUE, year = 2023)  # cb = cartographic boundaries

# 2. Make sure FIPS codes match format (zero-padded 5-digit strings)
caliYPLL$FIPS <- sprintf("%05d", as.numeric(caliYPLL$FIPS))

# 3. Join your data to the map data using FIPS
county_map_df <- counties_sf %>%
  mutate(FIPS = GEOID) %>%
  left_join(caliYPLL, by = "FIPS")

# 4. Plot choropleth
YPLL_plot <- ggplot(county_map_df %>% filter(STATEFP == "06")) +
  geom_sf(aes(fill = YPLL_Rate), color = NA) +     # NA removes borders for cleaner heat map
  scale_fill_viridis(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Years of Potential Life Lost Rate by County",
    fill = "YPLL Rate"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

# 1. Clean and aggregate
ufo_by_county_clean <- ufo_data %>%
  filter(Location.State == "CA") %>%
  mutate(
    County = str_replace(County, " County$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # optional: match shapefile
  ) %>%
  group_by(County) %>%
  summarise(sightings = n(), .groups = "drop")    # aggregate count

# 2. Clean shapefile
library(tigris)
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))             # lowercase to match

# 3. Join aggregated counts to map
map_df_sightings <- counties_sf %>%
  left_join(ufo_by_county_clean, by = "County")

# 4. Plot
library(ggplot2)

plot_sightings <- ggplot(map_df_sightings) +
  geom_sf(aes(fill = sightings), color = NA) +
  geom_sf_text(aes(label = sightings), size = 3, color = "black") +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California",
    fill = "Sightings"
  ) +
  theme_minimal()
```

```{r average precip heat map, echo=FALSE}
# 1. Load California counties shapefile
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))  # lowercase for consistent matching

# 2. Clean UFO data and keep only CA
ufo_by_precip_clean <- ufo_data %>%
  filter(Location.State == "CA") %>%
  mutate(
    County = str_replace(County, " COUNTY$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # lowercase to match shapefile
  )

# 3. Aggregate average precipitation by county
precip_by_county <- ufo_by_precip_clean %>%
  group_by(County) %>%
  summarise(
    Avg_Precip = mean(Avg_Precip_Month_County, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Join aggregated data to map
map_df_precip <- counties_sf %>%
  left_join(precip_by_county, by = "County")

# 5. Plot choropleth with values
precip_plot <- ggplot(map_df_precip) +
  geom_sf(aes(fill = Avg_Precip), color = NA) +
  #geom_sf_text(aes(label = round(Avg_Precip, 1)), size = 3, color = "black") +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Average Monthly Precipitation by County (CA)",
    fill = "Avg Precip"
  ) +
  theme_minimal()
```

```{r year 2014, echo=FALSE}
# 1. Load California counties shapefile
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))  # lowercase for consistent matching

# 2. Filter UFO data for 2014 and clean county names
ufo_2014 <- ufo_data %>%
  filter(Location.State == "CA", Dates.Sighted.Year == 2014) %>%
  mutate(
    County = str_replace(County, " COUNTY$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # lowercase to match shapefile
  )

# 3. Aggregate sightings by county
sightings_2014_by_county <- ufo_2014 %>%
  group_by(County) %>%
  summarise(sightings = n(), .groups = "drop")

# 4. Join with shapefile
map_df_2014 <- counties_sf %>%
  left_join(sightings_2014_by_county, by = "County")

# 5. Plot choropleth with counts
sightings_2014_plot <- ggplot(map_df_2014) +
  geom_sf(aes(fill = sightings), color = NA) +
  geom_sf_text(aes(label = sightings), size = 3, color = "black", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2014)",
    fill = "Sightings"
  ) +
  theme_minimal()
```

```{r year 2013, echo=FALSE}
# 1. Load California counties shapefile
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))  # lowercase for consistent matching

# 2. Filter UFO data for 2013 and clean county names
ufo_2013 <- ufo_data %>%
  filter(Location.State == "CA", Dates.Sighted.Year == 2013) %>%
  mutate(
    County = str_replace(County, " COUNTY$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # lowercase to match shapefile
  )

# 3. Aggregate sightings by county
sightings_2013_by_county <- ufo_2013 %>%
  group_by(County) %>%
  summarise(sightings = n(), .groups = "drop")

# 4. Join with shapefile
map_df_2013 <- counties_sf %>%
  left_join(sightings_2013_by_county, by = "County")

# 5. Plot choropleth with counts
sightings_2013_plot <- ggplot(map_df_2013) +
  geom_sf(aes(fill = sightings), color = NA) +
  geom_sf_text(aes(label = sightings), size = 3, color = "black", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  theme_minimal()
```