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

############################  #############################

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
  neon_theme

YPLL_plot

ggsave(
  "YPLL_plots.png", 
  plot = YPLL_plot,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################  #############################

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
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))             # lowercase to match

# 3. Join aggregated counts to map
map_df_sightings <- counties_sf %>%
  left_join(ufo_by_county_clean, by = "County")

# 4. Plot
plot_sightings <- ggplot(map_df_sightings) +
  geom_sf(aes(fill = sightings), color = NA) +
  geom_sf_text(aes(label = sightings), size = 3, color = "green") +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California",
    fill = "Sightings"
  ) +
  neon_theme

plot_sightings

ggsave(
  "plot_sightings.png", 
  plot = plot_sightings,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################ precipitation #############################

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
  neon_theme

precip_plot

ggsave(
  "precip_plot.png", 
  plot = precip_plot,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)
############################ temperature #############################
# 2. Clean UFO data and keep only CA
ufo_by_temp_clean <- ufo_data %>%
  filter(Location.State == "CA") %>%
  mutate(
    County = str_replace(County, " COUNTY$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # lowercase to match shapefile
  )

# 3. Aggregate average temperature by county
temp_by_county <- ufo_by_temp_clean %>%
  group_by(County) %>%
  summarise(
    Avg_Temp = mean(Avg_Temp_Month_County, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Join aggregated data to map
map_df_temp<- counties_sf %>%
  left_join(temp_by_county, by = "County")

# 5. Plot choropleth with values
temp_plot <- ggplot(map_df_temp) +
  geom_sf(aes(fill = Avg_Temp), color = NA) +
  #geom_sf_text(aes(label = round(Avg_Precip, 1)), size = 3, color = "black") +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "Average Monthly Temperature by County (CA)",
    fill = "Avg Temp"
  ) +
  neon_theme

temp_plot

ggsave(
  "temp_plot.png", 
  plot = temp_plot,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################ 2014 sightings #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2014)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2014_plot

ggsave(
  "sightings_2014_plot.png", 
  plot = sightings_2014_plot,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)


############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

ggsave(
  "sightings_2013_plot.png", 
  plot = sightings_2013_plot,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

############################ 2013 sightings  #############################

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
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2013)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2013_plot

############################ 2004 sightings  #############################

# 1. Load California counties shapefile
counties_sf <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(County = str_to_lower(NAME))  # lowercase for consistent matching

# 2. Filter UFO data for 2004 and clean county names
ufo_2004 <- ufo_data %>%
  filter(Location.State == "CA", Dates.Sighted.Year == 2004) %>%
  mutate(
    County = str_replace(County, " COUNTY$", ""),  # remove suffix
    County = str_trim(County),
    County = str_to_lower(County)                 # lowercase to match shapefile
  )

# 3. Aggregate sightings by county
sightings_2004_by_county <- ufo_2004 %>%
  group_by(County) %>%
  summarise(sightings = n(), .groups = "drop")

# 4. Join with shapefile
map_df_2004 <- counties_sf %>%
  left_join(sightings_2004_by_county, by = "County")

# 5. Plot choropleth with counts
sightings_2004_plot <- ggplot(map_df_2004) +
  geom_sf(aes(fill = sightings), color = NA) +
  geom_sf_text(aes(label = sightings), size = 3, color = "green", check_overlap = TRUE) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  labs(
    title = "UFO Sightings by County in California (2004)",
    fill = "Sightings"
  ) +
  neon_theme

sightings_2004_plot


