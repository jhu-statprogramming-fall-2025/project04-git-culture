# ===============================
# Libraries
# ===============================
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tigris)

# NEON THEME
neon_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "black", color = NA),
    panel.background  = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key        = element_rect(fill = "black", color = NA),
    text              = element_text(color = "white"),
    axis.text         = element_text(color = "white"),
    plot.title        = element_text(color = "white", face = "bold", size = 18),
    axis.title        = element_text(color = "white"),
    panel.grid.major  = element_line(color = "#39FF14", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor  = element_line(color = "#39FF1450", linewidth = 0.1),
    panel.border = element_blank()
  )

options(tigris_use_cache = TRUE)

# ---------------------
# Load and prepare data
df <- read_csv("nuforc_county_weather_bigfoot_with_census_population.csv") %>%
  mutate(
    County = str_trim(County),
    County = str_to_title(County),
    County = if_else(str_detect(County, "County$"), County, paste0(County, " County"))
  )

# Filter for California only
df_ca <- df %>% 
  filter(Location.State == "CA") %>%
  filter(!is.na(County), County != "")

ca_counties <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(NAME = str_to_title(NAME)) %>%
  mutate(NAME = if_else(str_detect(NAME, "County$"), NAME, paste0(NAME, " County")))

# Clean county names in data
df_ca <- df_ca %>%
  mutate(County = if_else(str_detect(County, "County$"), County, paste0(County, " County")))

# County level data
county_precip <- df_ca %>%
  group_by(County) %>%
  summarize(Avg_Precip = mean(Avg_Precip_Month_County, na.rm = TRUE), .groups = "drop")

ufo_counts <- df_ca %>%
  filter(sighting_type == "UFO") %>%
  count(County, name = "UFO_Count")

bigfoot_counts <- df_ca %>%
  filter(sighting_type == "Bigfoot") %>%
  count(County, name = "Bigfoot_Count")

county_pop <- df_ca %>%
  group_by(County) %>%
  summarize(population = first(population), .groups = "drop")

# Combine all county data
county_data <- county_precip %>%
  left_join(county_pop, by = "County") %>%
  left_join(ufo_counts, by = "County") %>%
  left_join(bigfoot_counts, by = "County") %>%
  mutate(
    UFO_Count = replace_na(UFO_Count, 0),
    Bigfoot_Count = replace_na(Bigfoot_Count, 0),
    UFO_per_100k = if_else(!is.na(population) & population > 0, 
                           UFO_Count / population * 100000, NA_real_),
    Bigfoot_per_100k = if_else(!is.na(population) & population > 0, 
                               Bigfoot_Count / population * 100000, NA_real_)
  )

# -----------------------------
# Map making
ca_map <- ca_counties %>%
  left_join(county_precip, by = c("NAME" = "County"))

#Blue color for precipitation
precip_pal <- colorNumeric("Blues", domain = county_data$Avg_Precip, na.color = "#f0f0f0")

# Create base map
orig_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = ca_map,
    fillColor = ~precip_pal(Avg_Precip),
    fillOpacity = 0.6,
    color = "white",
    weight = 1,
    smoothFactor = 0.3,
    group = "Precipitation Map",
    label = ~paste0(NAME, ": ", ifelse(is.na(Avg_Precip), "No data", 
                                       paste0(round(Avg_Precip, 2), " in")))
  ) %>%
  addLegend(
    "bottomleft",
    pal = precip_pal,
    values = county_data$Avg_Precip,
    title = "Avg Monthly Precipitation (in)",
    na.label = "No data"
  )

# Add UFO heatmap and points
ufo_data <- df_ca %>% filter(sighting_type == "UFO", !is.na(lon), !is.na(lat))
orig_map <- orig_map %>%
  addHeatmap(
    data = ufo_data,
    lng = ~lon, lat = ~lat,
    radius = 25,
    blur = 30,
    max = 0.01,
    group = "UFO Heatmap"
  ) %>%
  addCircleMarkers(
    data = ufo_data,
    lng = ~lon, lat = ~lat,
    radius = 3,
    color = "blue",
    fillOpacity = 0.7,
    label = ~paste("UFO in", County),
    group = "UFO Points"
  )

# Adding Bigfoot heatmap and points
bigfoot_data <- df_ca %>% filter(sighting_type == "Bigfoot", !is.na(lon), !is.na(lat))
orig_map <- orig_map %>%
  addHeatmap(
    data = bigfoot_data,
    lng = ~lon, lat = ~lat,
    radius = 25,
    blur = 30,
    max = 0.01,
    group = "Bigfoot Heatmap",
    gradient = c("0" = "black", "0.3" = "red", "0.7" = "orange", "1" = "yellow")
  ) %>%
  addCircleMarkers(
    data = bigfoot_data,
    lng = ~lon, lat = ~lat,
    radius = 3,
    color = "red",
    fillOpacity = 0.7,
    label = ~paste("Bigfoot in", County),
    group = "Bigfoot Points"
  ) %>%
  addLayersControl(
    baseGroups = c("Precipitation Map"),
    overlayGroups = c("UFO Heatmap", "Bigfoot Heatmap", "UFO Points", "Bigfoot Points"),
    options = layersControlOptions(collapsed = FALSE)
  )

# --------------------
# Scatterers

# 1. Bigfoot vs UFO (raw counts)
plot_bigfoot_vs_ufo <- ggplot(county_data %>% filter(Bigfoot_Count > 0, UFO_Count > 0), 
                              aes(x = UFO_Count, y = Bigfoot_Count)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +
  labs(
    title = "UFO vs Bigfoot Sightings by County (Raw Counts)",
    x = "Number of UFO Sightings",
    y = "Number of Bigfoot Sightings"
  ) +
  neon_theme

# 2. UFO vs Precipitation (raw counts)
plot_ufo_vs_precip <- ggplot(county_data %>% filter(UFO_Count > 0), 
                             aes(x = Avg_Precip, y = UFO_Count)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(
    title = "UFO Sightings vs Avg County Precipitation",
    x = "Avg Monthly Precipitation (inches)",
    y = "Number of UFO Sightings"
  ) +
  neon_theme

# 3. Bigfoot vs UFO per 100k
plot_bigfoot_vs_ufo_percap <- {
  plot_data <- county_data %>% 
    filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k), 
           UFO_per_100k > 0, Bigfoot_per_100k > 0)
  
  if (nrow(plot_data) > 1) {
    fit <- lm(Bigfoot_per_100k ~ UFO_per_100k, data = plot_data)
    r2 <- round(summary(fit)$r.squared, 3)
    
    ggplot(plot_data, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
      geom_point(color = "darkviolet", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.2) +
      labs(
        title = "UFO vs Bigfoot per 100k by County",
        subtitle = paste("R² =", r2),
        x = "UFO Sightings per 100k",
        y = "Bigfoot Sightings per 100k"
      ) +
      neon_theme
  }
}

# 4. UFO vs Precipitation per 100k
plot_ufo_vs_precip_percap <- {
  plot_data <- county_data %>% 
    filter(!is.na(UFO_per_100k), UFO_per_100k > 0)
  
  if (nrow(plot_data) > 1) {
    fit <- lm(UFO_per_100k ~ Avg_Precip, data = plot_data)
    r2 <- round(summary(fit)$r.squared, 3)
    
    ggplot(plot_data, aes(x = Avg_Precip, y = UFO_per_100k)) +
      geom_point(color = "darkblue", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
      labs(
        title = "UFO Sightings per 100k vs Avg County Precipitation",
        subtitle = paste("R² =", r2),
        x = "Avg Monthly Precipitation (inches)",
        y = "UFO Sightings per 100k"
      ) +
      neon_theme
  }
}

# 5. Log-transformed Bigfoot vs UFO per 100k
plot_log_bigfoot_vs_ufo <- {
  plot_data <- county_data %>% 
    filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k),
           UFO_per_100k > 0, Bigfoot_per_100k > 0) %>%
    mutate(
      log_UFO = log10(UFO_per_100k + 1),
      log_Bigfoot = log10(Bigfoot_per_100k + 1)
    )
  
  if (nrow(plot_data) > 1) {
    fit <- lm(log_Bigfoot ~ log_UFO, data = plot_data)
    r2 <- round(summary(fit)$r.squared, 3)
    
    ggplot(plot_data, aes(x = log_UFO, y = log_Bigfoot)) +
      geom_point(color = "darkorange", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "orange", alpha = 0.2) +
      labs(
        title = "Log-transformed: UFO vs Bigfoot per 100k",
        subtitle = paste("R² =", r2),
        x = "log10(UFO per 100k + 1)",
        y = "log10(Bigfoot per 100k + 1)"
      ) +
      neon_theme
  }
}

# ===============================
# Generate all plots
# ===============================
print(orig_map)
print(plot_bigfoot_vs_ufo)
print(plot_ufo_vs_precip)
