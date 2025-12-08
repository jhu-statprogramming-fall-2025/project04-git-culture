# ===============================
# Libraries
# ===============================
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tigris)
library(tidycensus)

#NEON THEME
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
    
    # Add neon gridlines
    panel.grid.major  = element_line(color = "#39FF14", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor  = element_line(color = "#39FF1450", linewidth = 0.1),
    
    # Optional: remove panel border for cleaner neon effect
    panel.border = element_blank()
  )


options(tigris_use_cache = TRUE)

# Set Census API key (for per-capita calculations)
census_api_key("d9bce7570759aab4c11d00139db4d5bcec7e06e7", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# ===============================
# Load and prepare data
df <- read_csv("nuforc_county_weather_bigfoot.csv") %>%
  mutate(
    County = str_trim(County),
    County = str_to_title(County),
    County = if_else(str_detect(County, "County$"), County, paste0(County, " County"))
  )

# Check what states are in the data
cat("States in dataset:", paste(unique(df$Location.State), collapse = ", "), "\n")
cat("Number of CA rows:", sum(df$Location.State == "CA", na.rm = TRUE), "\n")

# Filter for California only
df_ca <- df %>% 
  filter(Location.State == "CA") %>%
  # Remove any rows with missing county
  filter(!is.na(County), County != "")

cat("CA counties in dataset:", length(unique(df_ca$County)), "\n")
cat("Sample CA counties:", paste(head(unique(df_ca$County)), collapse = ", "), "\n")

# Load CA counties shapefile
ca_counties <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  mutate(NAME = str_to_title(NAME))

cat("Counties in shapefile:", length(unique(ca_counties$NAME)), "\n")
cat("Sample shapefile counties:", paste(head(ca_counties$NAME), collapse = ", "), "\n")

# ===============================
#Check why counties don't match

# Check for exact matches
common_counties <- intersect(unique(df_ca$County), ca_counties$NAME)
cat("\nCommon counties (exact match):", length(common_counties), "\n")
if (length(common_counties) > 0) {
  cat("Sample:", paste(head(common_counties), collapse = ", "), "\n")
}

missing_in_shapefile <- setdiff(unique(df_ca$County), ca_counties$NAME)
missing_in_data <- setdiff(ca_counties$NAME, unique(df_ca$County))

cat("\nCounties in data but not in shapefile:", length(missing_in_shapefile), "\n")
if (length(missing_in_shapefile) > 0 && length(missing_in_shapefile) <= 10) {
  cat("Missing:", paste(missing_in_shapefile, collapse = ", "), "\n")
}

cat("\nCounties in shapefile but not in data:", length(missing_in_data), "\n")
if (length(missing_in_data) > 0 && length(missing_in_data) <= 10) {
  cat("Missing:", paste(head(missing_in_data), collapse = ", "), "\n")
}

# ===============================
# Fix

# Clean county names in both datasets
clean_county_name <- function(name) {
  name %>%
    str_trim() %>%
    str_to_title() %>%
    # Remove any extra words like "Parish" or "Borough" (not in CA, but just in case)
    str_remove(" Parish$") %>%
    str_remove(" Borough$") %>%
    # Ensure "County" suffix
    if_else(str_detect(., "County$"), ., paste0(., " County"))
}

# Apply cleaning
df_ca <- df_ca %>%
  mutate(County = clean_county_name(County))

ca_counties <- ca_counties %>%
  mutate(NAME = clean_county_name(NAME))

# Check again after cleaning
common_counties <- intersect(unique(df_ca$County), ca_counties$NAME)
cat("\nAfter cleaning - Common counties:", length(common_counties), "\n")

# If still no matches, check the actual values
if (length(common_counties) == 0) {
  cat("\nDEBUG: First 10 counties in df_ca:\n")
  print(head(unique(df_ca$County), 10))
  cat("\nDEBUG: First 10 counties in shapefile:\n")
  print(head(ca_counties$NAME, 10))
}

# ===============================
# Map 1: Original map (non per-capita, standard)
# Aggregate precipitation
county_precip <- df_ca %>%
  group_by(County) %>%
  summarize(Avg_Precip = mean(Avg_Precip_Month_County, na.rm = TRUE), .groups = "drop")

cat("\nPrecipitation data for", nrow(county_precip), "counties\n")
print(head(county_precip))

# Join map with precipitation
ca_map <- ca_counties %>%
  left_join(county_precip, by = c("NAME" = "County"))

cat("\nMap data summary:\n")
cat("Rows in ca_map:", nrow(ca_map), "\n")
cat("Rows with precipitation data:", sum(!is.na(ca_map$Avg_Precip)), "\n")

# Check if we have any precipitation data
if (sum(!is.na(ca_map$Avg_Precip)) > 0) {
  # Create palette with valid values only
  valid_precip <- ca_map$Avg_Precip[!is.na(ca_map$Avg_Precip)]
  precip_pal <- colorNumeric("Blues", domain = valid_precip)
  
  # Create original map
  orig_map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    
    # Base precipitation polygons
    addPolygons(
      data = ca_map,
      fillColor = ~ifelse(is.na(Avg_Precip), "#f0f0f0", precip_pal(Avg_Precip)),
      fillOpacity = 0.6,
      color = "white",
      weight = 1,
      smoothFactor = 0.3,
      group = "Precipitation Map",
      label = ~paste0(NAME, ": ", 
                      ifelse(is.na(Avg_Precip), "No data", 
                             paste0(round(Avg_Precip, 2), " in")))
    ) %>%
    
    addLegend(
      "bottomleft",
      pal = precip_pal,
      values = valid_precip,
      title = "Avg Monthly Precipitation (in)",
      na.label = "No data"
    )
} else {
  # Create map without precipitation coloring if no data
  orig_map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    
    addPolygons(
      data = ca_map,
      fillColor = "#f0f0f0",
      fillOpacity = 0.6,
      color = "white",
      weight = 1,
      smoothFactor = 0.3,
      group = "Precipitation Map",
      label = ~paste0(NAME, ": No precipitation data")
    ) %>%
    
    addLegend(
      "bottomleft",
      colors = "#f0f0f0",
      labels = "No precipitation data",
      title = "Avg Monthly Precipitation"
    )
}

# Add heatmaps and points (check if we have coordinates)
if (nrow(df_ca %>% filter(sighting_type == "UFO", !is.na(lon), !is.na(lat))) > 0) {
  orig_map <- orig_map %>%
    addHeatmap(
      data = df_ca %>% filter(sighting_type == "UFO", !is.na(lon), !is.na(lat)),
      lng = ~lon, lat = ~lat,
      radius = 25,
      blur = 30,
      max = 0.01,
      group = "UFO Heatmap"
    )
}

if (nrow(df_ca %>% filter(sighting_type == "Bigfoot", !is.na(lon), !is.na(lat))) > 0) {
  orig_map <- orig_map %>%
    addHeatmap(
      data = df_ca %>% filter(sighting_type == "Bigfoot", !is.na(lon), !is.na(lat)),
      lng = ~lon, lat = ~lat,
      radius = 25,
      blur = 30,
      max = 0.01,
      group = "Bigfoot Heatmap",
      gradient = c("0" = "black", "0.3" = "red", "0.7" = "orange", "1" = "yellow")
    )
}

# Add individual sightings as points
if (nrow(df_ca %>% filter(sighting_type == "UFO", !is.na(lon), !is.na(lat))) > 0) {
  orig_map <- orig_map %>%
    addCircleMarkers(
      data = df_ca %>% filter(sighting_type == "UFO", !is.na(lon), !is.na(lat)),
      lng = ~lon, lat = ~lat,
      radius = 3,
      color = "blue",
      fillOpacity = 0.7,
      label = ~paste("UFO in", County),
      group = "UFO Points"
    )
}

if (nrow(df_ca %>% filter(sighting_type == "Bigfoot", !is.na(lon), !is.na(lat))) > 0) {
  orig_map <- orig_map %>%
    addCircleMarkers(
      data = df_ca %>% filter(sighting_type == "Bigfoot", !is.na(lon), !is.na(lat)),
      lng = ~lon, lat = ~lat,
      radius = 3,
      color = "red",
      fillOpacity = 0.7,
      label = ~paste("Bigfoot in", County),
      group = "Bigfoot Points"
    )
}

# Add layer control
orig_map <- orig_map %>%
  addLayersControl(
    baseGroups = c("Precipitation Map"),
    overlayGroups = c("UFO Heatmap", "Bigfoot Heatmap", "UFO Points", "Bigfoot Points"),
    options = layersControlOptions(collapsed = FALSE)
  ) 
#callable function
print(orig_map)

# ===============================
# Prepare data for scatter plots

# Get population data for per-capita calculations
ca_pop <- get_acs(
  geography = "county",
  variables = "B01003_001",
  state = "CA",
  year = 2020,
  geometry = FALSE
) %>%
  mutate(
    County = str_remove(NAME, ", California"),
    County = clean_county_name(County)
  ) %>%
  select(County, population = estimate)

# Count sightings per county
ufo_counts <- df_ca %>%
  filter(sighting_type == "UFO") %>%
  count(County, name = "UFO_Count")

bigfoot_counts <- df_ca %>%
  filter(sighting_type == "Bigfoot") %>%
  count(County, name = "Bigfoot_Count")

# Combine all data
county_data <- county_precip %>%
  full_join(ca_pop, by = "County") %>%  # Use full_join to include all counties
  left_join(ufo_counts, by = "County") %>%
  left_join(bigfoot_counts, by = "County") %>%
  mutate(
    UFO_Count = replace_na(UFO_Count, 0),
    Bigfoot_Count = replace_na(Bigfoot_Count, 0),
    UFO_per_100k = if_else(!is.na(population) & population > 0, 
                           UFO_Count / population * 100000, 
                           NA_real_),
    Bigfoot_per_100k = if_else(!is.na(population) & population > 0, 
                               Bigfoot_Count / population * 100000, 
                               NA_real_)
  )

cat("\n=== COUNTY DATA SUMMARY ===\n")
cat("Total counties with data:", nrow(county_data), "\n")
cat("Counties with precipitation data:", sum(!is.na(county_data$Avg_Precip)), "\n")
cat("Counties with population data:", sum(!is.na(county_data$population)), "\n")
cat("Counties with UFO sightings:", sum(county_data$UFO_Count > 0), "\n")
cat("Counties with Bigfoot sightings:", sum(county_data$Bigfoot_Count > 0), "\n")

# ===============================
# Scatter Plot 1: Non per-capita
# UFO vs Precipitation (raw counts) - only counties with data
plot1_data <- county_data %>% 
  filter(!is.na(Avg_Precip), UFO_Count > 0)

if (nrow(plot1_data) > 0) {
  p1 <- ggplot(plot1_data, aes(x = Avg_Precip, y = UFO_Count)) +
    geom_point(color = "blue", size = 3, alpha = 0.7) +
    labs(
      title = "UFO Sightings vs Avg County Precipitation (Raw Counts)",
      subtitle = paste("California Counties with UFO sightings (n =", nrow(plot1_data), ")"),
      x = "Avg Monthly Precipitation (inches)",
      y = "Number of UFO Sightings"
    ) +
    neon_theme
  print(p1)
} else {
  cat("\nNo data for UFO vs Precipitation scatter plot\n")
}

# UFO vs Bigfoot (raw counts) - only counties with both
plot2_data <- county_data %>% 
  filter(Bigfoot_Count > 0, UFO_Count > 0)

if (nrow(plot2_data) > 0) {
  p2 <- ggplot(plot2_data, aes(x = UFO_Count, y = Bigfoot_Count)) +
    geom_point(color = "purple", size = 3, alpha = 0.7) +
    labs(
      title = "UFO vs Bigfoot Sightings by County (Raw Counts)",
      subtitle = paste("Counties with both UFO and Bigfoot sightings (n =", nrow(plot2_data), ")"),
      x = "Number of UFO Sightings",
      y = "Number of Bigfoot Sightings"
    ) +
    neon_theme
  print(p2)
} else {
  cat("\nNo counties with both UFO and Bigfoot sightings\n")
}

# ===============================
# Scatter Plot 2: Per-capita with regression and R²

# UFO per 100k vs Precipitation
plot3_data <- county_data %>% 
  filter(!is.na(UFO_per_100k), !is.na(Avg_Precip), UFO_per_100k > 0)

if (nrow(plot3_data) > 1) {
  # Calculate regression statistics
  fit1 <- lm(UFO_per_100k ~ Avg_Precip, data = plot3_data)
  r2_1 <- round(summary(fit1)$r.squared, 3)
  coef1 <- round(coef(fit1)[2], 3)
  p_val1 <- round(summary(fit1)$coefficients[2, 4], 4)
  
  p3 <- ggplot(plot3_data, aes(x = Avg_Precip, y = UFO_per_100k)) +
    geom_point(color = "darkblue", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
    labs(
      title = "UFO Sightings per 100k vs Avg County Precipitation",
      subtitle = paste("California Counties (n =", nrow(plot3_data), 
                       ") | R² =", r2_1, "| Slope =", coef1, 
                       ifelse(p_val1 < 0.05, " (p < 0.05)", paste0(" (p = ", p_val1, ")"))),
      x = "Avg Monthly Precipitation (inches)",
      y = "UFO Sightings per 100k"
    ) +
    neon_theme
  print(p3)
} else {
  cat("\nInsufficient data for UFO per-capita vs Precipitation regression\n")
}

# UFO per 100k vs Bigfoot per 100k
plot4_data <- county_data %>% 
  filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k), 
         Bigfoot_per_100k > 0, UFO_per_100k > 0)

if (nrow(plot4_data) > 1) {
  # Calculate regression statistics
  fit2 <- lm(Bigfoot_per_100k ~ UFO_per_100k, data = plot4_data)
  r2_2 <- round(summary(fit2)$r.squared, 3)
  coef2 <- round(coef(fit2)[2], 3)
  p_val2 <- round(summary(fit2)$coefficients[2, 4], 4)
  
  p4 <- ggplot(plot4_data, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
    geom_point(color = "darkviolet", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.2) +
    labs(
      title = "UFO vs Bigfoot Sightings per 100k by County",
      subtitle = paste("Counties with both sightings (n =", nrow(plot4_data),
                       ") | R² =", r2_2, "| Slope =", coef2,
                       ifelse(p_val2 < 0.05, " (p < 0.05)", paste0(" (p = ", p_val2, ")"))),
      x = "UFO Sightings per 100k",
      y = "Bigfoot Sightings per 100k"
    ) +
    neon_theme
  print(p4)
} else {
  cat("\nInsufficient data for UFO vs Bigfoot per-capita regression\n")
}

# ===============================
# Summary statistics
# ===============================
cat("\n=== FINAL SUMMARY ===\n")
cat("Total CA counties in analysis:", nrow(county_data), "\n")
cat("Mean precipitation (inches):", round(mean(county_data$Avg_Precip, na.rm = TRUE), 2), "\n")
cat("Total UFO sightings in CA:", sum(county_data$UFO_Count, na.rm = TRUE), "\n")
cat("Total Bigfoot sightings in CA:", sum(county_data$Bigfoot_Count, na.rm = TRUE), "\n")
cat("Mean UFO count per county:", round(mean(county_data$UFO_Count, na.rm = TRUE), 1), "\n")
cat("Mean Bigfoot count per county:", round(mean(county_data$Bigfoot_Count, na.rm = TRUE), 1), "\n")

if (sum(!is.na(county_data$UFO_per_100k)) > 0) {
  cat("Mean UFO per 100k:", round(mean(county_data$UFO_per_100k, na.rm = TRUE), 2), "\n")
}
if (sum(!is.na(county_data$Bigfoot_per_100k)) > 0) {
  cat("Mean Bigfoot per 100k:", round(mean(county_data$Bigfoot_per_100k, na.rm = TRUE), 2), "\n")
}

#-------------------------
# ===============================
# Regression

# First, let's look at the data for the regression
reg_data <- county_data %>% 
  filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k), 
         Bigfoot_per_100k > 0, UFO_per_100k > 0)

cat("\n=== REGRESSION DATA DETAILS ===\n")
cat("Number of counties in regression:", nrow(reg_data), "\n")
print(reg_data %>% 
        select(County, Avg_Precip, UFO_per_100k, Bigfoot_per_100k, population) %>%
        arrange(desc(UFO_per_100k)))

# Check for outliers using the IQR method
ufo_q <- quantile(reg_data$UFO_per_100k, probs = c(0.25, 0.75), na.rm = TRUE)
bf_q <- quantile(reg_data$Bigfoot_per_100k, probs = c(0.25, 0.75), na.rm = TRUE)

ufo_iqr <- ufo_q[2] - ufo_q[1]
bf_iqr <- bf_q[2] - bf_q[1]

ufo_outliers <- reg_data %>%
  filter(UFO_per_100k < (ufo_q[1] - 1.5 * ufo_iqr) | 
           UFO_per_100k > (ufo_q[2] + 1.5 * ufo_iqr))

bf_outliers <- reg_data %>%
  filter(Bigfoot_per_100k < (bf_q[1] - 1.5 * bf_iqr) | 
           Bigfoot_per_100k > (bf_q[2] + 1.5 * bf_iqr))

cat("\nUFO per 100k outliers:", nrow(ufo_outliers), "counties\n")
cat("Bigfoot per 100k outliers:", nrow(bf_outliers), "counties\n")

# Let's also calculate correlation without potential outliers
correlation <- cor(reg_data$UFO_per_100k, reg_data$Bigfoot_per_100k, 
                   use = "complete.obs")
cat("\nPearson correlation:", round(correlation, 3), "\n")

# ===============================
# Visualization alternatives

# Plot 1: With regression line
p4a <- ggplot(reg_data, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
  geom_point(color = "darkviolet", size = 4, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.2) +
  geom_text(aes(label = County), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(
    title = "UFO vs Bigfoot Sightings per 100k (with county labels)",
    subtitle = paste("n =", nrow(reg_data), "| Shows why R² can be misleading"),
    x = "UFO Sightings per 100k",
    y = "Bigfoot Sightings per 100k"
  ) +
  theme_minimal()
print(p4a)

# Plot 2: Log-transformed to reduce outlier influence
if (all(reg_data$UFO_per_100k > 0) && all(reg_data$Bigfoot_per_100k > 0)) {
  reg_data_log <- reg_data %>%
    mutate(
      log_UFO = log10(UFO_per_100k + 1),
      log_Bigfoot = log10(Bigfoot_per_100k + 1)
    )
  
  fit_log <- lm(log_Bigfoot ~ log_UFO, data = reg_data_log)
  r2_log <- round(summary(fit_log)$r.squared, 3)
  
  p4b <- ggplot(reg_data_log, aes(x = log_UFO, y = log_Bigfoot)) +
    geom_point(color = "darkorange", size = 4, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "orange", alpha = 0.2) +
    geom_text(aes(label = County), vjust = -0.5, hjust = 0.5, size = 3) +
    labs(
      title = "Log-transformed: UFO vs Bigfoot per 100k",
      subtitle = paste("Log10 transformation reduces outlier influence | R² =", r2_log),
      x = "log10(UFO Sightings per 100k + 1)",
      y = "log10(Bigfoot Sightings per 100k + 1)"
    ) +
    neon_theme
  print(p4b)
}

# Plot 3: Non-parametric smoothing (LOESS)
p4c <- ggplot(reg_data, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
  geom_point(color = "darkgreen", size = 4, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "green", alpha = 0.2) +
  labs(
    title = "UFO vs Bigfoot per 100k with LOESS smoothing",
    subtitle = "Non-parametric fit (less influenced by outliers)",
    x = "UFO Sightings per 100k",
    y = "Bigfoot Sightings per 100k"
  ) +
  theme_minimal()
print(p4c)

# Plot 4: Check distribution of variables
p4d <- ggplot(reg_data, aes(x = UFO_per_100k)) +
  geom_histogram(fill = "blue", alpha = 0.7, bins = 20) +
  labs(title = "Distribution of UFO per 100k", x = "UFO per 100k", y = "Count") +
  theme_minimal()

p4e <- ggplot(reg_data, aes(x = Bigfoot_per_100k)) +
  geom_histogram(fill = "red", alpha = 0.7, bins = 20) +
  labs(title = "Distribution of Bigfoot per 100k", x = "Bigfoot per 100k", y = "Count") +
  theme_minimal()

# Use patchwork if available, otherwise print separately
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  print(p4d + p4e)
} else {
  print(p4d)
  print(p4e)
}

# ===============================
# More robust statistical analysis
# ===============================

cat("\n=== ROBUST REGRESSION ANALYSIS ===\n")

# 1. Original OLS regression
fit_ols <- lm(Bigfoot_per_100k ~ UFO_per_100k, data = reg_data)
cat("OLS R²:", round(summary(fit_ols)$r.squared, 3), "\n")

# 2. Check Cook's distance for influential points
cooks_d <- cooks.distance(fit_ols)
influential <- which(cooks_d > 4/nrow(reg_data))  # Common threshold
cat("Influential points (Cook's distance > 4/n):", 
    ifelse(length(influential) > 0, paste(influential, collapse = ", "), "None"), "\n")

if (length(influential) > 0) {
  cat("Influential counties:", paste(reg_data$County[influential], collapse = ", "), "\n")
  
  # 3. Regression without influential points
  if (nrow(reg_data[-influential, ]) > 1) {
    fit_robust <- lm(Bigfoot_per_100k ~ UFO_per_100k, data = reg_data[-influential, ])
    cat("OLS R² without influential points:", round(summary(fit_robust)$r.squared, 3), "\n")
  }
}

# 4. Non-parametric correlation (Spearman's rank)
spearman_cor <- cor(reg_data$UFO_per_100k, reg_data$Bigfoot_per_100k, 
                    method = "spearman", use = "complete.obs")
cat("Spearman's rank correlation:", round(spearman_cor, 3), "\n")

# 5. Check if relationship is significant with permutation test
permute_test <- function(x, y, n_perm = 1000) {
  observed_cor <- cor(x, y, use = "complete.obs")
  perm_cors <- replicate(n_perm, {
    cor(x, sample(y), use = "complete.obs")
  })
  p_value <- mean(abs(perm_cors) >= abs(observed_cor))
  return(list(observed = observed_cor, p_value = p_value))
}

perm_result <- permute_test(reg_data$UFO_per_100k, reg_data$Bigfoot_per_100k, n_perm = 1000)
cat("Permutation test p-value:", round(perm_result$p_value, 4), "\n")

# ===============================
# Revised scatter plot with better statistics
# ===============================

# Use Spearman correlation for the subtitle
spearman_label <- paste("Spearman's ρ =", round(spearman_cor, 3))

# Create a better plot that shows the issue
p4_revised <- ggplot(reg_data, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
  geom_point(aes(size = population), color = "darkviolet", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.2, 
              linetype = "dashed", linewidth = 0.8) +
  # Add county labels for the most extreme points
  geom_text(data = reg_data %>% 
              arrange(desc(UFO_per_100k + Bigfoot_per_100k)) %>% 
              head(3),
            aes(label = County), vjust = -0.8, hjust = 0.5, size = 3, fontface = "bold") +
  labs(
    title = "UFO vs Bigfoot Sightings per 100k Population",
    subtitle = paste("n =", nrow(reg_data), "CA counties with both sightings |", 
                     "OLS R² =", round(summary(fit_ols)$r.squared, 3), "|", spearman_label),
    x = "UFO Sightings per 100k Population",
    y = "Bigfoot Sightings per 100k Population",
    size = "County Population"
  ) +
  scale_size_continuous(range = c(3, 8)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4_revised)

# ===============================
# Final interpretation
# ===============================
cat("\n=== INTERPRETATION ===\n")
cat("A high R² (0.86) with poor visual fit suggests:\n")
cat("1. You have very few data points (small n)\n")
cat("2. Some counties are extreme outliers in both UFO and Bigfoot sightings\n")
cat("3. These outliers dominate the regression calculation\n")
cat("4. Consider reporting Spearman's correlation instead: ρ =", round(spearman_cor, 3), "\n")
cat("5. Or use a log transformation to reduce outlier influence\n")

# ===============================
# Scatter plot functions
# ===============================

# 1. Bigfoot vs UFO (raw counts)
plot_bigfoot_vs_ufo <- function(data) {
  df <- data %>% filter(Bigfoot_Count > 0, UFO_Count > 0)
  if (nrow(df) < 1) return(NULL)
  
  ggplot(df, aes(x = UFO_Count, y = Bigfoot_Count)) +
    geom_point(color = "purple", size = 3, alpha = 0.7) +
    labs(
      title = "UFO vs Bigfoot Sightings by County (Raw Counts)",
      x = "Number of UFO Sightings",
      y = "Number of Bigfoot Sightings"
    ) +
    neon_theme
}

# 2. UFO vs Precipitation (raw counts)
plot_ufo_vs_precip <- function(data) {
  df <- data %>% filter(!is.na(Avg_Precip), UFO_Count > 0)
  if (nrow(df) < 1) return(NULL)
  
  ggplot(df, aes(x = Avg_Precip, y = UFO_Count)) +
    geom_point(color = "blue", size = 3, alpha = 0.7) +
    labs(
      title = "UFO Sightings vs Avg County Precipitation",
      x = "Avg Monthly Precipitation (inches)",
      y = "Number of UFO Sightings"
    ) +
    neon_theme
}

# 3. Bigfoot vs UFO per 100k
plot_bigfoot_vs_ufo_percap <- function(data) {
  df <- data %>% filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k), 
                        UFO_per_100k > 0, Bigfoot_per_100k > 0)
  if (nrow(df) < 2) return(NULL)
  
  fit <- lm(Bigfoot_per_100k ~ UFO_per_100k, data = df)
  r2 <- round(summary(fit)$r.squared, 3)
  
  ggplot(df, aes(x = UFO_per_100k, y = Bigfoot_per_100k)) +
    geom_point(color = "darkviolet", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "purple", alpha = 0.2) +
    labs(
      title = "UFO vs Bigfoot per 100k by County",
      subtitle = paste("OLS R² =", r2),
      x = "UFO Sightings per 100k",
      y = "Bigfoot Sightings per 100k"
    ) +
    neon_theme
}

# 4. UFO vs Precipitation per 100k
plot_ufo_vs_precip_percap <- function(data) {
  df <- data %>% filter(!is.na(UFO_per_100k), !is.na(Avg_Precip), UFO_per_100k > 0)
  if (nrow(df) < 2) return(NULL)
  
  fit <- lm(UFO_per_100k ~ Avg_Precip, data = df)
  r2 <- round(summary(fit)$r.squared, 3)
  
  ggplot(df, aes(x = Avg_Precip, y = UFO_per_100k)) +
    geom_point(color = "darkblue", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.2) +
    labs(
      title = "UFO Sightings per 100k vs Avg County Precipitation",
      subtitle = paste("OLS R² =", r2),
      x = "Avg Monthly Precipitation (inches)",
      y = "UFO Sightings per 100k"
    ) +
    neon_theme
}

# 5. Log-transformed Bigfoot vs UFO per 100k
plot_log_bigfoot_vs_ufo <- function(data) {
  df <- data %>% filter(!is.na(UFO_per_100k), !is.na(Bigfoot_per_100k),
                        UFO_per_100k > 0, Bigfoot_per_100k > 0) %>%
    mutate(log_UFO = log10(UFO_per_100k + 1),
           log_Bigfoot = log10(Bigfoot_per_100k + 1))
  
  if (nrow(df) < 2) return(NULL)
  
  fit <- lm(log_Bigfoot ~ log_UFO, data = df)
  r2 <- round(summary(fit)$r.squared, 3)
  
  ggplot(df, aes(x = log_UFO, y = log_Bigfoot)) +
    geom_point(color = "darkorange", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "orange", alpha = 0.2) +
    labs(
      title = "Log-transformed: UFO vs Bigfoot per 100k",
      subtitle = paste("Log10 transformation reduces outlier influence | R² =", r2),
      x = "log10(UFO per 100k + 1)",
      y = "log10(Bigfoot per 100k + 1)"
    ) +
    neon_theme
}

plot_bigfoot_vs_ufo(county_data)
plot_ufo_vs_precip(county_data)
plot_bigfoot_vs_ufo_percap(county_data)
plot_ufo_vs_precip_percap(county_data)
plot_log_bigfoot_vs_ufo(county_data)
print(orig_map)
