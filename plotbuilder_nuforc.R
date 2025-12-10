# Plot builder for ufology-dashboard 

# libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggridges)
library(wordcloud)
library(tm)
library(stringr)

#### WANT TO SET UP HERE PACKAGE TO AUTO PULL ALL FILES FROM REPO FOLDER? 

setwd("/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/")

# Read the files
df <- read_csv("nuforc_county_weather_bigfoot.csv")

# Filter UFO data & create year-month
ufo_data <- df %>%
  filter(sighting_type == "UFO") %>%
  mutate(
    sighted_date = as.Date(sighted_date),
    year_month = floor_date(sighted_date, "month")
  )
############################# neon theme  ############################# 

## Generating cool neon theme

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

############################# top 5 shapes ############################# 
# Top 5 shapes
top_shapes <- c("light", "circle", "triangle", "fireball", "sphere")

# We're looking at sightings per month 
monthly_total <- ufo_data %>%
  group_by(year_month) %>%
  summarise(monthly_count = n(), .groups = "drop") %>%
  arrange(year_month) %>%
  mutate(cumulative_total = cumsum(monthly_count))

# Top shapes
monthly_shapes <- ufo_data %>%
  filter(Data.Shape %in% top_shapes) %>%
  group_by(year_month, Data.Shape) %>%
  summarise(monthly_count = n(), .groups = "drop") %>%
  arrange(year_month, Data.Shape) %>%
  group_by(Data.Shape) %>%
  mutate(cumulative_count = cumsum(monthly_count)) %>%
  ungroup()

############################# plot: cumulative sightings over time ############################# 
#Cumulative sightings over time, neon theme
cumulative_sightings <- ggplot() +
  # Outer glow for total line
  geom_line(data = monthly_total,
            aes(x = year_month, y = cumulative_total),
            color = "#00FFFF", size = 3, alpha = 0.2) +
  # Core total line
  geom_line(data = monthly_total,
            aes(x = year_month, y = cumulative_total),
            color = "#00FFFF", size = 1) +
  
  # Shape-specific glowing lines
  geom_line(data = monthly_shapes,
            aes(x = year_month, y = cumulative_count, color = Data.Shape),
            size = 3, alpha = 0.15) +
  geom_line(data = monthly_shapes,
            aes(x = year_month, y = cumulative_count, color = Data.Shape),
            size = 1) +
  
  scale_color_manual(values = c(
    "light"    = "#00FFFF",
    "circle"   = "#FF00FF",
    "triangle" = "#39FF14",
    "fireball" = "#FF4500",
    "sphere"   = "#FFD700"
  )) +
  
  labs(
    title = "Cumulative UFO Sightings Over Time (Neon Style)",
    x = "Month",
    y = "Cumulative Sightings",
    color = "UFO Shape"
  ) +
  neon_theme
cumulative_sightings

ggsave(
  "cumulative_sightings.png", 
  plot = cumulative_sightings,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

#Cumulative sightings over time
UFO_shape_years <- ufo_data %>%
  ggplot(aes(x = year, fill = Data.Shape))+
  geom_bar()+
  theme_minimal()+
  labs(title="Shape over years?")+
  neon_theme

############################# plot: TOP COUNTIES ############################# 

#Neon theme hell yeah
top15_counties <- ufo_data %>%
  count(County, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(County, n), y = n)) +
  
  # Glow layer
  geom_col(fill = "#00FFFF", alpha = 0.25, width = 0.9) +
  # Core bar
  geom_col(fill = "#00FFFF", width = 0.6) +
  
  coord_flip() +
  labs(title = "Top 15 California Counties for UFO Sightings") +
  neon_theme

top15_counties

ggsave(
  "top15_counties.png", 
  plot = top15_counties,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)


#############################  plot:SEASONAL PATTERNS #############################  

## want to display months in box plots and use the pichart visual for the four seasons
### add column indicating season

# Create Seasons column
ufo_data <- ufo_data %>%
  mutate(Seasons = case_when(
    Dates.Sighted.Month %in% c(12, 1, 2) ~ "Winter",
    Dates.Sighted.Month %in% 3:5 ~ "Spring",
    Dates.Sighted.Month %in% 6:8 ~ "Summer",
    Dates.Sighted.Month %in% 9:11 ~ "Fall",
    TRUE ~ NA_character_  # in case of missing or invalid month
  ))
# create month name column
ufo_data <- ufo_data %>%
  mutate(Month.Name = case_when(
    Dates.Sighted.Month %in% 1 ~"Janaury",
    Dates.Sighted.Month %in% 2 ~ "February",
    Dates.Sighted.Month %in% 3 ~ "March",
    Dates.Sighted.Month %in% 4 ~ "April",
    Dates.Sighted.Month %in% 5 ~ "May",
    Dates.Sighted.Month %in% 6 ~ "June",
    Dates.Sighted.Month %in% 7 ~ "July",
    Dates.Sighted.Month %in% 8 ~ "August",
    Dates.Sighted.Month %in% 9 ~ "September",
    Dates.Sighted.Month %in% 10 ~ "October",
    Dates.Sighted.Month %in% 11 ~ "November",
    Dates.Sighted.Month %in% 12 ~ "December"
  ))

#Neon theme my man
seasonal_sightings <- ufo_data %>%
  count(Seasons) %>%
  ggplot(aes(x = as.factor(Seasons), y = n)) +
  geom_col(fill = "#FF00FF", alpha = 0.25, width = 1) +
  geom_col(fill = "#FF00FF", width = 0.7) +
  
  coord_polar() +
  labs(title = "Seasonal Pattern of UFO Sightings") +
  neon_theme

seasonal_sightings

ggsave(
  "seasonal_sightingss.png", 
  plot = seasonal_sightings,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

monthly_sightings <- ufo_data %>%
  group_by(Month.Name) %>%
  mutate(SightingNumber = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(Month.Name), y = SightingNumber, fill = as.factor(Month.Name))) +
  geom_boxplot(color = "yellow") +  # bright yellow outline
  labs(x = "Month", y = "Number of sightings",title = "Count of Sightings by Month") +
  theme_minimal() +
  neon_theme +
  theme(
    legend.position = "none",  
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

monthly_sightings

ggsave(
  "monthly_sightings.png", 
  plot = monthly_sightings,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################# DISTRIBUTION PLOTS #############################  
#Neon boy
UFOsightings_time_space <- ufo_data %>%
  filter(sighting_type == "UFO") %>%
  ggplot(aes(x = sighted_date, y = Data.Shape, fill = Data.Shape)) +
  
  geom_density_ridges(alpha = 0.8, color = "black") +
  
  labs(title = "Distribution of UFO Sightings Over Time by Shape",
       x = "Date", y = "Shape") +
  neon_theme+
  theme(legend.position = "none")

UFOsightings_time_space

ggsave(
  "UFOsightings_time_space.png", 
  plot = UFOsightings_time_space,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################# ENCOUNTER DURATION #############################  

#Neon
UFOencounter_duration_shape <- ufo_data %>%
  filter(sighting_type == "UFO") %>%
  group_by(Data.Shape) %>%
  summarise(mean_duration = mean(as.numeric(Data.Encounter.duration), na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Data.Shape, mean_duration), y = mean_duration)) +
  
  geom_col(fill = "#39FF14", alpha = 0.3) +
  geom_col(fill = "#39FF14", width = 0.6) +
  
  coord_flip() +
  labs(title = "Average Encounter Duration by UFO Shape",
       x = "Shape", y = "Mean Duration (seconds)") +
  neon_theme

UFOencounter_duration_shape

ggsave(
  "UFOencounter_duration_shape.png", 
  plot = UFOencounter_duration_shape,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################# WORDCLOUD #############################  
# Load required packages
library(tm)
library(wordcloud)

# Define the function to return a plot object
create_neon_wordcloud <- function(data_column, max_words = 100) {
  # Prepare the text
  text <- Corpus(VectorSource(data_column))
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, removeWords, stopwords("english"))
  
  # Define neon colors
  neon_colors <- c("#00FFFF", "#FF00FF", "#39FF14", "#FFD700", "#FF4500", "#00FF7F")
  
  # Open off-screen device
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 800, height = 600, bg = "black")
  
  # Draw wordcloud
  wordcloud(
    words = text,
    max.words = max_words,
    scale = c(3, 0.5),
    random.order = FALSE,
    rot.per = 0.25,
    colors = neon_colors
  )
  
  # Capture the plot
  wc_plot <- recordPlot()
  
  # Close device
  dev.off()
  
  return(wc_plot)
}

# Usage
plot <- create_neon_wordcloud(ufo_data$Data.Description.excerpt, max_words = 100)

# Display later
replayPlot(plot)

############################# Temperature #############################  

#Neon
UFOsightings_temperature <- ufo_data %>%
  group_by(Avg_Temp_Month_County) %>%
  summarise(count = n()) %>%
  ggplot(aes(Avg_Temp_Month_County, count)) +
  
  geom_point(color = "#FFD700", size = 3, alpha = 0.9) +
  geom_smooth(color = "#FF00FF", size = 1) +
  
  labs(title = "Do UFO Sightings Correlate with Temperature?",
       x = "Average Monthly Temperature (°F)",
       y = "Sightings") +
  neon_theme
UFOsightings_temperature

ggsave(
  "UFOsightings_temperature.png", 
  plot = UFOsightings_temperature,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)

############################# DAY VS NIGHT #############################  
# NEON
day_night_UFOsightings <- ufo_data %>%
  mutate(time_of_day = ifelse(Dates.Sighted.Hour >= 6 & Dates.Sighted.Hour < 18, "Day", "Night")) %>%
  count(time_of_day) %>%
  ggplot(aes(time_of_day, n, fill = time_of_day)) +
  
  geom_col(alpha = 0.25) +
  geom_col(width = 0.6) +
  
  scale_fill_manual(values = c(
    "Day" = "#00FFFF",
    "Night" = "#FF00FF"
  )) +
  
  labs(title = "Day vs Night UFO Sightings",
       x = "", y = "Sightings") +
  neon_theme

day_night_UFOsightings

ggsave(
  "day_night_UFOsightings.png", 
  plot = day_night_UFOsightings,       # Specify the plot object (optional if it's the last one shown)
  width = 8,           # Width in specified units
  height = 6,          # Height in specified units
  units = "in",        # Units (inches is default, others are "cm", "mm")
  dpi = 300,           # Resolution (dots per inch, "retina" or "screen" are also options)
  path = "/users/jacquelineferri/Desktop/sppw/biostats777-final-project-git-culture/plot-images" # Specify a different directory
)
