library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

#-----------------------------
# Plotting cumuluative UFO sightings over time
#-----------------------------

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


# Read the file
df <- read_csv("nuforc_county_weather_bigfoot.csv")

# Filter UFO data & create year-month
ufo_data <- df %>%
  filter(sighting_type == "UFO") %>%
  mutate(
    sighted_date = as.Date(sighted_date),
    year_month = floor_date(sighted_date, "month")
  )

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

#Plotting

#CUMULATIVE SIGHTINGS
#--------------------

#Cumulative sightings over time
ggplot() +
  # Total line
  geom_line(data = monthly_total,
            aes(x = year_month, y = cumulative_total),
            color = "black", size = 1.2) +
  
  # Shape-specific lines
  geom_line(data = monthly_shapes,
            aes(x = year_month, y = cumulative_count, color = Data.Shape),
            size = 1) +
  
  labs(
    title = "Cumulative UFO Sightings Over Time (Monthly Granularity)",
    x = "Month",
    y = "Cumulative Sightings",
    color = "UFO Shape"
  ) +
  theme_minimal()


ufo_data %>%
  ggplot(aes(x = year, fill = Data.Shape))+
  geom_bar()+
  theme_minimal()

#Cumulative sightings over time, neon theme
ggplot() +
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

#TOP COUNTIES
#------------

ufo_data %>%
  count(County, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(County, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 California Counties for UFO Sightings")

#Neon theme hell yeah
ufo_data %>%
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

# SEASONAL PATTERNS
#------------------

ufo_data %>%
  count(Dates.Sighted.Month) %>%
  ggplot(aes(x = Dates.Sighted.Month, y = n)) +
  geom_col(fill = "cadetblue") +
  coord_polar() +
  labs(title = "Seasonal Pattern of UFO Sightings")

#Neon theme my man
ufo_data %>%
  count(Dates.Sighted.Month) %>%
  ggplot(aes(x = as.factor(Dates.Sighted.Month), y = n)) +
  geom_col(fill = "#FF00FF", alpha = 0.25, width = 1) +
  geom_col(fill = "#FF00FF", width = 0.7) +
  
  coord_polar() +
  labs(title = "Seasonal Pattern of UFO Sightings") +
  neon_theme


library(ggridges)

#DISTRIBUTION PLOTS
#-----------------

ufo_data %>%
  filter(sighting_type == "UFO") %>%
  ggplot(aes(x = sighted_date, y = Data.Shape, fill = Data.Shape)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(title = "Distribution of UFO Sightings Over Time by Shape",
       x = "Date", y = "Shape")

#Neon boy

ufo_data %>%
  filter(sighting_type == "UFO") %>%
  ggplot(aes(x = sighted_date, y = Data.Shape, fill = Data.Shape)) +
  
  geom_density_ridges(alpha = 0.8, color = "black") +
  
  labs(title = "Distribution of UFO Sightings Over Time by Shape",
       x = "Date", y = "Shape") +
  neon_theme


#ENCOUNTER DURATION
#-----------------

ufo_data %>%
  filter(sighting_type == "UFO") %>%
  group_by(Data.Shape) %>%
  summarise(mean_duration = mean(as.numeric(Data.Encounter.duration), na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Data.Shape, mean_duration), y = mean_duration, fill = Data.Shape)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Encounter Duration by UFO Shape",
       x = "Shape", y = "Mean Duration (seconds)")

#Neon
ufo_data %>%
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

#WORDCLOUD
#---------

install.packages("wordcloud")
library(wordcloud)
install.packages("tm")
library(tm)

text <- Corpus(VectorSource(ufo_data$Data.Description.excerpt))
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, removeWords, stopwords("english"))

wordcloud(text, max.words = 100, scale = c(3, 0.5))

#Neon Cloudsssss
library(wordcloud)
library(tm)

# Prepare the text
text <- Corpus(VectorSource(ufo_data$Data.Description.excerpt))
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, removeWords, stopwords("english"))

# Define neon colors
neon_colors <- c("#00FFFF", "#FF00FF", "#39FF14", "#FFD700", "#FF4500", "#00FF7F")

# Dark background plot
par(bg = "black")  # sets plot background to black

# Basic neon wordcloud
wordcloud(
  words = text,
  max.words = 100,
  scale = c(3, 0.5),
  random.order = FALSE,
  rot.per = 0.25,
  colors = neon_colors
)


#Temperature
#-----------


ufo_data %>%
  group_by(Avg_Temp_Month_County) %>%
  summarise(count = n()) %>%
  ggplot(aes(Avg_Temp_Month_County, count)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Do UFO Sightings Correlate with Temperature?",
       x = "Average Monthly Temperature (°F)",
       y = "Sightings")

#Neon
ufo_data %>%
  group_by(Avg_Temp_Month_County) %>%
  summarise(count = n()) %>%
  ggplot(aes(Avg_Temp_Month_County, count)) +
  
  geom_point(color = "#FFD700", size = 3, alpha = 0.9) +
  geom_smooth(color = "#FF00FF", size = 1) +
  
  labs(title = "Do UFO Sightings Correlate with Temperature?",
       x = "Average Monthly Temperature (°F)",
       y = "Sightings") +
  neon_theme


#DAY VS NIGHT
#------------

ufo_data %>%
  mutate(
    time_of_day = ifelse(Dates.Sighted.Hour >= 6 & Dates.Sighted.Hour < 18, "Day", "Night")
  ) %>%
  count(time_of_day) %>%
  ggplot(aes(time_of_day, n, fill = time_of_day)) +
  geom_col() +
  labs(title = "Day vs Night UFO Sightings",
       x = "", y = "Sightings")

# NEON
ufo_data %>%
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







