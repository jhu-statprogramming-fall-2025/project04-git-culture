
# Generating RDS file
try({
  census_api_key("d9bce7570759aab4c11d00139db4d5bcec7e06e7")
  
  # Try multiple endpoints
  data_sources <- list()
  
  # 2020 ACS
  tryCatch({
    data_sources$census2020 <- get_decennial(
      geography = "county",
      variables = "P1_001N",
      state = "CA",
      year = 2020
    )
    cat("✓ Got 2020 Census data\n")
  }, error = function(e) cat("✗ 2020 Census failed\n"))
  
  # 2019 ACS
  tryCatch({
    data_sources$acs2019 <- get_acs(
      geography = "county",
      variables = "B01003_001",
      state = "CA",
      year = 2019,
      survey = "acs5"
    )
    cat("✓ Got 2019 ACS data\n")
  }, error = function(e) cat("✗ 2019 ACS failed\n"))
  

  if (length(data_sources) > 0) {
    saveRDS(data_sources, "census_population_data.rds")
    cat("Saved census data to census_population_data.rds\n")
  }
})


library(tidyverse)


df <- read_csv("nuforc_county_weather_bigfoot.csv")

# Census data
census_data <- readRDS("census_population_data.rds")

# Get population data
if ("census2020" %in% names(census_data)) {
  cat("Using 2020 Census data\n")
  pop_data <- census_data$census2020 %>%
    mutate(
      County = str_remove(NAME, ", California") %>% 
        str_trim() %>%
        str_to_title(),
      County = if_else(str_detect(County, "County$"), 
                       County, 
                       paste0(County, " County")),
      population = value,
      data_source = "2020 Decennial Census"
    ) %>%
    select(County, population, data_source)
  
} else if ("acs2019" %in% names(census_data)) {
  cat("Using 2019 ACS data\n")
  pop_data <- census_data$acs2019 %>%
    mutate(
      County = str_remove(NAME, ", California") %>% 
        str_trim() %>%
        str_to_title(),
      County = if_else(str_detect(County, "County$"), 
                       County, 
                       paste0(County, " County")),
      population = estimate,
      data_source = "2015-2019 ACS 5-Year Estimates"
    ) %>%
    select(County, population, data_source)
  
} else {
  stop("No population data found in the .rds file")
}

#Clean
df_clean <- df %>%
  mutate(
    County_Clean = str_trim(County) %>%
      str_to_title() %>%
      if_else(str_detect(., "County$"), ., paste0(., " County"))
  )

# Merge
df_with_pop <- df_clean %>%
  left_join(pop_data, by = c("County_Clean" = "County")) %>%
  select(-County_Clean)  # Remove helper column


if (sum(is.na(df_with_pop$population)) > 0) {
  cat("\nCounties without population data (first 10):\n")
  df_with_pop %>%
    filter(is.na(population)) %>%
    select(County, Location.State) %>%
    distinct() %>%
    head(10) %>%
    print()
}

# Save dataset
output_file <- "nuforc_county_weather_bigfoot_with_census_population.csv"
write_csv(df_with_pop, output_file)

