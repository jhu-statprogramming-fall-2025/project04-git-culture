setwd("C:/Users/sonia_y/OneDrive - Johns Hopkins/2025-26/Term 2 2025/Statistical Programming/biostats777-final-project-git-culture")

# Read in UFO sighting data

library(readr)

nuforc <- read.csv("nuforc_ufo_us_wrangled.csv")

# Convert date column to date format

library(lubridate)

nuforc$sighted_date <- ymd(nuforc$sighted_date)

# Number of sightings by state

table(nuforc$Location.State)

# California has the largest number of sightings. Filter NUFORC data to only include California

nuforc <- nuforc[nuforc$Location.State=="CA",]

# Start retrieving monthly weather data by CA county using data from: 
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/service-api

# Read in NOAA county labels

library(readxl)

NOAA_counties <- read_xlsx("NOAA_counties.xlsx")

NOAA_counties <- NOAA_counties %>%
  separate(County, into = c("County", "State"), sep = ", ")

# Filter by CA

NOAA_counties <- NOAA_counties %>% filter(State=="CA")

# Create URL data frame

url_df <- data.frame(Label=NOAA_counties$Label,County=NOAA_counties$County)

# Add columns with the remaining URL components

url_df$url1 <- "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/"

url_df$precip <- "/pcp/ytd/0/2004-2014/data.csv" 

url_df$avgtemp <- "/tavg/ytd/0/2004-2014/data.csv"

# Combine the URL components to make each individual URL

url_df$pcp_url <- paste(url_df$url1,url_df$Label,url_df$precip,sep="")

url_df$temp_url <- paste(url_df$url1,url_df$Label,url_df$avgtemp,sep="")

# Create CSV for monthly average precipitation for first listed CA county (Alameda) 

avg_precip <- as.data.frame(read.csv(url_df[1,"pcp_url"],skip=2))

avg_precip$County <- url_df[1,2]

# Append remaining counties' data

for(i in 2:58) {
  df <- as.data.frame(read.csv(url_df[i,"pcp_url"],skip=2))
  df$County <- url_df[i,2]
  avg_precip <- bind_rows(avg_precip,df)
}

# Repeat this process for monthly average temp by CA county

avg_temp <- as.data.frame(read.csv(url_df[1,"temp_url"],skip=2))

avg_temp$County <- url_df[1,2]

for(i in 2:58) {
  df <- as.data.frame(read.csv(url_df[i,"temp_url"],skip=2))
  df$County <- url_df[i,2]
  avg_temp <- bind_rows(avg_temp,df)
}

# Since we were only able to pull weather data for 2004-2014, let's subset the NUFORC data

nuforc <- nuforc[nuforc$year>=2004,]

# 5510 observations left! Years before 1998 all have < 100 observations anyways

# Let's pull the month out of the avg precip and avg temp data frames, to make it easier to merge the data

avg_precip <- avg_precip %>%
  separate(Date, into = c("Year", "Month"), sep = 4)

avg_precip$Year <- as.integer(avg_precip$Year)

avg_precip$Month <- as.integer(avg_precip$Month)

avg_temp <- avg_temp %>%
  separate(Date, into = c("Year", "Month"), sep = 4)

avg_temp$Year <- as.integer(avg_temp$Year)

avg_temp$Month <- as.integer(avg_temp$Month)

# Read in the city and county data to match the NUFORC city data to the appropriate county

city_county <- as.data.frame(read.csv("city_county_data.csv"))

# Subset to CA

city_county <- city_county[city_county$State.Name=="CALIFORNIA",]

# Change the city_county and nuforc city column names to match

names(city_county)[names(city_county)=="City.Name"] <- "City"

names(nuforc)[names(nuforc)=="Location.City"] <- "City"

# Change city names in nuforc to all caps

nuforc$City <- toupper(nuforc$City)

# Add county column to nuforc & relocate the column next to City

nuforc <- left_join(nuforc,city_county %>% select(City,County.Name),by="City")

nuforc <- nuforc %>% relocate(County.Name, .after=City)

# Rename county column for easy merging

names(nuforc)[names(nuforc)=="County.Name"] <- "County"

# Change county values in avg temp & precip DFs to match the county column in nuforc

avg_precip$County <- str_remove(avg_precip$County, " County") %>% toupper()

avg_temp$County <- str_remove(avg_temp$County, " County") %>% toupper()

# Add avg monthly temp & precip to nuforc data

nuforc <- left_join(nuforc,avg_precip,by=c('Dates.Sighted.Year'='Year',
                                           'Dates.Sighted.Month'='Month',
                                           'County'='County'),relationship='many-to-one')

names(nuforc)[names(nuforc)=="Value"] <- "Avg_Precip_Month_County"

nuforc <- left_join(nuforc,avg_temp,by=c('Dates.Sighted.Year'='Year',
                                           'Dates.Sighted.Month'='Month',
                                           'County'='County'),relationship='many-to-one')

names(nuforc)[names(nuforc)=="Value"] <- "Avg_Temp_Month_County"

# Create updated NUFORC csv

write.csv(nuforc, "C:\\Users\\sonia_y\\OneDrive - Johns Hopkins\\2025-26\\Term 2 2025\\Statistical Programming\\biostats777-final-project-git-culture\\nuforc_county_weather.csv", row.names=FALSE)