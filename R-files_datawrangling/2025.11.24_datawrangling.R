#2025.11.24 data wrangling

setwd("/users/jacquelineferri/Desktop/sppw/finalprojrepo/")
getwd()

library(readr)
library(dplyr)
library(tidyverse)

bfro_reports <- read.csv("bfro_reports_geocoded.csv")
countyhealh <- read.csv("countystatepop_20-24.csv")
