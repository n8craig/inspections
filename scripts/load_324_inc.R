# Load libraries
library(tidyverse)
library(googlesheets4)
library(readr)
library(janitor)
library(lubridate)

# Load custom name cleaning function
source("scripts/function_clean_facility_names.R")

# Get current warning level
defaultW <- getOption("warn")
# Suppress warnings
options(warn = -1)

# Read Google Sheet incident worksheet, convert to data frame, and wrangle
df_324_inc <- read_sheet("https://docs.google.com/spreadsheets/d/1im5VSi3bIEi13O8WQ56wEIXSyNEstbGMylXXgD9bAG0/edit#gid=1858227071",
                         sheet="G-324A-19-inc",
                         col_types = "c") %>% 
  clean_names() %>%
  
  # Run custom cleaning function
  clean_facility_names() %>% 
  
  # df_specific changes
  unite(date, year:month) %>% 
  mutate(facility = as.factor(facility),
         state = as.factor(state),
         date = ym(date)
  ) %>% 
  mutate_at(c(6:76), as.numeric)

# Turn on original warnings
options(warn = defaultW)