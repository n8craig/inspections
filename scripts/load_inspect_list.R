# Load necessary libraries
library(googlesheets4)
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)

# Load custom name cleaning function
source("scripts/function_clean_facility_names.R")

# Read in the sheet
df_inspect <- read_sheet("https://docs.google.com/spreadsheets/d/1im5VSi3bIEi13O8WQ56wEIXSyNEstbGMylXXgD9bAG0/edit#gid=1858227071",
                         sheet="Inspections",
                         col_names = TRUE,
                         col_types = "c") %>% 
  clean_names() %>% 
  clean_facility_names()