# Load libraries
library(tidyverse)
library(googlesheets4)
library(readr)
library(janitor)
library(lubridate)


# Cleaning ----------------------------------------------------------------


# Load custom name cleaning function
source("scripts/function_clean_facility_names.R")

# Data cleaning vectors
change_pbnds2008 <- c("PBNDS 2008; 2011 SAAPI", "PBNDS 2008/SAAPI 2011", "PBNDS 2008/ SAAPI 2011",
                      "PBNDS 2008, 2011 SAAPI", "PBNDS 2008 for Over 72 Hours", "PBNDS 2008 / SAAPI 2011",
                      "ICE PBNDS 2008", "(PBNDS) 2008 with SAAPI 2010")

change_pbnds2011 <- c("PBNDS2011rev2016", "PBNDS2011", "PBNDS 2012",
                      "PBNDS 2011rev2016", "PBNDS 2011/2016 Revisions for Over 72 Hours",
                      "PBNDS 2011/2016", "PBNDS 2011 with 2016 revisions", "PBNDS 2011 SAAPI")

change_nds <- c("Over 72 Hours-NDS/SAAPI 2011", "Over 72 Hours-NDS", "NDS/SAAPI 2011", "NDS for Over 72 Hours",
                "NDS-Over 72 Hours", "NDS-Over-72-Hours", "ICE NDS 2000-Over 72 Hours", "ICE NDS")

change_nds2019 <- c("NDS 2019/SAAPI 2011", "NDS 2019 for Over 72 Hour", "ICE NDS 2019")

change_nds2000 <- c("NDS 2000/2011 SAAPI", "ICE NDS 2000")



# Load from File ----------------------------------------------------------


# Read in Sheet G-324A-19
df_324 <- read_sheet("https://docs.google.com/spreadsheets/d/1im5VSi3bIEi13O8WQ56wEIXSyNEstbGMylXXgD9bAG0/edit#gid=1858227071",
                     sheet="G-324A-19",
                     col_names = TRUE,
                     col_types = "c") %>% 
  clean_names() %>% 
  
  # Run custom cleaning function
  clean_facility_names() %>% 
  
  # Clean inspection standards names
  mutate(standards = replace(standards, standards %in% change_pbnds2011, "PBNDS 2011"),
         standards = replace(standards, standards %in% change_pbnds2008, "PBNDS 2008"),
         standards = replace(standards, standards %in% change_nds, "NDS"),
         standards = replace(standards, standards %in% change_nds2019, "NDS 2019"),
         standards = replace(standards, standards %in% change_nds2000, "NDS 2000")
  ) %>% 
  
  # df specific changes
  mutate(facility = as.factor(facility),
         state = as.factor(state),
         date = mdy(inspection_date),
         current_inspection_date_from = mdy(current_inspection_date_from),
         current_inspection_date_to = mdy(current_inspection_date_to)
  ) %>% 
  
  relocate(date, .before = inspection_date) %>% 
  mutate_at(c(20:49), as.numeric)