library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(openxlsx2)
library(here)
library(leaflet)
library(mapview)
library(stringr)
library(rjson)
library(jsonlite)
library(installr)
setwd(here())

# prep files for basemap tiles --------------------------------------------
neighborhoods <- read_delim("data/CommAreas_20250416.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)
st_write(neighborhoods, ("data/CommAreas.shp"))
park <- read_delim("data/CPD_Parks_20250416.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)
st_write(park, ("data/parks.shp"))
water <- read_delim("data/Hydro_20250416.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)
st_write(water, ("data/waterWays.shp"))
mapview()
streets <- read_delim("data/csv/transportation_20250416.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)
st_write(streets, ("data/shp/streets.shp"))
cta_bus <- read_delim("data/csv/CTA_BusStops_20250416.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)
st_write(cta_bus, ("data/shp/cta_busStops.shp"))


# DONT NEED TO RUN THIS EVERY TIME read in warehouse data and write as raw file--------------------------------------------------
jsonFiles <- list.files("data/slack/raw/urban-canopy-log")
cfscSlack <- data.frame()
for (i in jsonFiles) {
  fileName <- i
  jsonData <- fromJSON(txt = paste0("data/slack/raw/urban-canopy-log/", fileName))%>%
    select(text)%>%
    mutate(date = str_replace(fileName,".json",""))
  cfscSlack <- bind_rows(cfscSlack, jsonData)
}
write_rds(cfscSlack, "data/slack/cfsc_warehouseSlack.rds")

# read in raw file for cleaning -------------------------------------------
cfscSlack <- read_rds("data/slack/cfsc_warehouseSlack.rds")

# start parsing and cleaning ----------------------------------------------
rescueFrom <- c("WP Aldi", "Aldi WP", "Aldi Hodgkins", "Aldi in Wicker Park", "Wicker Park", "Cicero Aldi", "Kostner Aldi",
                "Aldi kostner", "Aldi Kostner", "Aldi on Kostner", "Aldi Lyons", "Aldi Clybourn", "Lyons and Hodgkins ALDI", 
                "kimball Aldi", "Belmont Aldi", "Aldi Cicero", "Aldi Belmont",
                "Aldi milwaukee", "Hodgkins", "Lyons", "Aldi at 2828 N Central", "Aldi in Western Humboldt Park",
                
                "West Loop Marianos","South Loop Marianos", "Mariano’s south loop", "South Loop Mariano’s","Mariano’s",
                "Marianos w loop", "S.Loop Marianos", "Marianos West Loop", "SL Mariano's", "South Loop Mariano's",
                
                "local foods", "Local Foods", "Sysco", "Jewel","Bimbo", "BIMBO", "bimbo", "Windy City", 
                "MEANS database", "Marillac", "Midwest Foods", "Cold Chain", "LSFM", "PFP",
                "Green City Market", "Green city", "Green City", "green city market", "Dom's", "Chef’s Warehouse",
                
                "West Suburban Pantry", "West Suburban Community Pantry", "Friendship Center",
                "Irving Park pantry", "Irving Park Food Pantry","Irving Park Pantry",
                "Above &amp Beyond", "Above & Beyond", "above and beyond", "A&ampB","Above and Beyond", "above and Beyond", 
                "Above and beyond",
                "New Hope", "New Life", "Uptown Baptist Church", "St. Paul and the Redeemer", "Oakdale Covenant Church",
                "Bridgeport library", "A Just Harvest", "Nourishing Hope",
                "IPCFP",  "NA4J")
takenTo <- c("Love Fridges", "Love Fridge", "Love fridge", "love fridge", "Port &amp Pulaski", "55/Pulaski", 
             "55/Pulaski and Port", "55th &amp Pulaski", "Pilsen &amp Pulaski", "LoveFridge", "Lie fridge", "Live Fridge",
             "Live fridge", "community fridges", "community fridge",
              "Pilsen shelter", "shelter", "Shelter", 
             "pilsen food pantry", "Pilsen Food Pantry", 
             "Lsrsn", "lsrsn", "LSRSN ","Fnb", "CACC", "SSMA","SWC", "Mr Wiggins", "Edgewater Mutual Aid","Community Dinners",
             "PSN", "Psn", "LF", "Avondale", "WSMA")
both <- c("Owmcl-chicago", "OWMCL-Chicago", "OWMCL-CHICAGO","keystone ", "Keystone")
#"Owmcl-chicago", "OWMCL-CHICAGO",
cfscSlack_clean0 <- cfscSlack%>%
  filter(!str_detect(text, "has joined the channel"),
         str_detect(date, "2025-") | str_detect(date, "2024-"),
         text != "")%>%
  mutate(text = str_replace_all(text, "[:;]", ""),
         rescueFrom = str_extract(text, str_c(rescueFrom, collapse = "|")),
         both = str_extract(text, str_c(both, collapse = "|")),
         takenTo = str_extract(text, str_c(takenTo, collapse = "|")))
check0 <- cfscSlack_clean0%>%
  filter(is.na(takenTo)&is.na(rescueFrom)&is.na(both))
cfscSlack_clean1<- cfscSlack_clean0%>%
  mutate(rescueFrom = case_when(rescueFrom %in% c("WP Aldi", "Aldi milwaukee", "Aldi in Wicker Park", "Wicker Park") ~ "Aldi Wicker Park",
                         rescueFrom %in% c("Aldi in Western Humboldt Park", "Aldi kostner", "Aldi Kostner", 
                                           "Aldi on Kostner", "Kostner Aldi") ~ "Aldi Kostner",
                         rescueFrom %in% c("kimball Aldi") ~ "Aldi Kimball",
                         rescueFrom %in% c("Hodgkins") ~ "Aldi Hodgkins",
                         rescueFrom %in% c("Belmont Aldi") ~ "Aldi Belmont",
                         rescueFrom %in% c("Lyons") ~ "Aldi Lyons",
                         rescueFrom %in% c("Lyons and Hodgkins ALDI") ~ "Aldi Lyons and Hodgkins",
                         rescueFrom %in% c("Above &amp Beyond", "Above & Beyond", "above and beyond", "Above and beyond",
                                           "A&ampB","Above and Beyond", "above and Beyond") ~ "Above and Beyond",
                         rescueFrom %in% c("bimbo", "BIMBO") ~ "Bimbo",
                         rescueFrom %in% c("Marianos w loop", "West Loop Marianos") ~ "Marianos West Loop",
                         rescueFrom %in% c("South Loop Mariano's", "South Loop Mariano’s", "South Loop Mariano's",
                                           "South Loop Mariano’s", "South Loop Marianos") ~ "Marianos South Loop",
                         rescueFrom %in% c("S.Loop Marianos", "SL Mariano's", "Mariano’s south loop") ~ "Marianos South Loop",
                         rescueFrom %in% c("IPCFP", "Irving Park pantry", "Irving Park Food Pantry") ~ "Irving Park Pantry",
                         rescueFrom %in% c("Green city", "Green City", "green city market") ~ "Green City Market",
                         rescueFrom %in% c("local foods") ~ "Local Foods",
                         rescueFrom %in% c("West Suburban Community Pantry", "West Suburban Pantry") ~ "Western Suburban Pantry",
                         TRUE ~ rescueFrom),
         takenTo = case_when(takenTo %in% c("Shelter", "shelter") ~ "Pilsen shelter",
                             takenTo %in% c("55th &amp Pulaski") ~ "55/Pulaski",
                             takenTo %in% c("Pilsen &amp Pulaski", "Port &amp Pulaski") ~ "Pilsen/Pulaski",
                             takenTo %in% c("Love Fridges", "Love fridge", "love fridge", "LoveFridge", "community fridges",
                                            "Lie fridge", "Live Fridge","Live fridge", "community fridges") ~ "Love Fridge",
                             takenTo %in% c("Psn") ~ "PSN",
                             takenTo %in% c("Lsrsn", "lsrsn", "LSRSN ") ~ "LSRSN",
                             takenTo %in% c("pilsen food pantry") ~ "Pilsen Food Pantry",
                             TRUE ~ takenTo),
         both = case_when(both %in% c("OWMCL-Chicago", "OWMCL-CHICAGO") ~ "Owmcl-chicago",
                          both == "keystone " ~ "Keystone",
                          TRUE ~ both))%>%
  filter(!is.na(takenTo)|!is.na(rescueFrom)|!is.na(both))
check1 <- cfscSlack_clean1%>%
  group_by(rescueFrom)%>%
  summarize(n=n())
check2 <- cfscSlack_clean1%>%
  group_by(takenTo)%>%
  summarize(n=n())
check3 <- cfscSlack_clean1%>%
  group_by(both)%>%
  summarize(n=n())
