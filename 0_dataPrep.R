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
library(ggmap)
library(leaflet)
setwd(here())

# NOT NEEDED: prep files for basemap tiles --------------------------------------------
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
write_rds(cfscSlack, "data/slack/cfsc_keystoneSlack.rds")

# READ IN RAW UC SLACK FILE FOR CLEANING -------------------------------------------
cfscSlack <- read_rds("data/slack/cfsc_ucSlack.rds")

# INITIAL PARSING, CLEANING AND GEOCODING OF RAW UC WAREHOUSE SLACK CHANNEL ----------------------------------------------
#create lists of general group names to be flagged in slack data and pulled into rescue types
rescueFrom <- c("WP Aldi", "Aldi WP", "Aldi Hodgkins", "Aldi in Wicker Park", "Wicker Park", "Cicero Aldi", "Kostner Aldi",
                "Aldi kostner", "Aldi Kostner", "Aldi on Kostner", "Aldi Lyons", "Aldi Clybourn", "Lyons and Hodgkins ALDI", 
                "kimball Aldi", "Belmont Aldi", "Aldi Cicero", "Aldi Belmont",
                "Aldi milwaukee", "Hodgkins", "Lyons", "Aldi at 2828 N Central", "Aldi in Western Humboldt Park",
                
                "West Loop Marianos","South Loop Marianos", "Mariano’s south loop", "South Loop Mariano’s","Mariano’s",
                "Marianos w loop", "S.Loop Marianos", "Marianos West Loop", "SL Mariano's", "South Loop Mariano's",
                
                "local foods", "Local Foods", "Sysco", "Jewel","Bimbo", "BIMBO", "bimbo", "Windy City", 
                "MEANS database", "Marillac", "Midwest Foods", "Cold Chain", "LSFM", "Logan Square Farmers Market", "PFP",
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
both <- c("Owmcl-chicago", "OWMCL-Chicago", "OWMCL-CHICAGO","keystone ", "Keystone", "UC", 'uc', "Uc", "uC")
#"Owmcl-chicago", "OWMCL-CHICAGO",

#Pull out lines that are flagged from grouping above and clean into geocodable locations
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
                         rescueFrom %in% c("LSFM") ~ "Logan Square Farmers Market",
                         rescueFrom %in% c("West Suburban Community Pantry", "West Suburban Pantry") ~ "Western Suburban Pantry",
                         rescueFrom %in% c("New Life") ~ "New Life Albany Park",
                         rescueFrom %in% c("PFP") ~ "Pilsen Food Pantry",
                         TRUE ~ rescueFrom),
         takenTo = case_when(takenTo %in% c("Shelter", "shelter") ~ "Pilsen shelter",
                             takenTo %in% c("55th &amp Pulaski") ~ "55/Pulaski",
                             takenTo %in% c("Pilsen &amp Pulaski", "Port &amp Pulaski") ~ "Pilsen/Pulaski",
                             takenTo %in% c("Love Fridges", "Love fridge", "love fridge", "LoveFridge", "community fridges",
                                            "Lie fridge", "Live Fridge","Live fridge", "community fridges", "LF") ~ "Love Fridge",
                             takenTo %in% c("Psn") ~ "PSN",
                             takenTo %in% c("Lsrsn", "lsrsn", "LSRSN ") ~ "Lincoln Square Ravenswood Solidarity Network",
                             takenTo %in% c("pilsen food pantry") ~ "Pilsen Food Pantry",
                             takenTo %in% c("Fnb") ~ "Rogers Park Food Not Bombs",
                             takenTo %in% c("SSMA") ~ "South Side Mutual Aid",
                             takenTo %in% c("WSMA") ~ "West Side Mutual Aid",
                             takenTo %in% c("SWC") ~ "Southwest Collective",
                             takenTo %in% c("PSN") ~ "Pilsen Solidarity Network",
                             TRUE ~ takenTo),
         both = case_when(both %in% c("OWMCL-Chicago", "OWMCL-CHICAGO") ~ "Owmcl-chicago",
                          both == "keystone " ~ "Keystone",
                          both %in% c("uc", "UC") ~ "Urban Canopy",
                          TRUE ~ both),
         ID = row_number())%>%
  filter(!is.na(takenTo)|!is.na(rescueFrom)|!is.na(both))%>%
  filter(!takenTo %in% c("Love Fridge", "community fridge", "Pilsen/Pulaski", "Mr Wiggins", "55/Pulaski", "CACC", "Green City Market",
                         "Owmcl-chicago", "Community Dinners"))

#QUESSTION FOR COALITION:
#what do we do with: 
#Love Fridge, Community Fridge, Pilsen/Pulaski, Mr Wiggins, 55/Pulaski, CACC, Green City Market, Owmcl-chicago,
#community dinners,

#get frequencies of each identified location by group for location master list and 
check1 <- cfscSlack_clean1%>%
  group_by(rescueFrom)%>%
  summarize(n=n())
write_xlsx(check1, "data/slack/rescueFrom.xlsx")
check2 <- cfscSlack_clean1%>%
  group_by(takenTo)%>%
  summarize(n=n())
write_xlsx(check2, "data/slack/takenTo.xlsx")
check3 <- cfscSlack_clean1%>%
  group_by(both)%>%
  summarize(n=n())
write_xlsx(check3, "data/slack/both.xlsx")

locations <- bind_rows(check1, check2, check3)%>%
  filter(!is.na(rescueFrom)|!is.na(takenTo)|!is.na(both))%>%
  mutate(name = case_when(!is.na(rescueFrom) ~ rescueFrom,
                          !is.na(takenTo) ~ takenTo,
                          !is.na(both) ~ both),
         originFlag = case_when(!is.na(rescueFrom) ~ "1",
                                !is.na(takenTo) ~ "2",
                                !is.na(both) ~ "3"),
         City = "Chicago",
         State = "IL")%>%
  filter(!name %in% c("Mariano’s", "Dom's", "Jewel"))%>%
  mutate(pct_n = round((n/sum(n, na.rm = TRUE))*100, 2))%>%
  mutate(Address = paste(name, City, State, sep = ", "))
ggmap::register_google("AIzaSyAwz2wwo1PfDG-zAKe17i96d2HoCpvmZu8")
#geocode("1600 Amphitheatre Parkway, Mountain View, CA", output = "latlon", source = "google")

check0 <- locations%>%
  filter(is.na(Address) | Address == "")
addresses <- locations$Address
geocode_results <- geocode(addresses, output = "all", source = "google")

 geocode_data <- purrr::map_dfr(geocode_results, function(result) {
   if (length(result$results) == 0) {
     return(tibble(lat = NA, lon = NA, accuracy = NA))
   }
   location <- result$results[[1]]$geometry$location
   accuracy <- result$results[[1]]$geometry$location_type
   tibble(lat = location$lat, lon = location$lng, accuracy = accuracy)
 })
 locations_geocoded <- bind_cols(locations, geocode_data)
write_rds(locations_geocoded,"data/slack/uclocations_geocodedRaw.rds")

locations_geocoded<- read_rds("data/slack/uclocations_geocodedRaw.rds")
locations_sf0 <- locations_geocoded%>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)%>%
  select(-c(rescueFrom, takenTo, both))
mapview(locations_sf0, zcol = "originFlag")

st_write(locations_sf0, "data/shapefile/locations_geocoded.shp")


# CREATE MASTER FILE OF NEIGHBORHOODS WITH CFSC FOOD DISTRIBUTION ACTIVITY --------------------------------------------
neighborhood0 <- read_delim("data/csv/Boundaries_-_Community_Areas_20250502.csv", delim = ",")%>%
  st_as_sf(wkt = "the_geom", crs = 4326)

#these points are taken from the takento flag in the slack data
#we will join neighborhood to these points and add to the list below
takenTo_locations <- st_read("data/shapefile/locations_geocoded.shp")%>%
  filter(originFlag == "2")%>%
  st_join(neighborhood0, join = st_within)%>%
  select(COMMUNITY)%>%
  st_drop_geometry()%>%
  distinct()
print(takenTo_locations)

#these neighborhoods come from the cfsc food distribution calendar
distroNeighborhoods = c("LINCOLN SQUARE","ROGERS PARK","ALBANY PARK","IRVING PARK","BELMONT CRAGIN","AVONDALE",
                        "LOGAN SQUARE","HUMBOLDT PARK","WEST TOWN","AUSTIN","WEST GARFIELD PARK",
                        "EAST GARFIELD PARK","NEAR WEST SIDE","NORTH LAWNDALE","LOWER WEST SIDE",
                        "NEAR SOUTH SIDE","WEST LAWN","AUBURN GRESHAM","BEVERLY","MORGAN PARK","EDGEWATER")

                        #"DOUGLAS",                      #"OAKLAND",
                        #"FULLER PARK",                        #"GRAND BOULEVARD", 
                        #"KENWOOD",                        #"WASHINGTON PARK", 
                        #"HYDE PARK",                        #"WOODLAWN",
                        #"JEFFERSON PARK",                        #"FOREST GLEN",
                        #"NORTH PARK",                        #"PORTAGE PARK",
                        #"DUNNING",                        #"MONTCLARE",
                        #"WEST RIDGE",                        #"HERMOSA",
                        #"BURNSIDE",                        #"GARFIELD RIDGE",
                        #"UPTOWN",                        #"SOUTH LAWNDALE",
                        #"ARMOUR SQUARE",                        #"NORWOOD PARK",  
                        #"NEAR NORTH SIDE",                        #"LOOP",
                        #"SOUTH SHORE",                        #"CHATHAM",
                        #"AVALON PARK",                        #"SOUTH CHICAGO", 
                        #"MCKINLEY PARK",                        #"LAKE VIEW",
                        #"CALUMET HEIGHTS",                        #"ROSELAND",
                        #"NORTH CENTER",                        #"PULLMAN",
                        #"SOUTH DEERING",                        #"EAST SIDE",
                        #"WEST PULLMAN"                        #"RIVERDALE",
                        #"HEGEWISCH",                          #"ARCHER HEIGHTS",
                        #"BRIGHTON PARK",                        #"BRIDGEPORT",
                        #"NEW CITY",                        #"WEST ELSDON",
                        #"GAGE PARK",                        #"CLEARING",
                        #"CHICAGO LAWN",                        #"WEST ENGLEWOOD",
                        #"ENGLEWOOD",                        #"GREATER GRAND CROSSING",
                        #"LINCOLN PARK",                        #"ASHBURN",
                        #"WASHINGTON HEIGHTS",                        #"MOUNT GREENWOOD",
                        #"OHARE",  
                        #"EDISON PARK")
neighborhood1 <- neighborhood0%>%
  mutate(cfsc_foodDistro_flag = case_when(COMMUNITY %in% distroNeighborhoods ~ "1", 
                                          TRUE ~ "0"))
mapview(neighborhood1, zcol = "cfsc_foodDistro_flag")
st_write(neighborhood1, "data/shapefile/cfscDisto_neighborhood.shp")

#read in food locations
locations_sf0 <- st_read("data/shapefile/locations_geocoded.shp")%>%
  filter(originFlag == "1")

#check together
map0 <- mapview(locations_sf0, zcol = "originFlag") + mapview(neighborhood1, zcol = "cfsc_foodDistro_flag")
map0

# PULL IN WAREHOUSE LOCATIONS FOR SHAPEFILE EXPORT ------------------------
locations_warehouse0 <- st_read("data/shapefile/locations_geocoded.shp")%>%
  filter(originFlag == "3" & name != "Owmcl-chicago")
st_write(locations_warehouse0, "data/shapefile/warehouseLocations.shp")

library(RColorBrewer)
library(htmltools)

unique_n <- unique(locations_sf0$n)
quantilesPoints <- quantile(unique_n, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
palPoints <- colorBin(palette = "PuBuGn", domain = unique_n, bins = quantilesPoints, na.color = "#CCCCCC")
palNeighborhood <- colorFactor(
  palette = c("#FFFFFF","#ADD8E6"),  # Light blue for 1, white for 0
  domain = neighborhood1$cfsc_foodDistro_flag
)

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 12) %>%
  addPolygons(data = neighborhood1,
              fillColor = ~palNeighborhood(cfsc_foodDistro_flag),
              color = "black",
              weight = 1,
              opacity = 1,
              fillOpacity = ~ifelse(cfsc_foodDistro_flag == 0, 0, 0.7),
              popup = ~paste(COMMUNITY)) %>%
  addCircleMarkers(
    data = locations_sf0,
    radius = 6,
    color = ~palPoints(n),
    fillOpacity = 0.8,
    popup = ~paste(name, ":", n, "rescues this year")
  ) %>%
  addAwesomeMarkers(
    data = locations_warehouse0,
    icon = awesomeIcons(
      icon = 'star',
      iconColor = "white",
      markerColor = "green",
      library = "fa"),
  popup = ~paste(name, "warehouse")
  )%>%
  addLegend(
    "bottomright",
    pal = palNeighborhood,
    values = neighborhood1$cfsc_foodDistro_flag,
    title = "CFSC distribution in neighborhood",
    opacity = 1
  ) %>%
  addLegend(
    "bottomright",  # Position of the second legend (Quantiles of n)
    pal = palPoints,
    values = locations_sf0$n,
    title = "Quantiles of number of food rescues this year",
    opacity = 1,
    position = "bottomright"  # Position both legends at the bottom-right corner
  )

map
save_html(map, "webmap/leaflet_map.html")


#export into geojson
st_write(neighborhood1, "data/geojson/neighborhoods.geojson", driver = "GeoJSON")
st_write(locations_sf0, "data/geojson/locations_geocoded.geojson", driver = "GeoJSON")
st_write(locations_warehouse0, "data/geojson/locations_warehouse.geojson", driver = "GeoJSON")



