
# load libraries and set options----------------------------------------------------------
pacman::p_load(stringr, dplyr, sf, rjson, here, jsonlite)
setwd(here())

# read in warehouse data --------------------------------------------------
jsonFiles <- list.files("data/slack/urban-canopy-log")
cfscSlack <- data.frame()
for (i in jsonFiles) {
  fileName <- i
  jsonData <- fromJSON(txt = paste0("data/slack/urban-canopy-log/", fileName))%>%
    select(text)%>%
    mutate(date = str_replace(fileName,".json",""))
  cfscSlack <- bind_rows(cfscSlack, jsonData)
}