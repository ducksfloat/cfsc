
# load libraries and set options----------------------------------------------------------
pacman::p_load(tidyverse, sf, rjson)


# read in warehouse data --------------------------------------------------
json_fil <- 
json_data <- fromJSON(file=json_file)
