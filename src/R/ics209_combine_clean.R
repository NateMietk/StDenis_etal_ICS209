
# Load all necessary libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(assertthat)
library(rvest)
library(httr)
library(purrr)

# Load and process all the data from famweb and the scrape
source("src/R/helper_functions.R")
source("src/R/ics209_fam_cleaning.R")
source("src/R/ics209_scrape_cleaning.R")

make_consensus <- function(df) {
  
  variable_name <- unique(df$variable_name)

  if (variable_name %in% c('lat', 'lon')) {
    consensus <- ifelse(df$value != 0, df$value, df$value)
  } 
  
  output_df <- distinct(df, incidentnum, syear, state, variable_name) %>%
    mutate(consensus_value = consensus)
  
  stopifnot(nrow(output_df) == 1)
  
  return(output_df)
}

final <-  fam_clean %>%
  left_join(scrape_clean, by = c("incidentnum", "syear", "state")) %>%
  gather(variable, value, -incidentnum, -syear, -state) %>%
  mutate(variable = gsub("\\.x", "~x", variable), 
         variable = gsub("\\.y", "~y", variable), 
         variable = gsub("rdate", "rdate~y", variable)) %>%
  separate(variable, into = c('variable_name', 'source_df'), sep = "~") %>%
  filter(!is.na(value)) %>%
  group_by(incidentnum, syear, state, variable_name) %>%
  do(make_consensus(.))









# Make the cleaned ICS-209 data spatial  
ics209_pt <- st_as_sf(ics209_clean, coords = c("long", "lat"), 
                      crs = 4326)

# Clip the ICS-209 data to the CONUS and remove unknown cause
conus_209 <- st_intersection(ics209_pt, st_union(usa_shp)) %>%
  filter(cause != "Unk")

# Write out the shapefile.
if (!file.exists(file.path(ics_prefix, "ics209_conus.gpkg"))) {
  st_write(conus_209, file.path(ics_prefix, "ics209_conus.gpkg"), 
           driver = "GPKG",
           update=TRUE)}

wui_shp <- st_read(dsn = file.path("../data", "anthro", "wui_us.gpkg"),
                   layer = "wui_us", quiet= TRUE) %>%
  st_transform(crs = 4326)

wui_209 <- st_intersection(ics209_pt, wui_shp)

# Write out the shapefile.  
if (!file.exists(file.path(ics_prefix, "ics209_wui_conus.gpkg"))) {
  st_write(wui_209, file.path(ics_prefix, "ics209_wui_conus.gpkg"), 
           driver = "GPKG",
           update=TRUE)}