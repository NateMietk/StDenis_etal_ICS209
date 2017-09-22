
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
  # helper function to resolve conflicting variables from two fire 
  #   data sources
  
  # args:
  #   - df (data.frame) a data frame corresponding to a group 
  #       which is unique combo of incidentnum, syear, state, variable_name
  
  # returns:
  #   - output_df (data.frame) data.frame with grouping vars and consensus value
  
  variable_name <- unique(df$variable_name)
  stopifnot(length(variable_name) == 1)
  
  # there are two values, we need to choose which to use
  val_x <- filter(df, source_df == 'x')$value
  val_y <- filter(df, source_df == 'y')$value

  stopifnot(length(val_x) == 1)
  stopifnot(length(val_y) == 1)
  
  if (variable_name %in% c('lat', 'long')) {
    consensus <- ifelse(identical(val_x, 0) | is.na(val_x), val_y, val_x)
  } else if (variable_name %in% c('costs', 'fatalities', 'home.destroyed', 
                                  'home.threat', 'max.pers', 
                                  'max.aerial', 'tot.pers', 
                                  'tot.aerial', 'max.agency.support', 
                                  'eday', 'edoy', 'emonth', 'eyear', 
                                  'report_length', 'area_km2')) {
    consensus <- max(c(val_x, val_y), na.rm = TRUE)
  } else if (variable_name %in% c('sday', 'sdoy', 'smonth')) {
    consensus <- min(c(val_x, val_y), na.rm = TRUE) 
  } else if (variable_name %in% c('eday', 'edoy', 'emonth')) {
    consensus <- max(c(val_x, val_y), na.rm = TRUE) 
  } else if (variable_name == 'cause') {
    consensus <- cause_consensus(val_x, val_y)
  } else {
    consensus <- NA
  }

  output_df <- distinct(df, incidentnum, syear, state, variable_name) %>%
    mutate(consensus_value = consensus)

  stopifnot(nrow(output_df) == 1)
  output_df
}

cause_consensus <- function(val_x, val_y) {
  # helper function to generate consensus for fire cause
  
  # Args: 
  #   - cause_x (char) 
  #   - cause_y (char)
  
  # Returns:  
  #   - consensus (char)
  stopifnot(unique(c(val_x, val_y)) %in% c("Human", "Lightning", "Unk", NA))
  is_x_useless <- val_x == 'Unk' | is.na(val_x)
  is_y_useless <- val_y == 'Unk' | is.na(val_y)
  both_useless <- is_x_useless & is_y_useless
  are_xy_the_same <- identical(val_x, val_y)
  
  consensus <- 'DEFAULT'
  if (are_xy_the_same) {
    consensus <- val_x
  }
  if (is_x_useless) {
    consensus <- val_y
  }
  if (is_y_useless) {
    consensus <- val_x
  }
  if (both_useless) {
    consensus <- 'Unk'
  }
  if (identical(val_x, 'Lightning') & identical(val_y, 'Human') | 
      identical(val_x, 'Human') & identical(val_y, 'Lightning')) {
    # we don't really know which is right, paste them together
    consensus <- c('Human-Lightning?')
  }
  consensus
}


# Generate consensus values  ----------------------------------------------
ics209_clean <- fam_clean %>%
  # temporarily subset
  left_join(scrape_clean, by = c("incidentnum", "syear", "state")) %>% 
  gather(variable, value, -incidentnum, -syear, -state) %>%
  mutate(variable = gsub("\\.x", "~x", variable), 
         variable = gsub("\\.y", "~y", variable), 
         variable = gsub("rdate", "rdate~y", variable)) %>%
  separate(variable, into = c('variable_name', 'source_df'), sep = "~") %>%
  filter(!(variable_name %in% c('sdate', 'rdate'))) %>%
  group_by(incidentnum, syear, state, variable_name) %>% 
  do(make_consensus(.)) %>%
  spread(variable_name, consensus_value, convert = TRUE) %>%
  select(incidentnum, lat, long, state, area_km2, cause, sdoy, sday, smonth, syear, edoy, eday, emonth, eyear, report_length,
         costs, fatalities, home.destroyed, home.threat, max.aerial, tot.aerial, max.pers, tot.pers, max.agency.support)


# Make the cleaned ICS-209 data spatial  
ics209_pt <- st_as_sf(ics209_clean, coords = c("long", "lat"), 
                      crs = "+proj=longlat +datum=WGS84")

# Clip the ICS-209 data to the CONUS and remove unknown cause
conus_209 <- st_intersection(ics209_pt, st_union(usa_shp))

# Write out the shapefile.
st_write(conus_209, paste0("../data", "/anthro/", "ics209_conus.gpkg"), 
           driver = "GPKG",
           update=TRUE)

wui_shp <- st_read(dsn = file.path("../data", "anthro", "wui_us.gpkg"),
                   layer = "wui_us", quiet= TRUE) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

wui_209 <- st_intersection(conus_209, wui_shp) 

# Write out the shapefile.  
st_write(wui_209, paste0("../data", "/anthro/", "ics209_wui_conus.gpkg"), 
           driver = "GPKG",
           update=TRUE)