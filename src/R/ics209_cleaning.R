
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(assertthat)
library(rvest)
library(httr)
library(purrr)

## Download and process State data
# Creat directories for state data
raw_prefix <- file.path("data", "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ics_prefix <- file.path("data", "ics209")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(raw_prefix,
                us_prefix,
                ics_prefix)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

us_shp <- file.path(us_prefix, "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform(crs = 4326) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  mutate(group = 1)

# Clean ICS-209 from 2001-2013 -----------------------------
ics209_clean <- fread("data/ics209_2001_2013_wfonly.csv") %>%
  mutate_all(funs(replace(., is.na(.), 0))) 
  as.tibble()
  
names(ics209_clean) %<>% tolower 

ics209_clean <- ics209_clean %>% 
  filter(!(un_ustate %in% c("AK", "HI", "PR"))) %>%
  filter(type_inc != "RX") %>%
  mutate(long = -longitude,
         lat = latitude,
         date = mdy(report_date),
         doy = yday(date),
         day = day(date),
         month = month(date),
         year = year(date),
         area_ha = ifelse(area_measurement == "SQ MILES", area*258.99903998855,
                          area*0.404686),
         area_km2 = area_ha*0.01,
         costs = ifelse(est_final_costs == 0 & costs_to_date > 1, costs_to_date,
                        est_final_costs),
         cause_binary = ifelse(cause == "H", "2", 
                               ifelse(cause =="L", "1", "0"))) %>%
  group_by(incident_unique_id) %>%
  summarise(lat = max(lat),
            long = min(long),
            syear = first(year),
            smonth = first(month),
            sday = first(day),
            sdoy = first(doy),
            eyear = last(year),
            emonth = last(month),
            eday = last(day),
            edoy = last(doy),
            area_ha = max(area_ha),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            destroyed_res = max(destroyed_res),
            threatened_res = max(threatened_res),
            max_personal = max(imsr_total_personnel),
            max_aerial = max(imsr_num_aerial),
            tot_personal = sum(imsr_total_personnel),
            tot_aerial = sum(imsr_num_aerial),
            max_agencies = max(imsr_num_agencies),
            personal_per_size = tot_personal/area_km2,
            agency_per_size = max_agencies/area_km2,
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))

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


