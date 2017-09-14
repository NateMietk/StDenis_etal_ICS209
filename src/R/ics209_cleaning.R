
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
ics209_clean <- fread("data/ics209/tbls/ics209_2001_2013_wfonly.csv") %>%
  mutate_all(funs(replace(., is.na(.), 0))) 

names(ics209_clean) %<>% tolower 

ics209_clean <- ics209_clean %>% 
  filter(!(un_ustate %in% c("AK", "HI", "PR"))) %>%
  filter(type_inc != "RX") %>%
  mutate(long = -longitude,
         lat = latitude,
         incidentnum = incident_number,
         date = mdy(report_date),
         doy = yday(date),
         day = day(date),
         month = month(date),
         syear = year(date),
         state = un_ustate,
         area_ha = ifelse(area_measurement == "SQ MILES", area*258.99903998855,
                          area*0.404686),
         area_km2 = area_ha*0.01,
         costs = ifelse(est_final_costs == 0 & costs_to_date > 1, costs_to_date,
                        est_final_costs),
         cause_binary = ifelse(cause == "H", "2", 
                               ifelse(cause =="L", "1", "0"))) %>%
  group_by(incidentnum, syear, state) %>%
  summarise(lat = max(lat),
            long = min(long),
            sdate = min(date),
            smonth = min(month),
            sday = min(day),
            sdoy = min(doy),
            eyear = max(syear),
            emonth = max(month),
            eday = max(day),
            edoy = max(doy),
            report_length = max(edoy - sdoy),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            home.destroyed = max(destroyed_res),
            home.threat = max(threatened_res),
            max.pers = max(imsr_total_personnel),
            max.aerial.support = max(imsr_num_aerial),
            tot.personal = sum(imsr_total_personnel),
            tot.aerial = sum(imsr_num_aerial),
            max.agency.support = max(imsr_num_agencies),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))



