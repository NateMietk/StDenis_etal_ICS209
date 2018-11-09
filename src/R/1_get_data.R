source("src/R/functions/helper_functions.R")
source("src/R/functions/st_par.R")
source("src/R/functions/st_parallel.R")

x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf", "gridExtra", "raster", "lme4",
       'lubridate', "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "broom")
lapply(x, library, character.only = TRUE, verbose = FALSE)

## Download and process State data
# Creat directories for state data
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_5m")
bounds_dir <- file.path(prefix, 'bounds')
ics_prefix <- file.path(prefix, "ics_209")
ics_inputs <- file.path(ics_prefix, "input_tbls")
ics_spatial <- file.path(ics_prefix, "spatial")
ecoregion_prefix <- file.path(raw_prefix, "ecoreg")
counties_prefix <- file.path(raw_prefix, 'cb_2016_us_county_20m')

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ics_prefix, ics_spatial, ecoregion_prefix, 
                counties_prefix, ics_inputs, ics_latlong, bounds_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Download the USA states
us_shp <- file.path(us_prefix, "cb_2016_us_state_5m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_5m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

# Download the Level 3 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "us_eco_l3.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

# Download the US counties
counties_shp <- file.path(counties_prefix, "cb_2016_us_county_20m.shp")
if (!file.exists(counties_shp)) {
  loc <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
  dest <- paste0(counties_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = counties_prefix)
  unlink(dest)
  assert_that(file.exists(counties_shp))
}