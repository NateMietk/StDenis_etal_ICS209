source("src/R/functions/helper_functions.R")
source("src/R/functions/st_par.R")

x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf", "gridExtra", "raster",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "mblm", "broom")
lapply(x, library, character.only = TRUE, verbose = FALSE)

ncores <- 3

## Download and process State data
# Creat directories for state data
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_5m")
ics_prefix <- file.path(prefix, "ics_209")
ics_spatial <- file.path(ics_prefix, "spatial")
ecoregion_prefix <- file.path(raw_prefix, "ecoreg")
counties_prefix <- file.path(raw_prefix, 'cb_2016_us_county_20m')

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ics_prefix, ics_spatial, ecoregion_prefix, counties_prefix)
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

# Import USA states
states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  mutate(state.id = STATEFP, 
         state.abv = STUSPS,
         state = NAME,
         state_km2 = as.numeric(st_area(geometry))/1000000) %>%
  dplyr::select(state.id, state.abv, state, state_km2) 

# Import USA counties
counties <- st_read(dsn = counties_shp, quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = st_crs(states)) %>%  
  st_intersection(., st_union(states)) %>%
  mutate(county.fp = COUNTYFP,
         county.ns = COUNTYNS,
         county_km2 = as.numeric(st_area(geometry))/1000000) %>%
  dplyr::select(county.fp, county.ns, county_km2)

# states_counties <- counties %>%
#   st_join(., states, join = st_intersects) %>%
#   select(state.id, state.abv, state, county.fp, county.ns, state_km2, county_km2) 
# 
# # Import the Level 3 Ecoregions
# ecoregion <- st_read(dsn = ecoregion_shp, quiet= TRUE) %>%
#   st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
#   st_buffer(0) %>%
#   st_par(., st_transform, n_cores = ncores, crs = st_crs(counties)) %>% 
#   st_intersection(., st_union(states)) %>%
#   mutate(ecoreg1.code = NA_L1CODE,
#          ecoreg1.name = NA_L1NAME,
#          ecoreg2.code = NA_L2CODE,
#          ecoreg2.name = NA_L2NAME,
#          ecoreg3.code = US_L3CODE,
#          ecoreg3.name = US_L3NAME,
#          ecoreg3_km2 = as.numeric(st_area(geometry))/1000000) %>%
#   select(ecoreg1.code, ecoreg1.name, ecoreg2.code, 
#          ecoreg2.name, ecoreg3.code, ecoreg3.name, ecoreg3_km2)  
# 
# state_ecoregion <- st_par(counties, st_join, n_cores = ncores, y = ecoregion, join = st_intersects)

# # 25k Fishnet
# fishnet_25k <- st_make_grid(usa_shp, cellsize = 25000, what = 'polygons') %>%
#   st_sf('geometry' = ., data.frame('fishid25k' = 1:length(.))) %>%
#   st_intersection(., st_union(usa_shp)) %>%
#   st_join(., wui24km, join = st_intersects, left = TRUE)
# 
# # Create raster template
# r.raster <- raster()
# extent(r.raster) <- extent(as(fishnet_25k, "Spatial"))
# res(r.raster) <- 25000 # set cell size to 2500 metres