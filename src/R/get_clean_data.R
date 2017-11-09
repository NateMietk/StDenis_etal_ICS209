source("src/R/functions/helper_functions.R")
source("src/R/functions/st_par.R")

x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel")
lapply(x, library, character.only = TRUE, verbose = FALSE)

## Download and process State data
# Creat directories for state data
prefix <- ifelse(Sys.getenv("LOGNAME") == "NateM", file.path("data"),
                 ifelse(Sys.getenv("LOGNAME") == "nami1114", file.path("data"),
                        file.path("../data")))
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_5m")
ics_prefix <- file.path(prefix, "ics209")
anthro_prefix <- file.path(prefix, "anthro")
ecoregion_prefix <- file.path(prefix, "ecoreg")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, anthro_prefix, us_prefix, ics_prefix, ecoregion_prefix)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

ncores <- 10

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

# Download the Level 2 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "NA_CEC_Eco_Level2.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

# Download the Level 1 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "NA_CEC_Eco_Level1.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

# Import USA states
usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_5m", quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  mutate(state.abv = STUSPS,
         state = NAME,
         state_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(state.abv, state, state_km2) 

# Import the Level 1 Ecoregions
ecoreg1 <- st_read(dsn = ecoregion_prefix, layer = "NA_CEC_Eco_Level1", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>% 
  st_intersection(., st_union(usa_shp)) %>%
  select(NA_L1CODE, NA_L1NAME) %>%
  mutate(ecoreg1.code = NA_L1CODE,
         ecoreg1.name = NA_L1NAME,
         ecoreg1_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(ecoreg1.code, ecoreg1.name, ecoreg1_km2) 

# Import the Level 2 Ecoregions
ecoreg2 <- st_read(dsn = ecoregion_prefix, layer = "NA_CEC_Eco_Level2", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>% 
  st_intersection(., st_union(usa_shp)) %>%
  select(NA_L2CODE, NA_L2NAME) %>%
  mutate(ecoreg2.code = NA_L2CODE,
         ecoreg2.name = NA_L2NAME,
         ecoreg2_km2 = as.numeric(st_area(geometry))/1000000) %>%
  select(ecoreg2.code, ecoreg2.name, ecoreg2_km2)  

# Import the Level 3 Ecoregions
ecoreg3 <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  st_buffer(0) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>% 
  st_intersection(., st_union(usa_shp)) %>%
  select(US_L3CODE, US_L3NAME) %>%
  mutate(ecoreg3.code = US_L3CODE,
         ecoreg3.name = US_L3NAME,
         ecoreg3_km2 = as.numeric(st_area(geometry))/1000000) %>%
  st_par(., st_intersection, n_cores = ncores, y = st_union(usa_shp)) %>%
  select(ecoreg3.code, ecoreg3.name, ecoreg3_km2)  

names(usa_shp) %<>% tolower
names(ecoreg1) %<>% tolower
names(ecoreg2) %<>% tolower
names(ecoreg3) %<>% tolower

state_ecoregion <- st_par(usa_shp, st_intersection, n_cores = ncores, y = ecoreg1) %>%
  st_par(., st_intersection, n_cores = ncores, y = ecoreg2) %>%
  st_par(., st_intersection, n_cores = ncores, y = ecoreg3) 

