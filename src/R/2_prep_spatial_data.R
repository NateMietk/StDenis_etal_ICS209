# Import USA states
states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_transform(proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  rename_all(tolower) %>%
  dplyr::select(stusps) 

# Import and clean by GACC
# Annoyingly I have to sign into ArcGIS Online to download the data
# https://hub.arcgis.com/items/72213d9266eb4aefa4403a1bf21dfd61?geometry=-266.765%2C-40.581%2C93.235%2C86.606
if(!file.exists(file.path(fire_dir, 'mtbs.gpkg'))) {
  gacc <- st_read(dsn = gacc_prefix, layer = 'National_GACC_2018', quiet= TRUE) %>%
    st_transform(st_crs(states)) %>%
    st_intersection(., st_union(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(gacc_label, gacc_name)
  
  st_write(mtbs, file.path(bounds_dir, 'gacc.gpkg'), delete_layer = TRUE)
  
} else {
  gacc <- st_read(file.path(bounds_dir, 'gacc.gpkg'))
}

# Import and clean the MTBS polygons
if(!file.exists(file.path(fire_dir, 'mtbs.gpkg'))) {
  
  mtbs_shp <- file.path(raw_dir_mtbs, 'mtbs_perims_DD.shp')
  if (!file.exists(mtbs_shp)) {
    dest <- paste0(raw_dir_mtbs, ".zip")
    download.file("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip", dest)
    unzip(dest, exdir = raw_dir_mtbs)
    unlink(dest)
    assert_that(file.exists(mtbs_shp))
  }
  
  mtbs <- st_read(dsn = raw_dir_mtbs, layer = 'mtbs_perims_DD', quiet= TRUE) %>%
    st_transform(st_crs(states)) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, 
                  discovery_day, discovery_month, discovery_doy, acres)
  st_write(mtbs, file.path(fire_dir, 'mtbs.gpkg'), delete_layer = TRUE)
} else {
  mtbs <- st_read(file.path(fire_dir, 'mtbs.gpkg'))
}


if (!exists("ecoregions_l3")){
  ecoregions_l3 <- st_read(file.path(ecoregion_prefix, 'us_eco_l3.shp')) %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
    sf::st_transform(st_crs(states)) %>%
    dplyr::mutate(NA_L3NAME = as.character(NA_L3NAME),
                  NA_L3NAME = ifelse(NA_L3NAME == 'Chihuahuan Desert',
                                     'Chihuahuan Deserts',
                                     NA_L3NAME)) %>%
    rename_all(tolower) %>%
    dplyr::select(us_l3name, na_l2name, na_l1name)
}

if (!file.exists(file.path(bounds_dir, 'hex_grid_50k.gpkg'))) {
  
  hexnet_50k <- st_sf(geom=st_make_grid(states, cellsize = 50000, square = FALSE), crs=st_crs(states)) %>%
    st_cast('MULTIPOLYGON') %>%
    mutate(hexid50k = row_number())
  
  st_write(hexnet_50k, file.path(bounds_dir, 'hex_grid_50k.gpkg'), delete_layer = TRUE)
  
} else {
  hexnet_50k <- st_read(file.path(bounds_dir, 'hex_grid_50k.gpkg'))
}

# Clip the ICS-209 data to the CONUS and remove unknown cause
if(!file.exists(file.path(ics_spatial, "ics209_conus.gpkg"))) {
  # Import ICS-209 from 1999-2014
  ics_209 <- fread(file.path(ics_inputs, "ics209_allWFincidents1999to2014.csv")) %>%
    as_tibble() %>%
    mutate(POO_LONGITUDE = ifelse(is.na(POO_LONGITUDE),0,as.numeric(POO_LONGITUDE)),
           POO_LATITUDE = ifelse(is.na(POO_LATITUDE),0,as.numeric(POO_LATITUDE)))
  
  # Make the cleaned ICS-209 data spatial
  ics_209_pt <- st_as_sf(ics_209, coords = c("POO_LONGITUDE", "POO_LATITUDE"),
                         crs = "+init=epsg:4326") %>%
    st_transform(crs = st_crs(states))
  
  conus_209 <- ics_209_pt %>%
    st_intersection(., states) %>%
    st_join(., ecoregions_l3) %>%
    st_join(., gacc) %>%
    st_join(., hexnet_50k)
  
  st_write(conus_209, file.path(ics_spatial, "ics209_conus.gpkg"), delete_layer=TRUE)

} else {
  conus_209 <- st_read(file.path(ics_spatial, "ics209_conus.gpkg"))
}
