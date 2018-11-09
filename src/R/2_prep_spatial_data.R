# Import USA states
states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_transform(proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  rename_all(tolower) %>%
  dplyr::select(stusps) 


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

if (!exists("counties")){
  counties <- st_read(file.path(counties_prefix, "cb_2016_us_county_20m.shp")) %>%
    sf::st_transform(st_crs(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(countyfp)
}

if (!file.exists(file.path(bounds_dir, 'hex_grid_50k.gpkg'))) {
  hex_points <- spsample(as(states, 'Spatial'), type = "hexagonal", cellsize = 50000)
  hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 50000)
  hexnet_50k <- st_as_sf(hex_grid) %>%
    mutate(hexid50k = row_number()) %>%
    st_intersection(., st_union(states))
} else {
  hexnet_50k <- st_read(file.path(bounds_dir, 'hex_grid_50k.gpkg'))
}

# Import ICS-209 from 1999-2014 -----------------------------
ics_209 <- st_read(file.path(ics_inputs, "ics209_allsitreps1999to2013.csv")) %>%
  as_tibble() %>%
  mutate(LONGITUDE = as.numeric(POO_LONGITUDE),
         LATITUDE = as.numeric(POO_LATITUDE))

# Clip the ICS-209 data to the CONUS and remove unknown cause
if(!file.exists(file.path(ics_spatial, "ics209_conus.gpkg"))) {
  # Make the cleaned ICS-209 data spatial
  ics_209_pt <- st_par(ics_209, st_as_sf, n_cores = 1,
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "+init=epsg:2163") %>%
    st_par(., st_transform, n_cores = 1, crs = st_crs(states))
  
  conus_209 <- st_par(fam_clean_pt, st_intersection, n_cores = 1, y = st_union(states)) %>%
    st_par(., st_intersection, n_cores = 1, y = ecoregions_l3) %>%
    st_par(., st_intersection, n_cores = 1, y = counties) %>%
    st_par(., st_intersection, n_cores = 1, y = hexnet_50k)
  
  conus_209 <- st_write(file.path(ics_spatial, "ics209_conus.gpkg"), delete_layer=TRUE)

} else {
  conus_209 <- st_read(file.path(ics_spatial, "ics209_conus.gpkg"))
}
