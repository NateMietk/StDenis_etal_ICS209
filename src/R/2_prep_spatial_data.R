# Import USA states
states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_transform(proj_ea) %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Hawaii", "Puerto Rico",
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

if (!file.exists(file.path(bounds_dir, 'hex_grid_50k.gpkg'))) {
  
  hexnet_50k <- st_sf(geom=st_make_grid(states, cellsize = 50000, square = FALSE), crs=st_crs(states)) %>%
    st_cast('MULTIPOLYGON') %>%
    mutate(hexid50k = row_number())
  
  st_write(hexnet_50k, file.path(bounds_dir, 'hex_grid_50k.gpkg'), delete_layer = TRUE)
  
} else {
  hexnet_50k <- st_read(file.path(bounds_dir, 'hex_grid_50k.gpkg'))
}

# Import ICS-209 from 1999-2014 -----------------------------
ics_209 <- fread(file.path(ics_inputs, "ics209_allsitreps1999to2014.csv")) %>%
  as_tibble() %>%
  mutate(POO_LONGITUDE = as.numeric(POO_LONGITUDE),
         POO_LATITUDE = as.numeric(POO_LATITUDE))

# Clip the ICS-209 data to the CONUS and remove unknown cause
if(!file.exists(file.path(ics_spatial, "ics209_conus.gpkg"))) {
  # Make the cleaned ICS-209 data spatial
  ics_209_pt <- st_as_sf(ics_209, coords = c("POO_LONGITUDE", "POO_LATITUDE"),
                         crs = "+init=epsg:4326") %>%
    st_transform(crs = st_crs(states))
  
  conus_209 <- ics_209_pt %>%
    st_intersection(., states) %>%
    st_join(., ecoregions_l3) %>%
    st_join(., hexnet_50k)
  
  st_write(conus_209, file.path(ics_spatial, "ics209_conus.gpkg"), delete_layer=TRUE)

} else {
  conus_209 <- st_read(file.path(ics_spatial, "ics209_conus.gpkg"))
}
