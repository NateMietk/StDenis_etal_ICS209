
legacy <- read_csv("data/ics_209/input_tbls/famweb/SIT209 Legacy Data - Orginal.csv")

states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = proj_ea) %>%  # e.g. US National Atlas Equal Area
  mutate(state.id = STATEFP, 
         state.abv = STUSPS,
         state = NAME,
         state_km2 = as.numeric(st_area(geometry))/1000000) %>%
  dplyr::select(state.id, state.abv, state, state_km2)

legacy <- st_par(legacy, st_as_sf, n_cores = ncores,
                 coords = c("LONGITUDE", "LATITUDE"),
                 crs = "+init=epsg:4326")  %>%
  st_transform(st_crs(states))

us_legacy <- st_intersection(legacy, st_union(states))

legacy_erractis <- st_difference(legacy, st_union(states))

leg <- st_transform(legacy_erractis, crs = "+init=epsg:4326") %>%
  sfc_as_cols() %>%
  as.data.frame() %>%
  select(-geometry)

write_csv(leg, "data/ics_209/output_tbls/legacy_erratics.csv")


ics2014 <- read_csv("data/ics_209/input_tbls/latlong/2014latlongAndPOODataFinalSitrep.csv") %>%
  mutate(POO_LONGITUDE = if_else(POO_LONGITUDE < 0, POO_LONGITUDE, -POO_LONGITUDE),
         POO_LATITUDE = if_else(POO_LATITUDE<0, -1*POO_LATITUDE, POO_LATITUDE),
         POO_LONGITUDE = if_else(is.na(POO_LONGITUDE), 0, POO_LONGITUDE),
         POO_LATITUDE = if_else(is.na(POO_LATITUDE), 0, POO_LATITUDE))

states <- st_read(dsn = us_shp, quiet= TRUE) %>%
  st_parallel(., st_transform, n_cores = ncores, crs = proj_ea) %>%  # e.g. US National Atlas Equal Area
  mutate(state.id = STATEFP, 
         state.abv = STUSPS,
         state = NAME,
         state_km2 = as.numeric(st_area(geometry))/1000000) %>%
  dplyr::select(state.id, state.abv, state, state_km2)

ics2014 <- st_par(ics2014, st_as_sf, n_cores = ncores,
                 coords = c("POO_LONGITUDE", "POO_LATITUDE"),
                 crs = "+init=epsg:4326")  %>%
  st_transform(st_crs(states))

us_ics2014 <- st_intersection(ics2014, st_union(states))

ics2014_erractis <- st_difference(ics2014, st_union(states))

ics2014_erractis_df <- st_transform(ics2014_erractis, crs = "+init=epsg:4326") %>%
  sfc_as_cols() %>%
  as.data.frame() %>%
  select(-geometry)

write_csv(ics2014_erractis_df, "data/ics_209/output_tbls/ics2014_erratics.csv")
