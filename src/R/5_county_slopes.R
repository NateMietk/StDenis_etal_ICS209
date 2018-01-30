
wui_209 <- st_read("https://s3-us-west-2.amazonaws.com/earthlab-natem/data/anthro/ics209/ics209_wui_conus.gpkg") %>%
  mutate(
    area_km2 = if_else(incident_unique_id == "CA-SQF-2511|2006|1", 4.9, 
                       if_else(incident_unique_id == "CO-PSF-283|2002|1", 16.5, area_km2)),
    class = clean_class(incident_unique_id,  as.character(class)),
    cause = if_else(
      incident_unique_id %in% c("ID-BOD-000553|2011|1","CA-RRU-062519|2005|1","NC-NCS-08081071|2008|1",
                                "CA-SCU-3094|2008|1","OR-SIF-003|2002|1"),
      "Human", as.character(cause)))

names(wui_209) %<>% tolower

wui_counties <- st_join(counties, wui_209, join = st_intersects) %>%
  group_by(county.ns, eyear) %>%
  summarise_at(vars(costs:max.agency.support), sum, na.rm = TRUE)

as.tibble(as.data.frame(wui_counties[1:10,])) %>% # For efficiency just run on the first 10 rows
  select(-geometry) %>% 
  gather(variable, value, -county.ns, -eyear) %>%
  group_by(variable) %>% 
  nest() %>%
  mutate(model = map(data, ~lm(value ~ county.ns, data = .x))) %>%
  unnest(model %>% map(tidy)) %>%
  filter(term != '(Intercept)') %>%
  mutate(term = as.factor(gsub('county.ns', '', .$term)))




