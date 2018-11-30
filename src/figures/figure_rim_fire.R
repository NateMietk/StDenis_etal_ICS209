rim_fire <- fread(file.path(ics_inputs, 'ics209_allsitreps1999to2014.csv')) %>%
  filter(INCIDENT_ID == '2013_CA-STF-002857_RIM')

tmp <- rim_fire %>%
  mutate(date = ymd(as_date(REPORT_TO_DATE))) %>% 
  as_tibble() %>%
  group_by(date) %>%
  summarise(costs = max(EST_IM_COST_TO_DATE),
            structures_destroyed = max(STR_DESTROYED),
            total_personnel = max(TOTAL_PERSONNEL),
            burned_area_acres = max(ACRES),
            total_threatened = max(STR_THREATENED)) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

p1 <- tmp %>%
  ggplot(aes(x = date, y = costs)) +
  geom_point() +
  theme_pub()

p2 <- tmp %>%
  ggplot(aes(x = date, y = structures_destroyed)) +
  geom_point() +
  theme_pub()

p3 <- tmp %>%
  ggplot(aes(x = date, y = total_personnel)) +
  geom_point() +
  theme_pub()

p4 <- tmp %>%
  ggplot(aes(x = date, y = burned_area_acres)) +
  geom_point() +
  theme_pub()

p5 <- tmp %>%
  ggplot(aes(x = date, y = total_threatened)) +
  geom_point() +
  theme_pub()

grid.arrange(p1, p2, p3, p4, p5, ncol = 1)