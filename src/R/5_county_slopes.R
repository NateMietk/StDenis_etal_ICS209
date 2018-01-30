library(lme4)

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


# find the slope for each variable by each county
damage_df <- as.tibble(as.data.frame(wui_counties)) %>%
  dplyr::select(-geometry) %>%
  # get rid of rows with missing years (we want a time trend, these don't help)
  filter(!is.na(eyear)) %>%
  droplevels() %>%
  mutate(obs_id = paste(eyear, county.ns), 
         cyear = c(scale(eyear)))


# Fit Poisson GLMMs to each response --------------------------------------
# for each response, I compared overdispersed and non-overdispersed Poisson
# glmms and chose the better of the two models. This has a random time trend 
# and intercept for county

damage_df$int_cost <- as.integer(damage_df$costs)
cost_model <- glmer(int_cost ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                   data = damage_df, family = poisson)

fatality_model <- glmer(fatalities ~ cyear + (1 + cyear|county.ns), 
                        data = damage_df, family = poisson)

home_dest_model <- glmer(home.destroyed ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                         data = damage_df, family = poisson)

home_threat_model <- glmer(home.threat ~ cyear + (1 + cyear | county.ns) + (1|obs_id), 
                         data = damage_df, family = poisson)

home_dam_model <- glmer(home.damage ~ cyear + (1 + cyear | county.ns), 
                        data = damage_df, family=poisson)

comm_dest_model <- glmer(comm.destroyed ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                         data = damage_df, family = poisson)

comm_threat_model <- glmer(comm.threat ~ cyear + (1 + cyear|county.ns), 
                         data = damage_df, family = poisson)

comm_damage_model <- glmer(comm.damage ~ cyear + (1 + cyear|county.ns), 
                           data = damage_df, family = poisson)

max_pers_model <- glmer(max.pers ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                        data = damage_df, family = poisson)

max_aerial_model <- glmer(max.aerial ~ cyear + (1 + cyear|county.ns), 
                          data = damage_df, family = poisson)

tot_pers_model <- glmer(tot.pers ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                        data = damage_df, family = poisson)

tot_aerial_model <- glmer(tot.aerial ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                        data = damage_df, family = poisson)

max_supp_model <- glmer(max.agency.support ~ cyear + (1 + cyear|county.ns) + (1|obs_id), 
                        data = damage_df, family = poisson)


# Extract county-level slopes from each model -----------------------------
# first, get names of all of the model fits
model_fits <- grep('_model$', x = ls(), value = TRUE)
names(model_fits) <- model_fits

# helper function to grab county slopes for one fit object
get_county_trend <- function(fit) {
  # this is the overall slope for (scaled) time + a county adjustment
  assert_that(class(fit) == 'glmerMod')
  slope <- fixef(fit)['cyear'] + ranef(fit)[['county.ns']]$cyear
  
  # scale by sd to get the expected increase on a log-scale per year
  slope / sd(damage_df$eyear)
}

# apply that function to all of the fits
county_slopes <- lapply(model_fits, get) %>%
  lapply(get_county_trend) %>%
  as.data.frame %>%
  as_tibble

# add the county column
county_slopes <- county_slopes %>%
  mutate(county.ns = levels(damage_df$county.ns))
