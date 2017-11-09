
source("src/R/functions/helper_functions.R")

wuw_eco_ICS <- st_read("data/ics209/spatial/ics209_wui_conus.gpkg") %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  filter(class == "WUI" | class == "VLD" | class == "Wildlands") %>%
  mutate(region = as.factor(
    if_else(
      ecoreg1.name %in% c("EASTERN TEMPERATE FORESTS","TROPICAL WET FORESTS","NORTHERN FORESTS"), 
      "East",
      if_else(
        ecoreg1.name %in% c("NORTH AMERICAN DESERTS", "SOUTHERN SEMIARID HIGHLANDS","TEMPERATE SIERRAS",
                            "MEDITERRANEAN CALIFORNIA","NORTHWESTERN FORESTED MOUNTAINS",
                            "MARINE WEST COAST FOREST"), 
        "West", "Central"))))
    
wuw_eco_ICS <- wuw_eco_ICS %>%
  mutate(
  class = clean_class(incident_unique_id,  as.character(class)),
    cause = if_else(
      incident_unique_id %in% c("ID-BOD-000553|2011|1","CA-RRU-062519|2005|1","NC-NCS-08081071|2008|1",
                                "CA-SCU-3094|2008|1","OR-SIF-003|2002|1"),
      "Human", as.character(cause)))

raw_with_wui <- fam %>%
  group_by(incident_unique_id, eyear) %>%
  summarise(ctd = max(costs_to_date_c),
         efc = max(est_final_costs_c)) %>%
  select(incident_unique_id, ctd, efc) %>%
  left_join(., wuw_eco_ICS, by = "incident_unique_id") %>%
  na.omit()
  
# Human related consequences
humcost_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by() %>%
  summarise(totcosts = sum(costs)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

wuw_area <- data.table(class=c("WUI", "VLD", "Wildlands"), 
                       class_area = c(784320, 2260783, 2598246))

p1 <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(class, cause) %>%
  summarise(ff = n(),
            costs = sum(costs),
            area_km2 = sum(area_km2)) %>%
  as.data.frame(.) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_costs = costs/humcost_cause$totcosts,
         area_norm = (costs/class_area)*100,
         cost_per_burnedarea = (costs/area_km2)*100) %>%
  mutate_all(funs(replace(., is.na(.), 0)))


 p1 %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_bar(aes(x = cause, y = cost_per_burnedarea,
               color = cause, fill = cause), 
           position = "dodge", stat = "identity") +
   theme_pub() +
   theme(legend.position = c(0.8, 0.8)) +
  facet_wrap( ~ class)


totals_by_cause <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by() %>%
  summarise(totdestroyed = sum(home.destroyed, comm.destroyed),
            totthreatened = sum(home.threat, comm.threat),
            totdeath = sum(fatalities),
            totperson = sum(tot.pers),
            totaerial = sum(tot.aerial),
            totcosts = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom)

wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(cause, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            threatened = sum(home.threat, comm.threat),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_destroyed = (destroyed/totals_by_cause$totdestroyed)*100,
         pct_threatened = (threatened/totals_by_cause$totthreatened)*100,
         pct_deaths = (deaths/totals_by_cause$totdeath)*100,
         pct_person = (person/totals_by_cause$totperson)*100,
         pct_aerial = (aerial/totals_by_cause$totaerial)*100,
         pct_costs = (costs/totals_by_cause$totcosts)*100,
         cost_per_km2 = (costs/totarea))

totals_by_year <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(syear) %>%
  summarise(totdestroyed = sum(home.destroyed, comm.destroyed),
            totdeath = sum(fatalities),
            totperson = sum(tot.pers),
            totaerial = sum(tot.aerial),
            totcosts = sum(costs)) %>%
  as.data.frame(.) %>%
  select(-geom)

totals_by_year_cause_class <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(syear, cause, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., totals_by_year, by = "syear") %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea)) %>%
  arrange(desc(cause, syear))

totals_by_year_mean <- totals_by_year_cause %>%
  select(-syear) %>%
  group_by(cause) %>%
  summarise_all(mean) %>%
  ungroup()

totals_by_year_cause_class <- wuw_eco_ICS %>% 
  filter(cause != "Unk") %>%
  group_by(syear, cause, class) %>%
  summarise(destroyed = sum(home.destroyed, comm.destroyed),
            deaths = sum(fatalities),
            person = sum(tot.pers),
            aerial = sum(tot.aerial),
            costs = sum(costs),
            totarea = sum(area_km2)) %>%
  as.data.frame(.) %>%
  select(-geom) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_by_year, by = "syear") %>%
  mutate(pct_destroyed = (destroyed/totdestroyed)*100,
         pct_deaths = (deaths/totdeath)*100,
         pct_person = (person/totperson)*100,
         pct_aerial = (aerial/totaerial)*100,
         pct_costs = (costs/totcosts)*100,
         cost_per_km2 = (costs/totarea),
         cost_norm_class_area = (costs/class_area)/totarea) %>%
  arrange(desc(cause, syear))

totals_by_year_cause_class %>%
  transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() +
  geom_bar(aes(x = cause, y = cost_norm_class_area,
               color = cause, fill = cause), 
           position = "dodge", stat = "identity") +
  theme_pub() +
  theme(legend.position = c(0.8, 0.8)) +
  facet_wrap( ~ class)
  
  
# Number of human related costs in the WUI

allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_costs = sum(costs))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_costs_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(costs = sum(costs)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_costs = (costs/tot_costs)*100)

mean_costs_wui_h <- mean(yrly_costs_wui_h$yrly_costs)

# Number of human related homes destroyed in the WUI
allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_destroy = sum(home.destroyed))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_destroy_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(destroy = sum(home.destroyed)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_destroy = (destroy/tot_destroy)*100)

sum_destroy_wui_h <- sum(yrly_destroy_wui_h$destroy)
mean_destroy_wui_h <- mean(yrly_destroy_wui_h$yrly_destroy)

# Number of human related fatalities in the WUI
allfires <- wuw_eco_ICS %>% 
  group_by(syear) %>%
  summarise(tot_fatalities = sum(fatalities))

allfires <- as.data.frame(allfires) %>%
  select(-geom)

yrly_fatalities_wui_h <- wuw_eco_ICS %>%
  filter(Class == "WUI" & cause == "Human") %>%
  group_by(syear) %>%
  summarise(deaths = sum(fatalities)) %>%
  left_join(., allfires, by = "syear") %>%
  mutate(yrly_fatalities = (deaths/tot_fatalities)*100)

sum_fatalities_wui_h <- sum(yrly_fatalities_wui_h$deaths)
mean_fatalities_wui_h <- mean(yrly_fatalities_wui_h$yrly_fatalities)

# Number of human related ignitions in the WUI
allfires <- wuw_eco_poly %>% 
  summarise(tot_fire = n())

humanWUIfires <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  filter(Class == "WUI" & IGNITION == "Human")

PctWUIHuamn <- (humanWUIfires$tot_fire/allfires$tot_fire)*100

# Social Impact of human started fires in the WUI

wuw_eco_ICS %>%
  group_by( cause) %>%
  summarise(costs = sum(costs),
            destory = sum(home.destroyed),
            threat = sum(home.threat),
            lives = sum(fatalities))

# number of people living in the WUI
t <- wuw_eco_wui %>%
  distinct(., BLK10, .keep_all = TRUE) %>%
  group_by(Class) %>%
  summarise(pop = sum(POP10),
            homes = sum(HHU10),
            area = sum(AREA_km2))

# For the level 1 ecoregions --------------------------------------------
wui_stat <- wuw_eco_wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(NA_L1NAME, Class) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  spread(Class, ClassArea)

ff <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by( Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, tot_fire)

bae_stats <- wuw_eco_bae %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(burn_area = sum(AREA_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area)

Eco_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)

conus_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)


cost_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, Ignition) %>% 
  summarise(totcosts = sum(costs)) %>%
  ungroup() %>%
  spread(Ignition, totcosts)

hdes_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, Ignition) %>% 
  summarise(totdes = sum(destroy)) %>%
  ungroup() %>%
  spread(Ignition, totdes)

fat_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(NA_L1NAME, Class, Ignition) %>% 
  summarise(totdea = sum(deaths)) %>%
  ungroup() %>%
  spread(Ignition, totdea)

# For the state
wui_stat <- wuw_eco_wui %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(STUSPS, Class) %>%
  summarise(ClassArea = sum(AREA_km2)) %>%
  ungroup()  %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  spread(Class, ClassArea)

ff <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(tot_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, tot_fire) %>%
  mutate(h_ff = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_ff = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_ff = h_ff/(h_ff+l_ff)*100) %>%
  select(STUSPS, Class, h_ff, l_ff, pct_ff)

bae_stats <- wuw_eco_bae %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(burn_area = sum(AREA_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area) %>%
  mutate(h_bae = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_bae = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_bae = h_bae/(h_bae+l_bae)*100) %>%
  select(STUSPS, Class, h_bae, l_bae, pct_bae)

Eco_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason) %>%
  mutate(h_fsea = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_fsea = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_fsea = ifelse(h_fsea > l_fsea, h_fsea/(l_fsea)*100, "--")) %>%
  select(STUSPS, Class, h_fsea, l_fsea, pct_fsea)

conus_IQRstats <- wuw_eco_poly %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(Class, IGNITION) %>% 
  summarise(fireseason = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  spread(IGNITION, fireseason)


cost_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totcosts = sum(costs)) %>%
  ungroup() %>%
  spread(Ignition, totcosts) %>%
  mutate(h_cost = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_cost = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_cost = h_cost/(h_cost+l_cost)*100) %>%
  select(STUSPS, Class, h_cost, l_cost, pct_cost)

hdes_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totdes = sum(destroy)) %>%
  ungroup() %>%
  spread(Ignition, totdes) %>%
  mutate(h_des = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_des = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_des = h_des/(h_des+l_des)*100) %>%
  select(STUSPS, Class, h_des, l_des, pct_des)

fat_stats <- wuw_eco_ICS %>% 
  transform(Class = factor(Class, levels = c("WUI", "VLD","Wildlands"))) %>%
  group_by(STUSPS, Class, Ignition) %>% 
  summarise(totdea = sum(deaths)) %>%
  ungroup() %>%
  spread(Ignition, totdea) %>%
  mutate(h_fat = ifelse(is.na(as.numeric(Human)), 0 , as.numeric(Human)),
         l_fat = ifelse(is.na(as.numeric(Lightning)), 0 , as.numeric(Lightning)),
         pct_fat = h_fat/(h_fat+l_fat)*100) %>%
  select(STUSPS, Class, h_fat, l_fat, pct_fat)

State_Stats <- ff %>%
  left_join(., bae_stats, by = c("STUSPS", "Class")) %>%
  left_join(., Eco_IQRstats, by = c("STUSPS", "Class")) %>%
  left_join(., cost_stats, by = c("STUSPS", "Class")) %>%
  left_join(., hdes_stats, by = c("STUSPS", "Class")) %>%
  left_join(., fat_stats, by = c("STUSPS", "Class")) %>%
  mutate(h_ff = round(ifelse(is.na(as.numeric(h_ff)), 0 , as.numeric(h_ff)),2),
         l_ff = round(ifelse(is.na(as.numeric(l_ff)), 0 , as.numeric(l_ff)),2),
         pct_ff = round(ifelse(is.na(as.numeric(pct_ff)), 0 , as.numeric(pct_ff)),2),
         h_bae = round(ifelse(is.na(as.numeric(h_bae)), 0 , as.numeric(h_bae)),2),
         l_bae = round(ifelse(is.na(as.numeric(l_bae)), 0 , as.numeric(l_bae)),2),
         pct_bae = round(ifelse(is.na(as.numeric(pct_bae)), 0 , as.numeric(pct_bae)),2),
         h_fsea = round(ifelse(is.na(as.numeric(h_fsea)), 0 , as.numeric(h_fsea)),2),
         l_fsea = round(ifelse(is.na(as.numeric(l_fsea)), 0 , as.numeric(l_fsea)),2),
         pct_fsea = round(ifelse(is.na(as.numeric(pct_fsea)), 0 , as.numeric(pct_fsea)),2),
         h_cost = round(ifelse(is.na(as.numeric(h_cost)), 0 , as.numeric(h_cost)),2),
         l_cost = round(ifelse(is.na(as.numeric(l_cost)), 0 , as.numeric(l_cost)),2),
         pct_cost = round(ifelse(is.na(as.numeric(pct_cost)), 0 , as.numeric(pct_cost)),2), 
         h_des = round(ifelse(is.na(as.numeric(h_des)), 0 , as.numeric(h_des)),2),
         l_des = round(ifelse(is.na(as.numeric(l_des)), 0 , as.numeric(l_des)),2),
         pct_des = round(ifelse(is.na(as.numeric(pct_des)), 0 , as.numeric(pct_des)),2),
         h_fat = round(ifelse(is.na(as.numeric(h_fat)), 0 , as.numeric(h_fat)),2),
         l_fat = round(ifelse(is.na(as.numeric(l_fat)), 0 , as.numeric(l_fat)),2),
         pct_fat = round(ifelse(is.na(as.numeric(pct_fat)), 0 , as.numeric(pct_fat)),2))

State_Stats_WUI_Wild <- State_Stats %>%
  filter(Class != "VLD")

State_Stats_VLD <- State_Stats %>%
  filter(Class == "VLD")              

State_Stats_CONUS <- State_Stats %>%
  select(., -STUSPS) %>%
  group_by(Class) %>%
  summarise_each(funs(sum)) %>%
  ungroup()



