
# Clean ICS-209 from 1999-2014 -----------------------------

ics_209 <- fread(file.path(ics_inputs, "nate_extract.csv")) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  as_tibble()
names(ics_209) %<>% tolower

latlong_legacy <- read_csv(file.path(ics_latlong, "legacy_cleaned_ll.csv"))

latlong_historic <- read_csv(file.path(ics_latlong, "historical_cleaned_ll.csv"))

latlong_2014 <- read_csv(file.path(ics_latlong, '2014_cleaned_ll.csv'))


ics_209_reports <- ics_209 %>%
  mutate(incident_unique_id = as.factor(incident_unique_id),
         incident_number = as.factor(incident_number),
         incident_name = as.factor(incident_name),
         
         report_to_date = ymd(as.Date(report_to_date, origin='1970-01-01')),
         report_to_doy = ifelse(is.na(yday(report_to_date)), 0, yday(report_to_date)),
         report_to_month = ifelse(is.na(month(report_to_date)), 0, month(report_to_date)),
         report_to_day = ifelse(is.na(day(report_to_date)), 0, day(report_to_date)),
         report_to_year = ifelse(is.na(year(report_to_date)), 0, year(report_to_date)),

         area_measurement = if_else(area_measurement == "", 'Acres', area_measurement),
         area_ha = ifelse(area_measurement == "Square Miles", curr_incident_area*258.99903998855,
                          ifelse(area_measurement == "Acres", curr_incident_area*0.4046859376605,
                                 curr_incident_area)),
        area_km2 = area_ha*0.01,
        cause_binary = ifelse(cause == "H", "2",
                              ifelse(cause == "O", "2",
                                      ifelse(cause =="L", "1", "0"))),
         confi = "H") %>%
  mutate(est_final_costs = ifelse(is.na(est_im_cost_to_date), projected_final_im_cost, projected_final_im_cost),
         costs = ifelse(projected_final_im_cost == 0 & est_im_cost_to_date > 1, est_im_cost_to_date,
                        projected_final_im_cost)) %>%
  dplyr::select( -projected_final_im_cost, 
                -est_im_cost_to_date, -est_final_costs)

ics_209_incidents1 <- ics_209_reports %>%
  group_by(incident_unique_id) %>%
  arrange(desc(incident_unique_id, report_to_date)) %>%
  summarise_at(
    .vars= vars( area_km2, costs, fatalities, total_aerial, total_personnel, destroyed_total, damaged_total, threatened_total,
                 damaged_res_total, destroyed_res_total, threatened_res_total, damaged_comm_total, destroyed_comm_total, 
                 threatened_comm_total), 
    .funs =  max) 

ics_209_incidents <- ics_209_reports %>%
  group_by(incident_unique_id) %>%
  arrange(desc(incident_unique_id, report_to_date)) %>% 
  summarise(incident_number = last(incident_number),
            incident_name = last(incident_name),
            sum_personnel = sum(total_personnel),
            sum_aerial = sum(total_aerial),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human",
                           ifelse(cause =="1", "Lightning", "Unk")),
            confi = last(confi),
            start_date = first(report_to_date),
            end_date = last(report_to_date)) %>%
  mutate(start_year = year(start_date),
         start_month = month(start_date),
         start_day = day(start_date),
         start_doy = yday(start_date),
         end_year = year(end_date),
         end_month = month(end_date),
         end_day = day(end_date),
         end_doy = yday(end_date)) %>%
  left_join(., ics_209_incidents1, by = "incident_unique_id") 
  left_join(., latlong, by = "incident_unique_id") %>%
  mutate(confidence = ifelse(is.na(confidence), confi, confidence),
         lat = ifelse(is.na(lat_c), lat.x, lat_c),
         long = ifelse(is.na(long_c), long.x, long_c),
         syear = syear.x,
         cause = cause.x) %>%
  dplyr::select(-lat_c, -long_c, -confi, -syear.y, -lat.y, -cause.x, -cause.y,
                -long.y, -lat.x, -long.x, -syear.x, -syear.y)

  
  
sumall <- ics_209_incidents %>%
  group_by() %>%
  summarise_at(
    .vars= vars(area_km2:threatened_comm_total),
    .funs =  sum 
    ) %>%
  gather(var, value)
               


