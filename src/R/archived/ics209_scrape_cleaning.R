# This script is to call in and clean the scraped data from Lise.

# Create list of all CSV files to import
sit_files <- list.files("data/ics209/input_tbls/sitrep_scrape", pattern = "csv", full.names = TRUE)

# Read in all files and append to list
mylist <- lapply(sit_files, fread, header= TRUE, stringsAsFactors = TRUE, check.names = TRUE)

# Merge all lists into one data.frame and replace all NA values with zeros
SIT_rep <- rbindlist(mylist, fill = TRUE) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  select(-othr)

SIT_ll <- fread("data/ics209/input_tbls/latlong/ics209Incidents-cleaned_ll.csv") %>%
  mutate_all(funs(replace(., is.na(.), 0)))

names(SIT_rep) %<>% tolower 
names(SIT_ll) %<>% tolower 

scrape <- SIT_rep %>%
  separate(state.unit, "state", sep = "-", extra = "drop") %>%
  mutate(incidentname = as.factor(incidentname),
         rdate = mdy(date),
         rdoy = yday(rdate),
         rmonth = month(rdate),
         rday = day(rdate),
         eyear = year(rdate),
         sdate = floor_date(mdy_hm(start), "day"),
         sdoy = yday(sdate),
         smonth = month(sdate),
         sday = day(sdate),
         syear = year(sdate),
         report_length =rdoy-sdoy,
         acres = ifelse(grepl('ACRES', size), gsub('\\ACRES', '', size), ""),
         sqmile = ifelse(grepl('SQ MILES', size), gsub('\\SQ MILES', '', size), ""),
         acres = ifelse(is.na(as.numeric(gsub(",", "", acres))), "", 
                            round(as.numeric(gsub(",", "", acres))*0.0040468599999991389238,2)),
         sqmile = ifelse(is.na(as.numeric(gsub(",", "", sqmile))), "", 
                             round(as.numeric(gsub(",", "", sqmile))*2.5899903999994489,2)),
         area_km2 = ifelse(is.na(as.numeric(paste0(acres, sqmile))), 0, 
                       as.numeric(paste0(acres, sqmile))),
         cause_binary = ifelse(cause == "Human", "2", 
                               ifelse(cause =="Lightning", "1", "0")),
         total.pers = ifelse(is.na(as.numeric(gsub(",", "", total.pers))), 0, as.numeric(gsub(",", "", total.pers))),
         home.damaged = ifelse(is.na(as.numeric(gsub(",", "", x.res.damaged))), 0, as.numeric(gsub(",", "", x.res.damaged))),
         home.threat = ifelse(is.na(as.numeric(gsub(",", "", x.res.threatened))), 0, as.numeric(gsub(",", "", x.res.threatened))),
         home.destroyed = ifelse(is.na(as.numeric(gsub(",", "", x.res.destroyed))), 0, as.numeric(gsub(",", "", x.res.destroyed))),
         aerial.support = 0,
         agency.support = 0,
         inctype = incident.type, 
         coststdate = round(ifelse(is.na(dollarToNumber_vectorised(ctd)), 0, dollarToNumber_vectorised(ctd)), 2),
         estfincosts = round(ifelse(is.na(dollarToNumber_vectorised(est.final.cost)), 0, dollarToNumber_vectorised(est.final.cost)),2),
         costs = ifelse(estfincosts == 0 & coststdate > 1, coststdate,
                        estfincosts)) %>%
  select(incidentnum, incidentname, inctype, rdate, rdoy, rday, rmonth, eyear, sdate, sdoy, sday, smonth, syear, report_length,
         area_km2, state, cause_binary, costs, coststdate, estfincosts, total.pers, agency.support, aerial.support, fatalities, home.threat, home.damaged, home.destroyed) %>%
  filter(inctype == "Wildland Fire(Monitor/Confine/Contain) " | 
           inctype == "Wildland Fire(Full Suppression/Perimeter Control)"  | 
           inctype == "Wildland Fire(Point or Zone Protection/Limited Perimeter Control)" | 
           inctype == "Wildland Fire" |
           inctype == "Wildland Fire(Not Entered)" |
           inctype != "Wildland Fire Used for Resource Benefit" |
           inctype == "Wildfire(Confine)" | 
           inctype == "Wildfire(Full Suppression)"  | 
           inctype == "Wildfire(Monitor)" | 
           inctype == "Wildfire(Point Zone Protection)") %>%
  left_join(., SIT_ll, by = c("incidentnum",	"syear")) %>%
  mutate(lon = ifelse(longitude_c > 0, -longitude_c, longitude_c),
         lon = ifelse(is.na(lon), 0, lon),
         lat = ifelse(is.na(latitude_c), 0, latitude_c))

scrape_clean <- scrape %>%
  group_by(incidentnum, eyear, state) %>%
  summarise(incidentname = last(incidentname),
            lat = max(lat),
            long = min(lon),
            emonth = max(rmonth),
            eday = max(rday),
            edoy = max(rdoy),
            syear = ifelse(is.na(max(syear)), max(eyear), max(syear)),
            smonth = ifelse(is.na(min(smonth)), min(emonth), min(smonth)),
            sday = ifelse(is.na(min(sday)), min(eday), min(sday)),
            sdoy = ifelse(is.na(min(sdoy)), min(edoy), min(sdoy)),
            report_length = max(edoy - sdoy),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            home.destroyed = max(home.destroyed),
            home.threat = max(home.threat),
            max.pers = max(total.pers),
            max.aerial = 0,
            tot.pers = sum(total.pers),
            tot.aerial = 0,
            max.agency.support = 0,
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))

