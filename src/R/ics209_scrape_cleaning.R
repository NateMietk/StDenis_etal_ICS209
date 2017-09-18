# This script is to call in and clean the scraped data from Lise.

# Create list of all CSV files to import
sit_files <- list.files("data/sitrep_scrape", pattern = "csv", full.names = TRUE)

# Read in all files and append to list
mylist <- lapply(sit_files, fread, header= TRUE, stringsAsFactors = TRUE, check.names = TRUE)

# Merge all lists into one data.frame and replace all NA values with zeros
SIT_rep <- rbindlist(mylist, fill = TRUE) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  select(-othr)

SIT_ll <- fread("data/ics209/ll/ics209Incidents-cleaned_ll.csv") %>%
  mutate(syear = Year) %>%
  select(-IncidentName)

names(SIT_rep) %<>% tolower 
names(SIT_ll) %<>% tolower 

scrape <- SIT_rep %>%
  separate(state.unit, "state", sep = "-", extra = "drop") %>%
  mutate(rdate = mdy(date),
         rdoy = yday(rdate),
         rmonth = month(rdate),
         rday = day(rdate),
         ryear = year(rdate),
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
         agency.support = (as.numeric(ag) + as.numeric(aphis) + as.numeric(bia) + as.numeric(blm) + as.numeric(bor) + as.numeric(c.l) + as.numeric(cdf) + as.numeric(dc) + as.numeric(ddq) + 
                             as.numeric(doc) + as.numeric(dod) + as.numeric(fws) + as.numeric(ia) + as.numeric(intl) + as.numeric(lgr) + as.numeric(nps) + as.numeric(oes) + as.numeric(pri) + 
                             as.numeric(st) + as.numeric(usfs) + as.numeric(wxw) + as.numeric(cnty) + as.numeric(othr) + as.numeric(dhs) + as.numeric(aphi)),
         inctype = incident.type, 
         coststdate = round(ifelse(is.na(dollarToNumber_vectorised(ctd)), 0, dollarToNumber_vectorised(ctd)), 2),
         estfincosts = round(ifelse(is.na(dollarToNumber_vectorised(est.final.cost)), 0, dollarToNumber_vectorised(est.final.cost)),2),
         costs = ifelse(estfincosts == 0 & coststdate > 1, coststdate,
                        estfincosts)) %>%
  select(incidentnum, incidentname, inctype, rdate, rdoy, rday, rmonth, ryear, sdate, sdoy, sday, smonth, syear, report_length,
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
  group_by(incidentnum, syear, state) %>%
  summarise(lat = max(lat),
            long = min(lon),
            sdate = min(sdate),
            rdate = max(rdate),
            smonth = min(smonth),
            sday = min(sday),
            sdoy = min(sdoy),
            eyear = max(ryear),
            emonth = max(rmonth),
            eday = max(rday),
            edoy = max(rdoy),
            report_length = max(edoy - sdoy),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            home.destroyed = max(home.destroyed),
            home.threat = max(home.threat),
            max.pers = max(total.pers),
            max.aerial.support = 0,
            tot.personal = sum(total.pers),
            tot.aerial = 0,
            max.agency.support = max(agency.support),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))

