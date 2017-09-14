# This script is to call in and clean the scraped data from Lise.

# Create list of all CSV files to import
sit_files <- list.files("data/sitrep_scrape", pattern = "csv", full.names = TRUE)

# Read in all files and append to list
mylist <- lapply(sit_files, fread, header= TRUE, stringsAsFactors = TRUE, check.names = TRUE)

# Merge all lists into one data.frame and replace all NA values with zeros
SIT_rep <- rbindlist(mylist, fill = TRUE) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  select(-othr)

SIT_rep <- fread("data/ics209/ll/ics209Incidents-cleaned_ll.csv") %>%
  left_join(., SIT_rep, by = c("IncidentNum",	"IncidentName")) %>%
  mutate(Lon = ifelse(LONGITUDE_C > 0, -LONGITUDE_C, LONGITUDE_C),
         Lon = ifelse(is.na(LONGITUDE_C), 0, LONGITUDE_C),
         Lat = ifelse(is.na(LATITUDE_C), 0, LATITUDE_C))

names(SIT_rep) %<>% tolower 

scrape_clean <- SIT_rep %>%
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
         cause_binary = as.numeric(classify_ignition(cause)),
         total.pers = as.numeric(total.pers),
         home.damaged = x.res.damaged,
         home.threat = as.numeric(x.res.threatened),
         home.destroyed = as.numeric(x.res.destroyed),
         aerial.support = hel1.sr + hel2.sr + hel3.sr,
         agency.support = (as.numeric(ag) + as.numeric(aphis) + as.numeric(bia) + as.numeric(blm) + as.numeric(bor) + as.numeric(c.l) + as.numeric(cdf) + as.numeric(dc) + as.numeric(ddq) + 
                             as.numeric(doc) + as.numeric(dod) + as.numeric(fws) + as.numeric(ia) + as.numeric(intl) + as.numeric(lgr) + as.numeric(nps) + as.numeric(oes) + as.numeric(pri) + 
                             as.numeric(st) + as.numeric(usfs) + as.numeric(wxw) + as.numeric(cnty) + as.numeric(othr) + as.numeric(dhs) + as.numeric(aphi)),
         inctype = incident.type, 
         coststdate = round(ifelse(is.na(dollarToNumber_vectorised(ctd)), 0, dollarToNumber_vectorised(ctd)), 2),
         estfincosts = round(ifelse(is.na(dollarToNumber_vectorised(est.final.cost)), 0, dollarToNumber_vectorised(est.final.cost)),2),
         costs = ifelse(estfincosts == 0 & coststdate > 1, coststdate,
                        estfincosts)) %>%
  select(incidentnum, incidentname, lon, lat, inctype, rdate, rdoy, rday, rmonth, ryear, sdate, sdoy, sday, smonth, syear, report_length,
         area_km2, state, cause_binary, costs, coststdate, estfincosts, total.pers, aerial.support, agency.support, evacs, human.threat, fatalities, home.threat, home.damaged, home.destroyed) %>%
  filter(inctype == "Wildland Fire(Monitor/Confine/Contain) " | 
           inctype == "Wildland Fire(Full Suppression/Perimeter Control)"  | 
           inctype == "Wildland Fire(Point or Zone Protection/Limited Perimeter Control)" | 
           inctype == "Wildland Fire" |
           inctype == "Wildland Fire(Not Entered)" |
           inctype != "Wildland Fire Used for Resource Benefit" |
           inctype == "Wildfire(Confine)" | 
           inctype == "Wildfire(Full Suppression)"  | 
           inctype == "Wildfire(Monitor)" | 
           inctype == "Wildfire(Point Zone Protection)")

scrape_clean <- scrape_clean %>%
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
            max.aerial.support = max(aerial.support),
            tot.personal = sum(total.pers),
            tot.aerial = sum(aerial.support),
            max.agency.support = max(agency.support),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))


