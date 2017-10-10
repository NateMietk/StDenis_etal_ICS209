
x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf", 
       "assertthat", "purrr", "httr", "rvest", "lubridate")
lapply(x, library, character.only = TRUE, verbose = FALSE)

## Download and process State data
# Creat directories for state data
raw_prefix <- file.path("data", "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ics_prefix <- file.path("data", "ics209")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(raw_prefix, us_prefix, ics_prefix)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

us_shp <- file.path(us_prefix, "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+proj=longlat +datum=WGS84") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  mutate(group = 1)

clean_estimated_costs <- function(x, y) {
  # x = incident_unique_id
  # y = est_final_costs
  
  est_final_costs_c <- 
    ifelse(x == "CA-ANF-4306|2007|1"       & y == 300000000, 3000000, 
    ifelse(x == "NC-NCS-08-048-065|2008|1" & y == 270000000, 27000000,
    ifelse(x == "AZ-COP-001102|2011|1"     & y ==  235000000, 2350000, 
    ifelse(x == "AZ-SCA-0072|2013|1"       & y == 200000000, 2000000,
    ifelse(x == "OR-DEF-000537|2012|1"     & y == 172375900, 17237590,
    ifelse(x == "CA-LNU-005007|2006|1"     & y == 100000000, 1000000, 
    ifelse(x == "CA-LPF-2142|2003|1"       & y == 15600000, 1560000,
    ifelse(x == "CA-LPF-2142|2003|1"       & y == 20000000, 2000000,
    ifelse(x == "CA-LPF-2142|2003|1"       & y == 87800000, 8780000,
    ifelse(x == "CA-INF-724|2005|1"        & y == 82500000, 825000,
    ifelse(x == "CA-SRF-1224|2008|1"       & y == 74000000, 7400000,
    ifelse(x == "AZ-TNF-039|2012|1"        & y == 73000000, 7300000,
    ifelse(x == "WA-OWF-199|2003|1"        & y > 39000000, 40000000,
    ifelse(x == "MT-BDF-0000118|2013|1"    & y == 60000000, 6000000,
    ifelse(x == "OR-UPF-073|2002|1"        & y == 54940000, 5494000,    
    ifelse(x == "CO-PSF-404|2002|1"        & y == 52000000, 45000000, 
    ifelse(x == "CO-LRX-329|2012|1"        & y > 39200000, 39200000, 
    ifelse(x == "MT-LNF-000264|2003|1"     & y == 49900000, 25000000, 
    ifelse(x == "MT-FNF-036|2003|1"        & y > 35000000, 37000000, 
    ifelse(x == "CA-SRF-1126|2008|1"       & y > 24000000, 24000000, 
    ifelse(x == "WA-OWF-271|2004|1"        & y >  42000000 , 28000000,
    ifelse(x == "WA-OWF-271|2004|1"        & y >  42000000 , 28000000,
    ifelse(x == "WY-YNP-0202|2002|1"       & y == 40000000 , 4000000,
    ifelse(x == "WY-YNP-0202|2002|1"       & y == 40000000 , 4000000,
    ifelse(x == "WA-OWF-199|2003|1"        & y > 40000000 , 40000000,
    ifelse(x == "WA-OWF-199|2003|1"        & y > 40000000 , 40000000,
    ifelse(x == "UT-MLF-2190|2002|1"       & y == 38000000 , 3800000,
    ifelse(x == "CA-KNF-5659|2012|1"       & y > 28000000, 28000000,
    ifelse(x == "CO-GMF-091|2002|1"        & y == 37000000, 370000,
    ifelse(x == "MT-FHA-115|2007|1"        & y == 37000000, 32000000,
    ifelse(x == "CA-SQF-001356|2008|1"     & y == 35000000, 25000000,
    ifelse(x == "CA-SRF-1131|2009|1"       & y > 20000000, 17500000,
    ifelse(x == "MT-FNF-036|2003|1"        & y == 48000000, 37000000,
    ifelse(x == "CA-SRF-997|2006|1"        & y > 20000000, 18000000,
    ifelse(x == "CO-RMP-000197|2010|1"     & y == 33000000, 3300000,
    ifelse(x == "CA-INF784|2007|1"         & y == 32236000, 3223600,
    ifelse(x == "CA-MNF-964|2003|1"        & y == 31600000, 3160000,
    ifelse(x == "CA-MVU-05658|2002|1"      & y == 30000000, 27000000,
    ifelse(x == "CA-TMU-11011|2007|1"      & y >= 15000000, 13500000,
    ifelse(x == "NM-GNF-000143|2012|1"     & y == 30000000, 25000000,
    ifelse(x == "MT-LNF-000264|2003|1"     & y == 49000000, 25000000,
    ifelse(x =="AZ-TNF-000133|2006|1"      & y == 25000000, 250000,
    ifelse(x =="ID-BOF-000183|2006|1"      & y > 18000000 , 18000000,
    ifelse(x == "CA-SHF-002744|2012|1"     & y > 10000000, 10000000,
    ifelse(x == "CA-MNF-964|2003|1"        & y == 31600000, 20000000,
    ifelse(x == "AZ-CNF-090|2003|1"        & y > 16400000, 16400000,
    ifelse(x == "CA-AFV-0364|2002|1"       & y == 17000000, 1700000,
    ifelse(x == "GA-GAS-201103|2011|1"     & y == 20000000, 2000000,
    ifelse(x == "OR-WSA-059|2002|1"        & y == 25000000, 2500000,
    ifelse(x == "MT-LCF-024|2001|1"        & y == 14000000, 1400000,
    ifelse(x == "MT-GNF-01-037|2001|1"     & y == 13000000, 1300000,
    ifelse(x == "NV-HTF-1365|2006|1"       & y == 14000000, 1400000,
    ifelse(x == "MN-MNS-121009|2012|1"     & y == 11000000, 1100000,
    ifelse(x == "MT-BDF-000181|2013|1"     & y == 14000000, 1400000,
    ifelse(x == "NV-WID-020210|2012|1"     & y == 17000000, 100000,
    ifelse(x == "NV-HTF-1545|2002|1"       & y == 8000000, 800000,
    ifelse(x == "UT-SLD-000478|2012|1"     & y == 3000000 | y == 8000000, 800000,
    ifelse(x == "OR-VAD-37|2013|1"         & y == 7500000, 750000,
    ifelse(x == "WY-BHF-000182|2007|1"     & y == 8400000, 840000,
           y)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  }

clean_costs_to_date <- function(x, y) {
  # x = incident_unique_id
  # y = est_final_costs
  
  costs_to_date_c <- 
    ifelse(x == "CA-BTU-007660|2008|1"   & y == 876000000, 87600000,
    ifelse(x == "CA-STF-002857|2013|1"   & y == 1161000000, 116100000, 
    ifelse(x == "CA-CNF-002512|2013|1"   & y == 328900000 | y == 34885970 , 3200000,
    ifelse(x == "UT-UIF-18044|2001|1"    & y == 293000000, 29300, 
    ifelse(x == "CA-ANF-2297|2013|1"     & y == 234000000 | y == 234100000, 23400000,
    ifelse(x == "NM-GNF-000230|2013|1"   & y == 143000000, 14300000,
    ifelse(x == "CA-STF-002857|2013|1"   & y == 1161000000, 116100000, 
    ifelse(x == "AZ-PNF-120266|2012|1"   & y == 122729904, 12272990,
    ifelse(x == "CA-INF-801|2007|1"      & y == 100000000, 1000000,
    ifelse(x == "CA-BTU-0008116|2012|1"  & y == 170000000 | y == 97000000, 9700000,
    ifelse(x == "CA-BTU-0008116|2012|1"  & y == 870000000, 87000000,
    ifelse(x == "CA-ENF-017646|2004|1"   & y == 77421200, 7742120,
    ifelse(x == "CA-INF-801|2007|1"      & y == 100000000, 1000000,
    ifelse(x == "CA-LNU -005495|2012|1"  & y == 11000000, 1000000,
    ifelse(x == "CA-MNF-894|2006|1"      & y == 14389638, 1438963,
    ifelse(x == "CA-MVU-011019|2011|1"   & y == 12180000, 1518000,
    ifelse(x == "CA-RRU-056869|2003|1"   & y == 29060700, 2996070,
    ifelse(x == "CA-RRU-56851|2011|1"    & y == 1000000, 100000,
    ifelse(x == "CA-SHF-002744|2012|1"   & y == 364000000, 36400000,
    ifelse(x == "CA-SHF-2521|2012|1"     & y == 36000000, 3600000,
    ifelse(x == "CA-VNC-03080298|2003|1" & y == 27500000, 27500000,
    ifelse(x == "ID-CWF-050314|2005|1"   & y == 15000000, 1500000,
    ifelse(x == "CA-MVU-014084|2013|1"   & y == 92000000, 9200000,
    ifelse(x == "WA-OWF-000616|2013|1"   & y == 85000000, 850000,
    ifelse(x == "CA-ENF-017646|2004|1"   & y == 77421200 | y == 77440000, 7740000,
    ifelse(x == "MT-BRF-005432|2012|1"   & y == 77000000, 7700000,
    ifelse(x == "WY-MB1-062|2002|1"      & y == 75000000, 7500000,
    ifelse(x == "AK-TAS-313401|2013|1"   & y == 70388044, 7038044,
    ifelse(x == "OR-MAF-012257|2012|1"   & y == 15847055 | y == 62000000, 6200000,
    ifelse(x == "WA-OWF-000530|2013|1"   & y == 53252222, 5352222,
    ifelse(x == "WY-BTF-014|2002|1"      & y == 48860963, 4886963,
    ifelse(x == "OR-UPF-120132|2012|1"   & y == 29981000, 2998100,
    ifelse(x == "ID-CMS-430012|2005|1"   & y == 29000000 | y == 28000000, 2800000,
    ifelse(x == "WA-OWF-398|2006|1"      & y == 68175390, 88175390,
    ifelse(x == "CA-KNF-3536|2006|1"     & y == 27900000, 2790000,
    ifelse(x == "NV-HTF-1365|2006|1"     & y == 14000000, 1400000,
    ifelse(x == "AZ-TCA-130031|2013|1"   & y == 8300000, 830000,
    ifelse(x == "CA-BDU-11262|2003|1"    & y > 13000000, 13000000,
    y))))))))))))))))))))))))))))))))))))))
}

# Clean ICS-209 from 2001-2013 -----------------------------
fam_rep <- fread("data/ics209/input_tbls/famweb/ics209_2001_2013_wfonlyv3.csv") %>%
  mutate_all(funs(replace(., is.na(.), 0))) 

names(fam_rep) %<>% tolower 

fam <- fam_rep %>% 
  filter(!(un_ustate %in% c("AK", "HI", "PR"))) %>%
  filter(type_inc != "RX") %>%
  mutate(long = -longitude,
         lat = latitude,
         incidentnum = incident_number,
         incidentname = as.factor(incident_name),
         rdate = ymd(report_date),
         rdoy = yday(rdate),
         rmonth = month(rdate),
         rday = day(rdate),
         eyear = year(rdate),
         sdate = floor_date(ymd_hms(start_date), "day"),
         sdoy = yday(sdate),
         smonth = month(sdate),
         sday = day(sdate),
         syear = year(sdate),
         report_length =rdoy-sdoy,
         state = un_ustate,
         area_ha = ifelse(area_measurement == "SQ MILES", area*258.99903998855,
                          area*0.404686),
         area_km2 = area_ha*0.01,
         est_final_costs_c = clean_estimated_costs(incident_unique_id, est_final_costs),
         costs_to_date_c = clean_costs_to_date(incident_unique_id, costs_to_date), 
         costs = ifelse(est_final_costs_c == 0 & costs_to_date_c > 1, costs_to_date_c,
                        est_final_costs_c),
         cause_binary = ifelse(cause == "H", "2", 
                               ifelse(cause =="L", "1", "0"))) 

fam_clean <- fam %>%
  group_by(incidentnum, eyear, state) %>%
  summarise(incidentname = last(incidentname),
            lat = max(lat),
            long = min(long),
            emonth = max(rmonth),
            eday = max(rday),
            edoy = max(rdoy),
            syear = ifelse(is.na(max(syear)), max(eyear), max(syear)),
            smonth = ifelse(is.na(min(smonth)), min(emonth), min(smonth)),
            sday = ifelse(is.na(min(sday)), min(eday), min(sday)),
            sdoy = ifelse(is.na(min(sdoy)), min(edoy), min(sdoy)),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            home.destroyed = max(destroyed_res),
            home.threat = max(threatened_res),
            max.pers = max(imsr_total_personnel),
            max.aerial = max(imsr_num_aerial),
            tot.pers = sum(imsr_total_personnel),
            tot.aerial = sum(imsr_num_aerial),
            max.agency.support = max(imsr_num_agencies),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")))

write_csv(fam_clean, path = "data/ics209/output_tbls/ics209_conus.csv")



