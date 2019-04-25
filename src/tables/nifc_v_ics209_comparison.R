
download.file('https://www.nifc.gov/fireInfo/fireInfo_documents/SuppCosts.pdf',
              file.path(ics209_input_dir, 'SuppCosts.pdf'))

nifc_table <- pdf_text(file.path(ics209_input_dir, 'SuppCosts.pdf'))

clean_table <- function(table){
  table <- str_split(table, "\n", simplify = TRUE)
  country_name <- table[1, 1] %>% 
    stringr::str_squish()
  table_start <- 1
  table_end <- stringr::str_which(table, "The Department of Interior agencies include: Bureau of Indian Affairs, Bureau of Land Management; National")
  table <- table[(table_start + 1 ):(table_end - 1)]
  table <- str_replace_all(table, "\\s{2,}", "|") %>%
    str_replace_all(., ",", "")
  
  
  text_con <- textConnection(table)
  write.csv(table, 'src/tables/nifc_totals.csv')
  data_table <- read.csv('src/tables/nifc_totals.csv', sep = "|")
  data_table <- transform(data_table, n = colsplit(X.x, split = "\\|", names = c('x', "Year", "Fires", "Acres", "Forest Service", "DOI Agencies", "Total"))) %>%
    dplyr::slice(2:35) %>%
    dplyr::select(year = n.Year,
                  n_fires = n.Fires,
                  acres = n.Acres,
                  total_costs = n.Total) %>%
    as_tibble() %>%
    mutate_all(funs(str_replace(., "\\$", ""))) %>%
    mutate_all(funs(as.numeric(as.character(.)))) %>%
    mutate(year = as.integer(year))
  unlink('src/tables/nifc_totals.csv')
  }

nifc <- map_df(nifc_table, clean_table)

ics209 <- conus_209 %>%
  mutate(year = start_year) %>%
  group_by(year) %>%
  summarise(n_fires_209 = n(),
            acres_209 = sum(final_acres, na.rm = TRUE),
            total_costs_209 = sum(projected_final_im_cost, na.rm = TRUE)) %>%
  as_tibble(as.data.frame(.)) %>%
  dplyr::select(-geom) %>%
  mutate_all(funs(as.numeric(as.character(.)))) %>%
  mutate(year = as.integer(year))

nifc_209s <- ics209 %>%
  left_join(., nifc, by = 'year') %>%
  mutate(pct_n_fires = (n_fires_209/n_fires)*100,
         pct_acres =  (acres_209/acres)*100,
         pct_costs = (total_costs_209/total_costs)*100) %>%
  dplyr::select(year, n_fires_209, n_fires, pct_n_fires,
                acres_209, acres,  pct_acres, 
                total_costs_209, total_costs, pct_costs)

write_csv(nifc_209s, file.path(draft_table_dir, 'nifc_v_209_comparison.csv'))
