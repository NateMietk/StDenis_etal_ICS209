# Create the Excel workbook
wb <- createWorkbook()

# Add a sheet for each table
addWorksheet(wb, "Table 1 - Ecoregion")
addWorksheet(wb, "Table 2 - States")
addWorksheet(wb, "Table 3 - GACC")

# Summarize the key 209 metrics based on Level 1 Ecoregions
eco_reg_table <- conus_209 %>%
  dplyr::filter(stusps != 'AK')  %>%
  as.data.frame() %>%
  group_by(na_l1name) %>%
  summarise(n = n(),
            burned_area_acres = sum(FINAL_ACRES, na.rm = TRUE),
            mean_fire_size = mean(FINAL_ACRES, na.rm = TRUE),
            meanMAX_fire_spread_rate = mean(WF_MAX_FSR, na.rm = TRUE),
            costs = sum(PROJECTED_FINAL_IM_COST, na.rm = TRUE),
            total_personnel = sum(TOTAL_PERSONNEL_SUM, na.rm = TRUE),
            total_threatened = sum(STR_THREATENED_MAX, na.rm = TRUE),
            structures_destroyed = sum(STR_DESTROYED_TOTAL, na.rm = TRUE)) %>%
  na.omit()

# Summarize the key 209 metrics based on US states
state_table <- conus_209 %>%
  as.data.frame() %>%
  group_by(stusps) %>%
  summarise(n = n(),
            burned_area_acres = sum(FINAL_ACRES, na.rm = TRUE),
            mean_fire_size = mean(FINAL_ACRES, na.rm = TRUE),
            meanMAX_fire_spread_rate = mean(WF_MAX_FSR, na.rm = TRUE),
            costs = sum(PROJECTED_FINAL_IM_COST, na.rm = TRUE),
            total_personnel = sum(TOTAL_PERSONNEL_SUM, na.rm = TRUE),
            total_threatened = sum(STR_THREATENED_MAX, na.rm = TRUE),
            structures_destroyed = sum(STR_DESTROYED_TOTAL, na.rm = TRUE)) %>%
  na.omit()

# Summarize the key 209 metrics based on GACCs
gacc_table <- conus_209 %>%
  as.data.frame() %>%
  group_by(gacc_label) %>%
  summarise(n = n(),
            burned_area_acres = sum(FINAL_ACRES, na.rm = TRUE),
            mean_fire_size = mean(FINAL_ACRES, na.rm = TRUE),
            meanMAX_fire_spread_rate = mean(WF_MAX_FSR, na.rm = TRUE),
            costs = sum(PROJECTED_FINAL_IM_COST, na.rm = TRUE),
            total_personnel = sum(TOTAL_PERSONNEL_SUM, na.rm = TRUE),
            total_threatened = sum(STR_THREATENED_MAX, na.rm = TRUE),
            structures_destroyed = sum(STR_DESTROYED_TOTAL, na.rm = TRUE)) %>%
  na.omit()

# Write out the tables into the workbook and relative sheet
writeData(wb, sheet = "Table 1 - Ecoregion", x = eco_reg_table)
writeData(wb, sheet = "Table 2 - States", x = state_table)
writeData(wb, sheet = "Table 3 - GACC", x = gacc_table)

# Save the workbook to the excel file
saveWorkbook(wb, file = file.path('results', 'draft_tables', "tables_raw.xlsx"),
             overwrite = TRUE)