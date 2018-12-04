# Create the Excel workbook
wb <- createWorkbook()

# Add a sheet for each table
addWorksheet(wb, "Table 1")
addWorksheet(wb, "Table 2")

# Summarize the key 209 metrics based on Level 1 Ecoregions
eco_reg_table <- conus_209 %>%
  dplyr::filter(stusps != 'AK')  %>%
  as.data.frame() %>%
  group_by(na_l1name) %>%
  summarise(n = n(),
            burned_area_acres = (sum(ACRES, na.rm = TRUE)),
            total_threatened = (sum(TOTAL_RES_THREATENED, na.rm = TRUE)),
            structures_destroyed = (sum(TOTAL_RES_DESTROYED, na.rm = TRUE)),
            total_personnel = (sum(TOTAL_PERSONNEL, na.rm = TRUE)),
            costs = (sum(EST_IM_COST_TO_DATE, na.rm = TRUE))
            ) %>%
  na.omit()

# Summarize the key 209 metrics based on US states
state_table <- conus_209 %>%
  as.data.frame() %>%
  group_by(stusps) %>%
  summarise(n = n(),
            burned_area_acres = (sum(ACRES, na.rm = TRUE)),
            total_threatened = (sum(TOTAL_RES_THREATENED, na.rm = TRUE)),
            structures_destroyed = (sum(TOTAL_RES_DESTROYED, na.rm = TRUE)),
            total_personnel = (sum(TOTAL_PERSONNEL, na.rm = TRUE)),
            costs = (sum(EST_IM_COST_TO_DATE, na.rm = TRUE))
  ) %>%
  na.omit()

# Write out the tables into the workbook and relative sheet
writeData(wb, sheet = "Table 1", x = eco_reg_table)
writeData(wb, sheet = "Table 2", x = state_table)

# Save the workbook to the excel file
saveWorkbook(wb, file = file.path('results', 'draft_tables', "tables_raw.xlsx"),
             overwrite = T)
