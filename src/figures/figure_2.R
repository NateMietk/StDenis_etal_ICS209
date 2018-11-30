conus_mask <- states %>%
  dplyr::filter(stusps != 'AK') 

figure_2_df <- conus_209 %>%
  dplyr::filter(stusps != 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n(),
            structures_destroyed = log(sum(TOTAL_RES_DESTROYED, na.rm = TRUE)),
            total_personnel = log(sum(TOTAL_PERSONNEL, na.rm = TRUE)),
            burned_area_acres = log(sum(ACRES, na.rm = TRUE)),
            costs = log(sum(EST_IM_COST_TO_DATE, na.rm = TRUE)),
            total_threatened = log(sum(TOTAL_RES_THREATENED, na.rm = TRUE))) %>%
  as.data.frame() %>%
  dplyr::select(-geom) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

p1 <- make_map(figure_2_df, 'n', '(A) log(# Events)', 'log(# Events)', conus_mask)
p2 <- make_map(figure_2_df, 'burned_area_acres', '(B) log(Burned Area)', 'log(Burned Area)', conus_mask) 
p3 <- make_map(figure_2_df, 'total_personnel', '(C) log(Total Personnel)', 'log(Total Personnel)', conus_mask)
p4 <- make_map(figure_2_df, 'costs', '(D) log(Costs)', 'log(Costs)', conus_mask)
p5 <- make_map(figure_2_df, 'structures_destroyed', '(E) log(Homes Destroyed)', 'log(Homes Destroyed)', conus_mask)
p6 <- make_map(figure_2_df, 'total_threatened', '(F) log(Homes Threatened)', 'log(Homes Threatened)', conus_mask)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)