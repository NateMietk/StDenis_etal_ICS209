ak_mask <- states %>%
  dplyr::filter(stusps == 'AK') 

figure_S1_df <- conus_209 %>%
  dplyr::filter(stusps == 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n(),
            fsr = log(max(WF_MAX_FSR, na.rm = TRUE)),
            structures_destroyed = log(sum(STR_DESTROYED_TOTAL, na.rm = TRUE)),
            total_personnel = log(sum(TOTAL_PERSONNEL_SUM, na.rm = TRUE)),
            burned_area_acres = log(sum(FINAL_ACRES, na.rm = TRUE)),
            costs = log(sum(PROJECTED_FINAL_IM_COST, na.rm = TRUE)),
            total_threatened = log(sum(STR_THREATENED_MAX, na.rm = TRUE))) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

# p1 <- make_map(figure_2_df, , '(A) log(Fire Spread Rate))', 'log(Fire Spread Rate)', ak_mask)
#placeholder
p1 <- make_map(figure_S1_df, 'fsr', '(A) Fire spread rate', 'log(Max FSR (acres/day))', ak_mask) 
p2 <- make_map(figure_S1_df, 'burned_area_acres', '(B) Burned Area', 'log(Burned Area)', ak_mask) 
p3 <- make_map(figure_S1_df, 'costs', '(D) Costs', 'log(Costs)', ak_mask)
p4 <- make_map(figure_S1_df, 'total_personnel', '(C) Total Personnel', 'log(Total Personnel)', ak_mask)
p5 <- make_map(figure_S1_df, 'total_threatened', '(F) Homes Threatened', 'log(Homes Threatened)', ak_mask)
p6 <- make_map(figure_S1_df, 'structures_destroyed', '(E) Homes Destroyed', 'log(Homes Destroyed)', ak_mask)


g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path('results', 'draft_figures', "Figure_S1.jpg"), g, width = 3, height = 5, 
       dpi = 1200, scale = 4, units = "cm") #saves g
