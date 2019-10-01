conus_mask <- states %>%
  dplyr::filter(stusps != 'AK') 

figure_2_df <- conus_209 %>%
  dplyr::filter(stusps != 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n(),
            fsr = log(max(wf_max_fsr, na.rm = TRUE)),
            structures_destroyed = log(sum(str_destroyed_total, na.rm = TRUE)),
            total_personnel = log(sum(total_personnel_sum, na.rm = TRUE)),
            burned_area_acres = log(sum(final_acres, na.rm = TRUE)),
            costs = log(sum(projected_final_im_cost, na.rm = TRUE)),
            total_threatened = log(sum(str_threatened_max, na.rm = TRUE))) %>%
  as.data.frame() %>%
  dplyr::select(-contains('geom')) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

p1 <- make_map(figure_2_df, 'fsr', '(A) Fire spread rate', 'log(Max FSR (acres/day))', conus_mask) 
p2 <- make_map(figure_2_df, 'burned_area_acres', '(B) Burned Area', 'log(Burned Area (acres))', conus_mask) 
p3 <- make_map(figure_2_df, 'costs', '(C) Costs', 'log(Costs ($))', conus_mask)
p4 <- make_map(figure_2_df, 'total_personnel', '(D) Total Personnel', 'log(Total Personnel)', conus_mask)
p5 <- make_map(figure_2_df, 'total_threatened', '(F) Homes Threatened', 'log(Homes Threatened)', conus_mask)
p6 <- make_map(figure_2_df, 'structures_destroyed', '(E) Homes Destroyed', 'log(Homes Destroyed)', conus_mask)

# grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path(draft_figs_dir, "Figure_2.pdf"), g, width = 4, height = 6, 
       dpi = 1200, scale = 4, units = "cm") #saves g
