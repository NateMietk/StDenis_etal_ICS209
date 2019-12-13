ak_mask <- states %>%
  dplyr::filter(stusps == 'AK') 

figure_S1_df <- conus_209 %>%
  dplyr::filter(stusps == 'AK') %>%
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

p1 <- make_map(figure_S1_df, 'fsr', '(a) Fire spread rate', 'log(Max FSR (acres/day))', ak_mask) 
p2 <- make_map(figure_S1_df, 'burned_area_acres', '(b) Burned Area', 'log(Burned Area (acres))', ak_mask) 
p3 <- make_map(figure_S1_df, 'costs', '(d) Costs', 'log(Costs ($))', ak_mask)
p4 <- make_map(figure_S1_df, 'total_personnel', '(c) Total Personnel', 'log(Total Personnel)', ak_mask)
p5 <- make_map(figure_S1_df, 'total_threatened', '(f) Homes Threatened', 'log(Homes Threatened)', ak_mask)
p6 <- make_map(figure_S1_df, 'structures_destroyed', '(e) Homes Destroyed', 'log(Homes Destroyed)', ak_mask)

g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path(draft_figs_dir, "Figure_S1.pdf"), g, width = 3, height = 5, 
       dpi = 1200, scale = 4, units = "cm") #saves g
