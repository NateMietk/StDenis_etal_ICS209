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

#p1 <- make_figure_2(figure_2_df, 'n', '(A) log(# Events)', 'log(# Events)', conus_mask)
#placeholder
p1 <- ggplot() +
  geom_sf(data = states, color = "white", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  ggtitle("PLACEHOLDER") +
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())

p2 <- make_map(figure_2_df, 'burned_area_acres', '(B) Burned Area', 'log(Burned Area)', conus_mask) 
p3 <- make_map(figure_2_df, 'total_personnel', '(C) Total Personnel', 'log(Total Personnel)', conus_mask)
p4 <- make_map(figure_2_df, 'costs', '(D) Costs', 'log(Costs)', conus_mask)
p5 <- make_map(figure_2_df, 'total_threatened', '(F) Homes Threatened', 'log(Homes Threatened)', conus_mask)
p6 <- make_map(figure_2_df, 'structures_destroyed', '(E) Homes Destroyed', 'log(Homes Destroyed)', conus_mask)

# grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path('results', 'draft_figures', "Figure_2.pdf"), g, width = 4, height = 6, 
       dpi = 600, scale = 4, units = "cm") #saves g