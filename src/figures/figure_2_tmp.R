ak_mask <- states %>%
  dplyr::filter(stusps == 'AK') 

figure_2_df <- conus_209 %>%
  dplyr::filter(stusps == 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n(),
            structures_destroyed = log(sum(TOTAL_RES_DESTROYED, na.rm = TRUE)),
            total_personnel = log(sum(TOTAL_PERSONNEL, na.rm = TRUE)),
            burned_area_acres = log(sum(ACRES, na.rm = TRUE)),
            costs = log(sum(EST_IM_COST_TO_DATE, na.rm = TRUE)),
            total_threatened = log(sum(TOTAL_RES_THREATENED, na.rm = TRUE))) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

make_map <- function(df, x, title, leg_title, mask) {
  p1 <- ggplot() +
    geom_sf(data =df, aes_string(fill = x), color = NA, lwd = 0) +
    scale_fill_distiller(leg_title, palette = 'BrBG', na.value = NA) +
    geom_sf(data = mask, color = "black", lwd=0.1, fill=NA) +
    coord_sf(crs = st_crs(mask), datum = NA) + 
    ggtitle(title) +
    theme(
      panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
      panel.grid = element_blank(), 
      line = element_blank(), 
      rect = element_blank(), 
      plot.background = element_blank())
  return(p1)
}

p1 <- make_map(figure_2_df, 'n', '(A) log(# Events)', 'log(# Events)', ak_mask)
p2 <- make_map(figure_2_df, 'burned_area_acres', '(B) log(Burned Area)', 'log(Burned Area)', ak_mask) 
p3 <- make_map(figure_2_df, 'total_personnel', '(C) log(Total Personnel)', 'log(Total Personnel)', ak_mask)
p4 <- make_map(figure_2_df, 'costs', '(D) log(Costs)', 'log(Costs)', ak_mask)
p5 <- make_map(figure_2_df, 'structures_destroyed', '(E) log(Homes Destroyed)', 'log(Homes Destroyed)', ak_mask)
p6 <- make_map(figure_2_df, 'total_threatened', '(F) log(Homes Threatened)', 'log(Homes Threatened)', ak_mask)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
