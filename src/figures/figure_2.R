
figure_2_df <- conus_209 %>%
  group_by(hexid50k) %>%
  summarise(n = n(),
            structures_destroyed = sum(DESTROYED_RES_TOTAL, na.rm = TRUE),
            total_personnel = sum(TOTAL_PERSONNEL, na.rm = TRUE),
            burned_area_acres = sum(ACRES, na.rm = TRUE),
            costs = sum(EST_IM_COST_TO_DATE, na.rm = TRUE),
            total_threatened = sum(THREATENED_TOTAL, na.rm = TRUE)) %>%
  as.data.frame() %>%
  dplyr::select(-geom) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

p1 <- ggplot() +
  geom_sf(data = figure_2_df, aes(fill = log(costs)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Costs)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = states, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', 'var_hexagonal', "log_costs.pdf"), p1, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g

p2 <- ggplot() +
  geom_sf(data = figure_2_df, aes(fill = log(structures_destroyed)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Structures Destroyed)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = states, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', 'var_hexagonal', "log_structures_destroyed.pdf"), p2, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g

p3 <- ggplot() +
  geom_sf(data = figure_2_df, aes(fill = log(total_personnel)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Total Personnel)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = states, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', 'var_hexagonal', "log_total_personnel.pdf"), p3, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g

p4 <- ggplot() +
  geom_sf(data = figure_2_df, aes(fill = log(burned_area_acres)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Burned Area); acres', palette = 'BrBG', na.value = NA) +
  geom_sf(data = states, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', 'var_hexagonal', "log_burned_area_acres.pdf"), p4, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g


p5 <- ggplot() +
  geom_sf(data = figure_2_df, aes(fill = log(total_threatened)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Total Threatened)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = states, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', 'var_hexagonal', "log_total_threatened.pdf"), p5, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g