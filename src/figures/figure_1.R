
figure_1_df_conus <- conus_209 %>%
  filter(stusps != 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  dplyr::select(-contains('geom')) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)
conus <- states %>%
  filter(stusps != 'AK')

p1 <- ggplot() +
  geom_sf(data = conus, color = "black", lwd=0.1, fill='gray95') +
  geom_sf(data = figure_1_df_conus, aes(fill = log(n)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Fire Counts)', palette = 'BrBG', na.value = NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank(),
    legend.position="bottom",
    legend.box="horizontal",
    legend.key.height = unit(0.2, "cm")) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))

ggsave(file = file.path(draft_figs_dir, "Figure_1_conus.pdf"), p1, width = 6, height = 5, 
       dpi = 1200, scale = 3, units = "cm") #saves g

figure_1_df_ak <- conus_209 %>%
  filter(stusps == 'AK') %>%
  group_by(hexid50k) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  dplyr::select(-contains('geom')) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)
ak <- states %>%
  filter(stusps == 'AK')
p1 <- ggplot() +
  geom_sf(data = ak, color = "black", lwd=0.1, fill='gray95') +
  geom_sf(data = figure_1_df_ak, aes(fill = log(n)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Fire Counts)', palette = 'BrBG', na.value = NA) +
  coord_sf(crs = st_crs(states), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank(),
    legend.position="bottom",
    legend.box="horizontal",
    legend.key.height = unit(0.2, "cm")) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))

ggsave(file = file.path(draft_figs_dir, "Figure_1_ak.pdf"), p1, width = 6, height = 5, 
       dpi = 1200, scale = 3, units = "cm") #saves g
