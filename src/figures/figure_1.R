
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

plot_function <- function(data, x, mask) {
  ggplot() +
    geom_sf(data = data, aes_string(fill = log(x)), color = NA, lwd = 0) +
    scale_fill_distiller(palette = 'BrBG', na.value = NA) +
    geom_sf(data = mask, color = "black", lwd=0.1, fill=NA) +
    coord_sf(crs = st_crs(mask), datum = NA) + 
    ggtitle(x) +
    theme(
      panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
      panel.grid = element_blank(), 
      line = element_blank(), 
      rect = element_blank(), 
      plot.background = element_blank())
  }


figure_1_df <- conus_209 %>%
  group_by(hexid50k) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  left_join(hexnet_50k, ., by = 'hexid50k') %>%
  na.omit(n)

p1 <- plot_function(dat = figure_1_df, x = 'log(n)', mask = states)
ggsave(file = file.path('results', 'var_hexagonal', "log_number_events.pdf"), p1, width = 8, height = 5, 
       dpi = 600, scale = 3, units = "cm") #saves g

figure_1_ts <- conus_209 %>%
  mutate(year = as.Date(conus_209$DISCOVERY_DATE),
         year = year(year)) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

p2 <- figure_1_ts %>%
  ggplot(aes(x = year, y = (n))) +
  geom_smooth(method="glm", method.args = list(family = "poisson")) +
  geom_point() +
  geom_line() +
  xlab('') + ylab('Average fire rate of spread (pixels per day)') +
  theme_pub()

ggsave(file = file.path('results', 'var_timeseries', "log_number_events.pdf"), p2, width = 8, height = 5, 
       dpi = 600, scale = 2, units = "cm") #saves g
