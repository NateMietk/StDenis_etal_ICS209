county_slopes_spatial <- left_join(counties, county_slopes, by = "county.ns")

types <- vapply(sf::st_geometry(counties), function(x) {
  class(x)[2]
}, "")
polys <- counties[ grepl("*POLYGON", types), ]
cnty <- as(polys, "Spatial")
cnty$id <- row.names(cnty)
cnty_df <- fortify(cnty, region = 'id')
cnty_df <- left_join(cnty_df, cnty@data, by = 'id')
names(cnty_df) <- tolower(names(cnty_df))

cnty_slp <- as(county_slopes_spatial, "Spatial")
cnty_slp$id <- row.names(cnty_slp)
cnty_slp_df <- fortify(cnty_slp, region = 'id')
cnty_slp_df <- left_join(cnty_slp_df, cnty_slp@data, by = 'id')
names(cnty_slp_df) <- tolower(names(cnty_slp_df))

ics_counties <- st_join(counties, wui_209, join = st_intersects) %>%
  group_by(county.ns) %>%
  summarise(fcnt = n(),
            sum_costs = sum(costs),
            sum_structures_threat = sum(home.threat) + sum(comm.threat),
            sum_structures_destroyed = sum(home.destroyed) + sum(comm.destroyed),
            median_costs = median(costs),
            median_structures_threat = median(sum_structures_threat),
            median_structures_destroyed = median(sum_structures_destroyed))

geom_sf(color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(10,"RdYlBu"))) +
  scale_size_discrete(range = c(.2, 0.9), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))

