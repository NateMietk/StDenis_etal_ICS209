rim_fire <- fread(file.path(ics209_input_dir, 'ics209-plus-wf_sitreps_1999to2014.csv')) %>%
  filter(INCIDENT_ID == '2013_CA-STF-002857_RIM')

rim_fire_df <- as.data.frame(rim_fire) %>%
  mutate(date = ymd(as_date(REPORT_TO_DATE))) %>% 
  as_tibble() %>%
  group_by(date) %>%
  summarise(costs = max(PROJECTED_FINAL_IM_COST),
            fsr = max(WF_FSR, na.rm = TRUE),
            structures_destroyed = max(STR_DESTROYED),
            total_personnel = max(TOTAL_PERSONNEL),
            burned_area_acres = max(ACRES),
            total_threatened = max(STR_THREATENED)) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  droplevels()

p1 <- rim_fire_df %>%
  ggplot(aes(x = date, y = fsr)) +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Max Fire Spread Rate (acres/day)') + xlab('') +
  ggtitle("(B) Max Fire Spread Rate (acres/day)") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p2 <- rim_fire_df %>%
  ggplot(aes(x = date, y = burned_area_acres)) +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Burned area (acres)') + xlab('') +
  ggtitle("(C) Burned area (acres)") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p3 <- rim_fire_df %>%
  ggplot(aes(x = date, y = costs)) +
  ylab('Costs') + xlab('') +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Costs ($)') + xlab('') +
  ggtitle("(D) Costs ($)") +
  theme_pub() +
  theme(axis.text.x=element_blank()) 

p4 <- rim_fire_df %>%
  ggplot(aes(x = date, y = total_personnel)) +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Total Personnel') + xlab('') +
  ggtitle("(E) Total Personnel") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p5 <- rim_fire_df %>%
  ggplot(aes(x = date, y = total_threatened)) +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Total Threatened') + xlab('Report Days') +
  ggtitle("(F) Structures Threatened") +
  theme_pub()

p6 <- rim_fire_df %>%
  ggplot(aes(x = date, y = structures_destroyed)) +
  geom_line(color = 'gray') +
  geom_point() +
  ylab('Total Destroyed') + xlab('Report Days') +
  ggtitle("(G) Structures Destroyed") +
  theme_pub()

# Create the spatial 
rim_fire_pt <- rim_fire %>%
  mutate(POO_LONGITUDE = ifelse(is.na(POO_LONGITUDE),0,as.numeric(POO_LONGITUDE)),
         POO_LATITUDE = ifelse(is.na(POO_LATITUDE),0,as.numeric(POO_LATITUDE))) %>%
  st_as_sf(., coords = c("POO_LONGITUDE", "POO_LATITUDE"),
                       crs = "+init=epsg:4326") %>%
  st_transform(crs = st_crs(states))

rim_fire_mtbs <- mtbs %>%
  filter(fire_id == 'CA3785712008620130817')

modis_burn_dates <- raster::raster(file.path(fire_dir, 'USA_BurnDate_2013.tif')) %>%
  crop(rim_fire_mtbs) %>%
  mask(rim_fire_mtbs)

# A function to plot our raster data
rim_fire_map <- rasterVis::levelplot(modis_burn_dates,
                     par.settings = list(layout.heights=list(xlab.key.padding=1),
                                         axis.line = list(col = "transparent"),
                                         strip.background = list(col = 'transparent'),
                                         strip.border = list(col = 'transparent')),
                     margin=FALSE, region = TRUE,
                     xlab = list(label = 'Day of Year', cex = 1.5), 
                     scales = list(draw = FALSE),
                     col.regions = colorRampPalette(brewer.pal(10, 'RdYlBu')),
                     colorkey = list(col = colorRampPalette(brewer.pal(10, 'RdYlBu')),
                                     space="bottom")) +
  latticeExtra::layer(sp.polygons(as(rim_fire_mtbs, 'Spatial'), lwd = 2)) +
  latticeExtra::layer(sp.points(as(rim_fire_pt, 'Spatial'), pch = 16, size = 8, col = 'black'))

# arrangeGrob(rim_fire_map, arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2), nrow = 1)
g <- arrangeGrob(rim_fire_map, arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2), nrow = 1)

ggsave(file = file.path(draft_figs_dir, "Figure_3.pdf"), g, width = 8, height = 5, 
       dpi = 1200, scale = 5, units = "cm") #saves g
