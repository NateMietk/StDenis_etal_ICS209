rim_fire <- fread(file.path(ics_inputs, 'ics209_allsitreps1999to2014.csv')) %>%
  filter(INCIDENT_ID == '2013_CA-STF-002857_RIM')

rim_fire_df <- rim_fire %>%
  mutate(date = ymd(as_date(REPORT_TO_DATE))) %>% 
  as_tibble() %>%
  group_by(date) %>%
  summarise(costs = max(EST_IM_COST_TO_DATE),
            structures_destroyed = max(STR_DESTROYED),
            total_personnel = max(TOTAL_PERSONNEL),
            burned_area_acres = max(ACRES),
            total_threatened = max(STR_THREATENED)) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

p1 <- rim_fire_df %>%
  ggplot(aes(x = date, y = 0)) +
  geom_point() +
  ggtitle("PLACEHOLDER") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p2 <- rim_fire_df %>%
  ggplot(aes(x = date, y = burned_area_acres)) +
  geom_point() +
  ylab('Burned area (acres)') + xlab('') +
  ggtitle("(B)") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p3 <- rim_fire_df %>%
  ggplot(aes(x = date, y = total_personnel)) +
  geom_point() +
  ylab('Total Personnel') + xlab('') +
  ggtitle("(C)") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p4 <- rim_fire_df %>%
  ggplot(aes(x = date, y = costs)) +
  ylab('Costs') + xlab('') +
  geom_point() +
  ylab('Costs ($)') + xlab('') +
  ggtitle("(D)") +
  theme_pub() +
  theme(axis.text.x=element_blank())

p5 <- rim_fire_df %>%
  ggplot(aes(x = date, y = total_threatened)) +
  geom_point() +
  ylab('Total Threatened') + xlab('Report Days') +
  ggtitle("(E)") +
  theme_pub()

p6 <- rim_fire_df %>%
  ggplot(aes(x = date, y = structures_destroyed)) +
  geom_point() +
  ylab('Total Destroyed') + xlab('Report Days') +
  ggtitle("(F)") +
  theme_pub()


grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path('results', 'draft_figures', "Figure_3.pdf"), g, width = 5, height = 6, 
       dpi = 600, scale = 4, units = "cm") #saves g
