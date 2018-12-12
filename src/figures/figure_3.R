figure_2_ts <- conus_209 %>%
  mutate(year = as.Date(conus_209$DISCOVERY_DATE),
         year = year(year)) %>%
  group_by(year) %>%
  summarise(n = n(),
            fsr = log(max(WF_MAX_FSR, na.rm = TRUE)),
            structures_destroyed = log(sum(STR_DESTROYED_TOTAL, na.rm = TRUE)),
            total_personnel = log(sum(TOTAL_PERSONNEL_SUM, na.rm = TRUE)),
            burned_area_acres = log(sum(FINAL_ACRES, na.rm = TRUE)),
            costs = log(sum(PROJECTED_FINAL_IM_COST, na.rm = TRUE)),
            total_threatened = log(sum(STR_THREATENED_MAX, na.rm = TRUE))) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

make_ts <- function(df, var_x = 'year', var_y, lab_title, plot_title) {
  p1 <- ggplot(data = df, aes_string(x =  var_x, y = var_y)) +
    geom_smooth(method="glm", method.args = list(family = "poisson"), se = FALSE) +
    geom_point() +
    geom_line() +
    xlab('') + ylab(lab_title) +
    ggtitle(plot_title) +
    theme_pub()
  return(p1)
}

p1 <- make_ts(df = figure_2_ts, var_y = 'fsr', lab_title = 'log(FSR)', plot_title = '(A) Fire Spread Rate (acres/day)')
p2 <- make_ts(df = figure_2_ts, var_y = 'burned_area_acres', lab_title = 'log(Burned Area)', plot_title = '(B) Burned Area (acres)')
p3 <- make_ts(df = figure_2_ts, var_y = 'costs', lab_title = 'log(Costs)', plot_title = '(C) Costs ($)')
p4 <- make_ts(df = figure_2_ts, var_y = 'total_personnel', lab_title = 'log(Total Personnel)', plot_title = '(D) Total Personnel')
p5 <- make_ts(df = figure_2_ts, var_y = 'total_threatened', lab_title = 'log(Total Threatened)', plot_title = '(E) Structures Threatened')
p6 <- make_ts(df = figure_2_ts, var_y = 'structures_destroyed', lab_title = 'log(Structures Destroyed)', plot_title = '(F) Structures Destroyed')

g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave(file = file.path('results', 'draft_figures', "Figure_3.jpg"), g, width = 5, height = 7, 
       dpi = 1200, scale = 4, units = "cm") #saves g


g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)

ggsave(file = file.path('results', 'draft_figures', "Figure_3_horizontal.jpg"), g, width = 7, height = 5, 
       dpi = 1200, scale = 4, units = "cm") #saves g