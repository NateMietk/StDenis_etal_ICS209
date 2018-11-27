
ics_df <- as.data.frame(conus_209) %>%
   mutate(structures_destroyed = DESTROYED_RES_TOTAL, 
            total_personnel = TOTAL_PERSONNEL,
            burned_area_acres = ACRES, 
            costs = EST_IM_COST_TO_DATE, 
            total_threatened = THREATENED_TOTAL) %>%
  dplyr::select(structures_destroyed, total_personnel, burned_area_acres, costs, total_threatened, START_YEAR) %>%
  mutate(cyear = c(scale(START_YEAR))) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  dplyr::select(-START_YEAR)

compose_formula <- function(x, y) {
  as.formula(paste0("log(", y, ") ~ log(", x, ") + (1|cyear)"))
}

formula <- tibble(response=response_var,
               predictor=setdiff(names(ics_df), response_var)) %>%
  filter(predictor != 'cyear') %>%
  mutate(formula = paste0("(", response, ") ~ (", predictor, ") + (1|cyear)")) %>%
  dplyr::pull(., formula)


library(lme4)
output <-
  tibble(formula) %>% 
  mutate(model = map(formula, ~lmer(formula = .x, data = ics_df)),
         glance = map(model, glance),
         augment = map(model, augment))
glanced <- output$glance
results <- output %>% unnest(glance)
predicted <- output %>% unnest(augment)

scatter_fun = function(x, y) {
  ggplot(predicted_slim, aes_string(x = x, y = y) ) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "grey74") +
    theme_bw()
}

predicted_slim <- predicted %>%
  dplyr::select(.fitted, structures_destroyed, burned_area_acres, total_personnel, total_threatened) 

elev_plots = map(predicted_slim, ~scatter_fun(.x, ".fitted") )

cowplot::plot_grid(plotlist = elev_plots,
                   labels = "AUTO",
                   align = "hv")

ics_df %>%
  mutate(zero_no_zero = ifelse(structures_destroyed == 0, 'NO', 'YES')) %>%
  group_by(zero_no_zero) %>%
  summarise(sumc = sum(costs, na.rm = TRUE)) %>%
  ggplot(aes(y= sumc, x = zero_no_zero)) +
  geom_bar(stat='identity')
