ics_sums <-   wui_209 %>%
  #mutate(cause = if_else(cause == "Unk", "Human", cause)) %>%
  group_by(syear, cause, class) %>%
  summarise(costs = sum(costs)) %>%
  left_join(.,  fread("data/fire/fpa_wui_year_cause_class.csv"), by = c("syear", "cause", "class"))  %>%
  group_by(syear, cause) %>%
  summarise(costs = sum(costs),
            size = sum(size))

ics_sums_p <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = costs/100000000, color = cause), size = 2) +
  geom_line(aes(y = costs/100000000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = costs/100000000, color = cause), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728", "green")) +
  xlab("Year") + ylab("Fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

ics_sums_s <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = size/1000, color = cause), size = 2) +
  geom_line(aes(y = size/1000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = size/1000, color = cause), method="gam", method.args = list(family = "poisson"),
              se = FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728", "green")) +
  xlab("Year") + ylab("Burned Area \n(in thousands; km2)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

grid.arrange(ics_sums_p, ics_sums_s, ncol =1)
g <- arrangeGrob(ics_sums_p, ics_sums_s, ncol =2)
ggsave(file = "Ignition_Costs_0213_HL.pdf", g, width = 8, height = 4, dpi=1200, scale = 3, units = "cm")

