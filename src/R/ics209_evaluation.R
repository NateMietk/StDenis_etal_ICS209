
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(gridExtra)

source("src/R/ggplot_theme.R")

wui209_shp <- st_read(dsn = file.path("data", "ics209", "gpkg", "ics209_wui_conus.gpkg"),
                   layer = "ics209_wui_conus", quiet= TRUE) %>%
  st_cast("POINT") %>%
  filter(cause != "Unk") 

ggplot(t) +
  geom_sf(aes(fill = cause, colour = cause), size = 0.1)

t <- wui209_shp %>%
  #filter(cause == "Human") %>%
  group_by(syear) %>%
  summarise(threatened_res = sum(threatened_res)) 
sum(t$threatened_res)

ics_sums_t <- t %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = threatened_res), size = 2) +
  geom_line(aes(y = threatened_res),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = threatened_res), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

t <- wui209_shp %>%
  group_by(syear) %>%
  summarise(costs = sum(costs),
            area_km2 = sum(area_km2))

ics_sums_p <- t %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = costs/100000000), size = 2) +
  geom_line(aes(y = costs/100000000),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = costs/100000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

ics_sums_s <- t %>%
  #transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = area_km2/10000), size = 2) +
  geom_line(aes(y = area_km2/10000),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = area_km2/10000), method="glm", method.args = list(family = "poisson"),
              se = FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Burned Area \n(in tens of thousands; km2)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") 

grid.arrange(ics_sums_p, ics_sums_s, ncol =2)

ics <- wui209_shp %>%
  ggplot(aes(x = (costs))) +
  geom_smooth(aes(y = (max_agencies), color = cause), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Fire Size") + ylab("Total Personal") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) +
  facet_wrap(~syear, scales = "free")



