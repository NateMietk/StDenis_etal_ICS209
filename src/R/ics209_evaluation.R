
library(data.table)
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(gridExtra)
library(ggmap)

source("src/R/ggplot_theme.R")

wui209_shp <- st_read(dsn = file.path("data", "spatial", "ics209_wui_conus.gpkg"),
                   layer = "ics209_wui_conus", quiet= TRUE) %>%
  st_cast("POINT") %>%
  st_transform("+init=epsg:2163")  # e.g. US National Atlas Equal Area)
  #filter(cause != "Unk") 

wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() + coord_equal() + 
  theme_nothing(legend = TRUE) + theme_pub() +
  geom_sf(aes(fill = cause, colour = cause), size = 0.5, shape=".") +
  facet_wrap(Class ~ cause)

wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot() + coord_equal() + 
  theme_nothing(legend = TRUE) + theme_pub() +
  geom_sf(aes(fill = cause, colour = cause), size = 0.5, shape=".") +
  facet_wrap(~ cause)


p1 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = max.aerial, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.aerial, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("")  + ylab("Max aerial") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

p2 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = max.pers, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.pers, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("Max personnel") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

p3 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = max.agency.support, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.agency.support, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("log suppression costs") + ylab("Max agency support") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

grid.arrange(p1, p2, p3, nrow =3)



t1 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = area_km2)) +
  geom_point(aes(y = max.aerial, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.aerial, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("")  + ylab("Max aerial") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t2 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = area_km2)) +
  geom_point(aes(y = max.pers, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.pers, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("Max personnel") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t3 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = area_km2)) +
  geom_point(aes(y = max.agency.support, color = cause), size = 0.75) +
  geom_smooth(aes(y = max.agency.support, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Burned area (km2)") + ylab("Max agency support") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

grid.arrange(t1, t2, t3, nrow =3)


































t <- wui209_shp %>%
  filter(cause != "Unk") %>%
  group_by(syear, cause) %>%
  summarise(costs = sum(as.numeric(costs)),
            area_km2 = sum(as.numeric(area_km2)))

ics_sums_p <- t %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = costs/100000000, color = cause), size = 2) +
  geom_line(aes(y = costs/100000000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = costs/100000000, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

ics_sums_s <- t %>%
  #transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = area_km2/10000, color = cause), size = 2) +
  geom_line(aes(y = area_km2/10000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = area_km2/10000, color = cause), method="glm", method.args = list(family = "poisson"),
              se = TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Burned Area \n(in tens of thousands; km2)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) 

grid.arrange(ics_sums_p, ics_sums_s, ncol =2)

