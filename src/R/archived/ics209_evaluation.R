
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
  geom_point(aes(y = log(max.aerial), color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.aerial), color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("")  + ylab("log(Max aerial)") +
  theme_pub() +facet_wrap(~Class)  

p2 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = log(max.pers), color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.pers), color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(Max personnel)") +
  theme_pub() +facet_wrap(~Class)  

p3 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = log(max.agency.support), color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.agency.support), color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(Max agency support)") +
  theme_pub() +facet_wrap(~Class)  

p4 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = log(home.destroyed)+1, color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(home.destroyed)+1, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(homes destroyed) + 1") +
  theme_pub() +facet_wrap(~Class)  

p5 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = (costs))) +
  geom_point(aes(y = log(home.threat)+1, color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(home.threat)+1, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("log(suppression costs)") + ylab("log(homes threatened) + 1") +
  theme_pub() +facet_wrap(~Class)  

grid.arrange(p1, p2, p3, p4, p5, nrow =5)





t1 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = log(area_km2))) +
  geom_point(aes(y = log(max.aerial), color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.aerial), color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("")  + ylab("log(Max aerial)") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t2 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = log(area_km2))) +
  geom_point(aes(y = log(max.pers), color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.pers), color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(Max personnel)") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t3 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = log(area_km2))) +
  geom_point(aes(y = log(max.agency.support)+1, color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(max.agency.support)+1, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(Max agency support)") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t4 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = log(area_km2))) +
  geom_point(aes(y = log(home.destroyed)+1, color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(home.destroyed)+1, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("") + ylab("log(homes destroyed) + 1") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

t5 <- wui209_shp %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  #filter(cause == "Human") %>%
  filter(cause != "Unk") %>%
  ggplot(aes(x = log(area_km2))) +
  geom_point(aes(y = log(home.threat)+1, color = cause), size = 0.75, alpha = 0.25, shape = 16) +
  geom_smooth(aes(y = log(home.threat)+1, color = cause), method="glm", method.args = list(family = "poisson"),
              se= TRUE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("log(area_km2)") + ylab("log(homes threatened) + 1") +
  theme_pub() +facet_wrap(~Class, scales = "free")  

grid.arrange(t1, t2, t3, t4, t5, nrow =5)



library(mblm)
fit1 <- mblm(costs ~ max.pers, data = wui209_shp)

library(agricolae)
t <- as.data.frame(wui209_shp)
for_ts <- t %>%
  filter(cause != "Unk") %>%
  mutate(combo = paste(cause, Class, sep = "_")) %>%
  select(combo, area_km2, fatalities, home.destroyed, home.threat, max.aerial, tot.aerial,
         max.pers, tot.pers, max.agency.support) %>%
  gather(variable, value, -combo, -area_km2) %>%
  split(.$variable) %>%
  map(~  mblm(area_km2 ~ value + combo, data = .x))
  # map(~ aov(area_km2 ~ combo + value, data = .x)) %>%
  # map(HSD.test, trt = 'combo', alpha = 0.05)
  
aov.models <- wui209_shp %>%
  select(-Year, -Age) %>%
  gather(variable, value, -sb_age, -Site, -Plot) %>%
  split(.$variable) %>%
  map(~  mblm(variable ~ max.pers + varaible, data = .x))
  #map(~ aov(value ~ sb_age + Site/Plot, data = .x)) %>%
  #map(HSD.test, trt = 'sb_age', alpha = 0.05)


plot(fit1)
residuals(fit1)
fitted(fit1)
plot(density(fit1$slopes))
plot(density(fit1$intercepts))
anova(fit1)



anova(fit2)
anova(fit,fit2)
confint(fit)
AIC(fit,fit2)





























t <- wui209_shp %>%
  filter(incidentnum == "CA-KNF-3393") %>%
  filter(cause != "Unk") %>%
  group_by(Class, syear, cause) %>%
  summarise(costs = sum(as.numeric(costs)),
            area_km2 = sum(as.numeric(area_km2)))

ics_sums_p <- t %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
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
        legend.position = "none") +
   facet_wrap(~Class, scales = "free")  

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

