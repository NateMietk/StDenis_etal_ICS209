
library(agricolae)
library(vegan)

mass_krus = function(x, y){
  # This function inputs a data frame, and outputs another data frame
  # of Kruskall-Wallis test results on that data. x is the many response
  # variables, y is one column - the treatment. It runs the K-W test 
  # column by column.
  
  krus = list()
  for(i in 1:(length(x))){
    means = as.data.frame(kruskal(x[,i], y)$means)
    groups = as.data.frame(kruskal(x[,i], y)$groups)
    means$trt = rownames(means)
    groups$trt = rownames(groups)
    krus[[i]] = left_join(means, groups, by = "trt")
    names(krus[[i]])[1] = "means"
    krus[[i]]$variable = as.factor(colnames(x[i]))
  }
  tuks_df = as.data.frame(do.call("rbind", krus))
  tuks_df$means = round(tuks_df$means, 2)
  tuks_df$trt = as.factor(tuks_df$trt)
  tuks_df$mean_std = paste0(tuks_df$means, " (", round(tuks_df$std,2), ")")
  return(tuks_df)
}

cleaned_wuw <- wui_209 %>%
  as.data.frame() %>%
  filter(cause != "Unk") %>%
  mutate(c = ifelse(cause == "Human", "H", "L"),
         cause_wui = paste(c, class, sep = "_")) %>%
  select(-incident_unique_id, -incidentnum, -incidentname, -state.abv, -state, -state_km2, -ecoreg1.code,
         -ecoreg1.name, -ecoreg1_km2, -ecoreg2.code, -ecoreg2.name, -ecoreg2_km2, -ecoreg3.code,
         -ecoreg3.name, -ecoreg3_km2, -blk10, -water10, -sum_area_m, -popden10, -hden10,
         -hhden10, -shden10, -area, -pubflag, -veg06, -nonveg06, -veg06pc, -forest06, -wuiflag10,
         -wuiclass10, -veg75_06, -hu10, -shu10, -hhu10, -pop10, -shape_length, -shape_area, -seasons, -region,
         -eyear, -eday, -emonth, -edoy, -geom, -cause, -class, -confidence, -c, -syear, -smonth, -sday) %>%
  as.tibble()
k13 = mass_krus(cleaned_wuw[,!names(cleaned_wuw) %in% "cause_wui"], cleaned_wuw$cause_wui)


class_ = function(x){
  ifelse(x == "a", 0, 1 )
}

plotit = function(kdf){
  only_sigs = kdf %>%
    mutate(M_id = class_(groups)) %>%
    group_by(variable) %>%
    filter(M_id >= 1 ) 
  
  ##############
  # ggplotting #
  ##############
  
  na.omit(only_sigs) %>%
    transform(trt = factor(trt, levels=c("H_WUI", "H_VLD", "H_Wildlands",
                                         "L_WUI", "L_VLD", "L_Wildlands"))) %>%
    ggplot(aes(x=trt, y = variable)) +
    geom_tile(aes(fill = groups), color = "grey10") +
    theme_minimal() + 
    scale_fill_grey(start = 1, end = 0.5) +
    geom_text(aes(label=mean_std))
}

###################
# using 2013 data #
###################

plotit(k13)


