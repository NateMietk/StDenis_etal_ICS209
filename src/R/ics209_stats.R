
library(agricolae)
library(vegan)

wui_209 <- wui_209 %>%
  mutate(ign_wui <- paste0(Class, "_", cause))

krus_df <- fam_clean %>%
  ungroup() %>%
  select(edoy, costs, fatalities, home.destroyed, home.threat, 
         max.pers, max.aerial, tot.pers, tot.aerial, max.agency.support, cause)

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
    krus[[i]] = plyr::join(means, groups, by = "trt")
    names(krus[[i]])[1] = "means"
    krus[[i]]$variable = as.factor(colnames(x[i]))
  }
  tuks_df = as.data.frame(do.call("rbind", krus))
  tuks_df$means = round(tuks_df$means, 2)
  tuks_df$trt = as.factor(tuks_df$trt)
  tuks_df$trt = factor(tuks_df$trt, levels(tuks_df$trt)[c(3,4,1,2)])
  tuks_df$mean_std = paste0(tuks_df$means, " (", round(tuks_df$std,2), ")")
  return(tuks_df)
}

cause_df <- mass_krus(krus_df[,!names(krus_df) %in% "cause"], krus_df$cause)

plotit = function(kdf, title){
  ##############
  # ggplotting #
  ##############
  
  ggplot(na.omit(only_sigs), aes(x=trt, y = variable)) +
    geom_tile(aes(fill = groups), color = "grey10") +
    theme_bw() + 
    scale_fill_grey(start = 1, end = 0.5) +
    geom_text(aes(label=mean_std)) +
    ggtitle(paste(title))
}

plotit(cause_df, "2013")
