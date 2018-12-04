make_map <- function(df, x, title, leg_title, mask) {
  p1 <- ggplot() +
    geom_sf(data = mask, color = "black", lwd=0.1, fill='gray95') +
    geom_sf(data = df, aes_string(fill = x), color = NA, lwd = 0) +
    scale_fill_distiller(leg_title, palette = 'BrBG', na.value = NA) +
    coord_sf(crs = st_crs(mask), datum = NA) + 
    ggtitle(title) +
    theme(
      panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
      panel.grid = element_blank(), 
      line = element_blank(), 
      rect = element_blank(), 
      plot.background = element_blank(),
      legend.position="bottom",
      legend.box="horizontal",
      legend.key.height = unit(0.2, "cm")) +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5))
  return(p1)
}
