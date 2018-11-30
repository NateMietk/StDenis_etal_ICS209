make_map <- function(df, x, title, leg_title, mask) {
  p1 <- ggplot() +
    geom_sf(data =df, aes_string(fill = x), color = NA, lwd = 0) +
    scale_fill_distiller(leg_title, palette = 'BrBG', na.value = NA) +
    geom_sf(data = mask, color = "black", lwd=0.1, fill=NA) +
    coord_sf(crs = st_crs(mask), datum = NA) + 
    ggtitle(title) +
    theme(
      panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
      panel.grid = element_blank(), 
      line = element_blank(), 
      rect = element_blank(), 
      plot.background = element_blank())
  return(p1)
}
