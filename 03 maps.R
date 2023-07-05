

#maps


tm_shape(poverty) +
  tm_polygons(col = "pov_perc", lwd = 0, palette = "plasma")


tm_shape(bg_vars_ruca) +
  tm_polygons(col = "rural_urban", lwd = 0, palette = "plasma")


plot(monitors.ppp_pm)
plot(density(monitors.ppp_pm, sigma=50))

plot(monitors.ppp_pb)
