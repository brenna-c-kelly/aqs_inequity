

bg_vars_ruca$code <- as.factor(bg_vars_ruca$code)
bg_vars_ruca$code <- relevel(as.factor(bg_vars_ruca$code), ref = "metro")

ftable(bg_vars_ruca$code)

bg_vars_ruca$metro[bg_vars_ruca$code == "metro"] <- "metro"
bg_vars_ruca$metro[bg_vars_ruca$code != "metro"] <- "not metro"
bg_vars_ruca$metro <- as.factor(bg_vars_ruca$metro)
bg_vars_ruca$metro <- relevel(as.factor(bg_vars_ruca$metro), ref = "metro")

ext <- extent(bg_vars_ruca)
r <- raster(ext, res=4000)
metro <- fasterize(bg_vars_ruca, r, field = "metro")
metro_im <- as.im(metro)
metro_im <-  rescale(metro_im, 1000, "km")

bg_vars_ruca$rural_urban <- case_when(bg_vars_ruca$rural_urban == "urban" ~ 0,
                                      bg_vars_ruca$rural_urban == "rural" ~ 1)



ext <- extent(bg_vars_ruca)
r <- raster(ext, res = 4000)
urban <- fasterize(bg_vars_ruca, r, field = "rural_urban")
writeRaster(urban, filename = "data/rural_urban.tif")
urban <- as.im(read_stars("data/urban.tif"))

#urban <- rasterize(bg_vars_ruca, r, field = "rural_urban")
#urban <- as.im(urban)
#metro_im <-  rescale(metro_im, 1000, "km")