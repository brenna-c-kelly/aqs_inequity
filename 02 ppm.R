
library(sf)
library(stars)
library(raster)
library(spatstat)

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

# boundary
load("/Users/brenna/Documents/School/GEOG 6960/states_shp_win.RData") # owin

# point data
monitors_sf <- st_read("/Users/brenna/Downloads/monitors_sf_clean.shp")
monitors_sf <- st_as_sf(monitors_sf, coords = c("long", "lat"), 
                        crs = 4326, agr = "constant")
monitors_sf <- st_transform(monitors_sf, st_crs(aea))
# convert to ppp
mon_crds <- st_coordinates(monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win) #us.proj.win)
monitors.ppp <- rescale(monitors.ppp), 1000, "km")

# rasterized covariates
aian <- as.im(read_stars("data/tifs/aian_lc.tif"))
asian <- as.im(read_stars("data/tifs/asian_lc.tif"))
black <- as.im(read_stars("data/tifs/black_lc.tif"))
hisp <- as.im(read_stars("data/tifs/hispanic_lc.tif"))
nhpi <- as.im(read_stars("data/tifs/nhpi_lc.tif"))
other <- as.im(read_stars("data/tifs/other_lc.tif"))
tom <- as.im(read_stars("data/tifs/tom_lc.tif"))
metro <- as.im(read_stars("data/tifs/metro.tif")) ##
urban <- as.im(read_stars("data/urban.tif"))
pov <- as.im(read_stars("data/tifs/pov_lc.tif"))
total <- as.im(read_stars("data/tifs/total_lc.tif"))


plot(metro)
plot(monitors.ppp, add = TRUE)
fit <- ppm(monitors.ppp ~ asian)

# ppm
fit1 <- ppm(monitors.ppp ~ (aian + asian + black + hisp +
                              nhpi + other + tom)*urban*pov + offset(total))
AIC(fit1) 

summary(fit1)
# race*urban*pov + off(total):      755485.2
# race*metro + pov + off(total):    757850.7
# race + metro + pov + off(total):  758421.8
# race*pov + metro + off(total):    756267.7

## this sucks — but note that in the results, "urban" means rural (i.e., urban variable = 0)

res <- summary(fit1)$coefs.SE.CI
names(res) <- tolower(names(res))

res$variable <- row.names(res)
head(res)

res <- tibble::rownames_to_column(res, "row_names")

res_split <- split(res, list(nchar(res$row_names) < 12,  nchar(res$row_names) > 5))

# exponentiate only
res_split$TRUE.FALSE <- res_split$TRUE.FALSE %>% 
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) exp(x)))




res_exp <- res %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 3)))
names(res_exp) <- paste(names(res_exp), "exp", sep = "_")



results <- cbind(res, res_exp[, 2:5])


res$row_names == 

intx_list = vector("list", length = 17)
for(i in c(11:17)) {
  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  intx_name_1 <- substr(fixed_z[i, "coef"], 1, 7)
  intx_name_2 <- substr(fixed_z[i, "coef"], 9, 15)
  #print(intx_name_2)
  intx <- fixed_z[i, "mean"]
  intx_name <- ifelse(intx_name_1 == "x_pov_z", intx_name_2, intx_name_1)
  fx_1 <- fixed_z[which(fixed_z$coef == intx_name), "mean"]
  fx_2 <- fixed_z[which(fixed_z$coef == "x_pov_z"), "mean"]
  
  prop_fx <- exp(int + intx + fx_1 + fx_2) / (1 + exp(int + intx + fx_1 + fx_2))
  ci_025 <- exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.025quant"] +
                  fixed_z[i, "X0.025quant"] +
                  fixed_z[which(fixed_z$coef == intx_name), "X0.025quant"] +
                  fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.025quant"]) /
    (1 + exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.025quant"] +
               fixed_z[i, "X0.025quant"] +
               fixed_z[which(fixed_z$coef == intx_name), "X0.025quant"] +
               fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.025quant"]))
  ci_975 <- exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.975quant"] +
                  fixed_z[i, "X0.975quant"] +
                  fixed_z[which(fixed_z$coef == intx_name), "X0.975quant"] +
                  fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.975quant"]) /
    (1 + exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.975quant"] +
               fixed_z[i, "X0.975quant"] +
               fixed_z[which(fixed_z$coef == intx_name), "X0.975quant"] +
               fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.975quant"]))
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(sig == "sig" & fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = fixed_z[i, "coef"], 
                   prob = prop_fx,
                   ci_025 = ci_025,
                   ci_975 = ci_975,
                   sig = sig,
                   dir = dir)
  intx_list[[i]] <- df
}
intx_list = do.call(rbind, intx_list)
intx_list
