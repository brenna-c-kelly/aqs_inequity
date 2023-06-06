
library(sf)
library(stars)
library(raster)
library(spatstat)
library(regclass)
library(tidycensus)

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

# boundary
load("data/states_shp_win.RData") # owin
states_shp <- get_decennial(geography = "state",
                            variables = c('P1_001N'),
                            geometry = TRUE,
                            year = 2020,
                            show_call = FALSE)

#states_shp <- st_read("states.shp")
names(states_shp) <- tolower(names(states_shp))
states_shp$statefp <- substr(states_shp$geoid, 1, 2)
states_shp$countyfp <- substr(states_shp$geoid, 3, 5)

noncontiguous <- c("02", "15", "72")
states_shp <- states_shp[!states_shp$statefp %in% noncontiguous, , drop=FALSE]

states_shp <- st_transform(states_shp, st_crs(aea))

#st_write(states_shp, "cont_states.shp")

states_shp.win <- as.owin(states_shp)

length(unique(monitors_sf$lat_long))

monitors_imp <- read.csv("/Users/brenna/Documents/School/GEOG 6960/monitors_imp.csv")
test <- read.csv("/Users/brenna/Documents/School/GEOG 6960/final project/aqs_monitors-2.csv")
length(unique(test$site_id))
head(monitors_imp)

test <- st_as_sf(monitors_imp, coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")
test <- st_transform(test, st_crs(aea))

monitors_sf$lat_long <- paste0(monitors_sf$lat, monitors_sf$long, sep = ", ")

# point data
monitors_sf <- st_read("/Users/brenna/Downloads/monitors_sf_clean.shp")
monitors_sf <- st_as_sf(monitors_sf, coords = c("long", "lat"), 
                        crs = 4326, agr = "constant")
monitors_sf <- st_transform(monitors_sf, st_crs(aea))
# convert to ppp
mon_crds <- st_coordinates(mons$so2)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win, marks = mons$criteria) #us.proj.win)
monitors.ppp_so2 <- rescale(monitors.ppp, 1000, "km")

# rasterized covariates
aian <- as.im(read_stars("data/tifs/aian_lc.tif")) %>%
  rescale(1000, "km")
asian <- as.im(read_stars("data/tifs/asian_lc.tif")) %>%
  rescale(1000, "km")
black <- as.im(read_stars("data/tifs/black_lc.tif")) %>%
  rescale(1000, "km")
hisp <- as.im(read_stars("data/tifs/hispanic_lc.tif")) %>%
  rescale(1000, "km")
nhpi <- as.im(read_stars("data/tifs/nhpi_lc.tif")) %>%
  rescale(1000, "km")
other <- as.im(read_stars("data/tifs/other_lc.tif")) %>%
  rescale(1000, "km")
tom <- as.im(read_stars("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
metro <- as.im(read_stars("data/tifs/metro.tif")) %>%
  rescale(1000, "km") ##
urban <- as.im(read_stars("data/urban.tif")) %>%
  rescale(1000, "km")
pov <- as.im(read_stars("data/tifs/pov_lc.tif")) %>%
  rescale(1000, "km")
total <- as.im(read_stars("data/tifs/total_lc.tif")) %>%
  rescale(1000, "km")


# n <- npoints(monitors.ppp)
# freq <- rpois(n, 3)
# ind <- rep(seq_len(n), freq)
# monitors.pppmonitors.ppp <- monitors.ppp[ind]
# ppm(beibei ~ grad+elev, data=bei.extra)

plot(aian)
plot(monitors.ppp, add = TRUE)

# ppms by criteria pollutant
fit_co <- ppm(monitors.ppp_co ~ (aian + asian + black + hisp +
                             nhpi + other + tom) + pov + urban + offset(total))
fit_no2 <- ppm(monitors.ppp_no2 ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*pov*urban + offset(total))
fit_o3 <- ppm(monitors.ppp_o3 ~ (aian + asian + black + hisp +
                                 nhpi + other + tom)*pov+urban + offset(total))
fit_pb <- ppm(monitors.ppp_pb ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*pov+urban + offset(total))
fit_pm <- ppm(monitors.ppp_pm ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*pov*urban + offset(total))
fit_so2 <- ppm(monitors.ppp_so2 ~ (aian + asian + black + hisp +
                                nhpi + other + tom) + urban + pov + offset(total))

fit_pm$Q
AIC(fit_co)
AIC(fit_no2)
AIC(fit_o3)
AIC(fit_pb)
AIC(fit_pm)
AIC(fit_so2)
#                  all intx / race*pov + metro /  all + / race*urban + pov
# > AIC(fit_co)   4505.745 / 4481.896 / 4504.011* / 4514.082
# > AIC(fit_no2)  3799.177* / 3808.147 / 3803.579 / 3803.927
# > AIC(fit_o3)   14611.7 / 14611.7* / 14666.06 / 14667.86 // 14706.2
# > AIC(fit_pb)   2897.46 / 2886.344* / 2914.953 / 2920.845 // 2919.828
# > AIC(fit_pm)   10101.29* / 10110.24 / 10143.89 / 10130.31
# > AIC(fit_so2)  5934.424 / 5922.01 / 5920.167* / 5925.133 // 5918.231 (race+urban)
summary(fit_co)
summary(fit_no2)
summary(fit_o3)
summary(fit_pb)
summary(fit_pm)
summary(fit_so2)


fit_so2$internal$glmdata

# race*urban*pov + off(total):      63505.4 / 25402.22
# race*metro + pov + off(total):    63653.6 / 25550.43
# race*pov + metro + off(total):    63562.03 / 25458.86
# race + pov*metro + off(total):    63562.03 / 25573.74
# race + metro + pov + off(total):  63695.3 / 25592.12
#Straauss Interaction Effect
# point patterns won't do a 0 distance interaction
# standard form (poisson), cox, log-gaussian cox process
# could cross-generational health outcomes have interactions (proper)
# other approach might be warranted for temporal
  # they're equivalent
  # dispersion nissues would apply to log as well
# access glm within ppm > get glm objects > pass to ggeffects
#   to help me, maybe for paper
#   don't fuss about specfici quantities; is it +/- sig; interactions slope positive
#   for paper, boil doen to direction and significance
# they'll want a clear message at this tier; think about how the abstract will work
#   takeaway
VIF(fit)
fit1 <- fit
# aian    asian    black     hisp     nhpi    other      tom      pov    urban 
# 1.218805 1.554713 1.227305 1.427738 1.218597 1.211360 1.257731 1.112446 1.073823 

data(simba)
print(simba)
# mppm
mon_crds <- st_coordinates(mons)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win, marks = mons$criteria) #us.proj.win)
monitors.ppp <- rescale(monitors.ppp, 1000, "km")

monitors.ppp[]

co.ppp <- subset(monitors.ppp, marks == "co")
no2.ppp <- subset(monitors.ppp, marks == "no2")
o3.ppp <- subset(monitors.ppp, marks == "o3")
pb.ppp <- subset(monitors.ppp, marks == "pb")
pm.ppp <- subset(monitors.ppp, marks == "pm")
so2.ppp <- subset(monitors.ppp, marks == "so2")
# create each ppp without other marks

H <- hyperframe(points = monitors.ppp,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                group = c("co", "no2", "o3", "pb", "pm", "so2"),
                aian = aian, asian = asian, black = black,
                hisp = hisp, nhpi = nhpi, other = other,
                tom = tom, pov = pov, urban = urban,
                total = total)
print(H)
# H <- hyperframe(points = monitors.ppp,
#                 group = c("co", "no2", "o3", "pb", "pm", "so2"),
#                 aian = aian, asian = asian, black = black,
#                 hisp = hisp, nhpi = nhpi, other = other,
#                 tom = tom, pov = pov, urban = urban,
#                 total = total)
                #covs = c(aian, asian, black))
print(H)
str(H)$points
cbind(H[H$group == "co", ])
H_0 <- hyperframe(points = monitors.ppp,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("all"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H_1 <- hyperframe(points = monitors.ppp_co,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                group = c("co"),
                aian = aian, asian = asian, black = black,
                hisp = hisp, nhpi = nhpi, other = other,
                tom = tom, pov = pov, urban = urban,
                total = total)
H_2 <- hyperframe(points = monitors.ppp_no2,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("no2"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H_3 <- hyperframe(points = monitors.ppp_o3,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("o3"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H_4 <- hyperframe(points = monitors.ppp_pb,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("pb"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H_5 <- hyperframe(points = monitors.ppp_pm,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("pm"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H_6 <- hyperframe(points = monitors.ppp_so2,#co.ppp, no2.ppp, o3.ppp, pb.ppp, pm.ppp, so2.ppp,
                  group = c("so2"),
                  aian = aian, asian = asian, black = black,
                  hisp = hisp, nhpi = nhpi, other = other,
                  tom = tom, pov = pov, urban = urban,
                  total = total)
H <- rbind.hyperframe(H_3, H_1, H_2, H_4, H_5, H_6) # trying to make largest group (o3) the referent
H$group <- relevel(factor(H$group), ref = "pm")

fit_H <- mppm(points ~ group + urban + (aian + asian + black + hisp +
                                          nhpi + other + tom) + pov + offset(total),
            data = H)# + offset(total))
summary(fit_H)
fit$Q
AIC(fit)
plot(H)

# group*race + off(total):        49526.85
# group*race*pov + off(total):        49318.38
# marks + race*urban*pov + off(total):    256150
# marks*race*urban*pov + off(total):      256460   
# marks*race*pov + metro + off(total):    256443.5
# marks*race*metro + pov + off(total):    257128.5
# marks*race + metro + pov + off(total):  257133
# marks + race + metro + pov + off(total):257063
# f(marks) + race+metro+pov + off(total): 2028167
# f(marks) + race*metro*pov + off(total): .
fit$Y
VIF(fit)
fit1 <- fit
# aian    asian    black     hisp     nhpi    other      tom      pov    urban 
# 1.218805 1.554713 1.227305 1.427738 1.218597 1.211360 1.257731 1.112446 1.073823 

# slrm
fit_log <- slrm(monitors.ppp ~ (aian + asian + black + hisp +
                                  nhpi + other + tom)*pov*urban + offset(total))
summary(fit_log)

# ppm - old, wrong point data
# race*urban*pov + off(total):      755485.2
# race*metro + pov + off(total):    757850.7
# race + metro + pov + off(total):  758421.8
# race*pov + metro + off(total):    756267.7

## this sucks — but note that in the results, "urban" means rural (i.e., urban variable = 0)

res <- summary(fit)$coefs.SE.CI
names(res) <- tolower(names(res))

res <- tibble::rownames_to_column(res, "variable")

# exponentiated results
res_exp <- res %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x / (1 - x)), 3)))
res_exp

coef(summary(fit1))[,4]
str(fit1)

#fit1$
res$pval <- round(pnorm(res$zval, mean = 0, sd = 1), 5)
res$pval <- ifelse(res$zval > 0, 1 - res$pval, res$pval)
