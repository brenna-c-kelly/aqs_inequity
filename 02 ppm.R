
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

#length(unique(monitors_sf$lat_long))

#monitors_imp <- read.csv("/Users/brenna/Documents/School/GEOG 6960/monitors_imp.csv")
#test <- read.csv("/Users/brenna/Documents/School/GEOG 6960/final project/aqs_monitors-2.csv")
#length(unique(test$site_id))
#head(monitors_imp)

#test <- st_as_sf(monitors_imp, coords = c("longitude", "latitude"),
#                 crs = 4326, agr = "constant")
#test <- st_transform(test, st_crs(aea))

#monitors_sf$lat_long <- paste0(monitors_sf$lat, monitors_sf$long, sep = ", ")

# point data
monitors_sf <- st_read("/Users/brenna/Downloads/monitors_sf_clean.shp")
monitors_sf <- st_as_sf(monitors_sf, coords = c("long", "lat"), 
                        crs = 4326, agr = "constant")
monitors_sf <- st_transform(monitors_sf, st_crs(aea))
# convert to ppp
mon_crds <- st_coordinates(mons$co)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_co <- rescale(monitors.ppp, 1000, "km")
# convert to ppp
mon_crds <- st_coordinates(mons$no2)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_no2 <- rescale(monitors.ppp, 1000, "km")
# convert to ppp
mon_crds <- st_coordinates(mons$o3)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_o3 <- rescale(monitors.ppp, 1000, "km")
# convert to ppp
mon_crds <- st_coordinates(mons$pb)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_pb <- rescale(monitors.ppp, 1000, "km")
# convert to ppp
mon_crds <- st_coordinates(mons$pm)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_pm <- rescale(monitors.ppp, 1000, "km")
# convert to ppp
mon_crds <- st_coordinates(mons$so2)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_so2 <- rescale(monitors.ppp, 1000, "km")
#monitors.ppp_pm <- rjitter(monitors.ppp, edge = "none")

# rasterized covariates
aian <- as.im(read_stars('data/quintiles/aian_q.tif')) %>%#"data/tifs/aian_lc.tif")) %>%
  rescale(1000, "km")
asian <- as.im(read_stars('data/quintiles/asian_q.tif')) %>%#("data/tifs/asian_lc.tif")) %>%
  rescale(1000, "km")
black <- as.im(read_stars('data/quintiles/black_q.tif')) %>%#("data/tifs/black_lc.tif")) %>%
  rescale(1000, "km")
hisp <- as.im(read_stars('data/quintiles/hispanic_q.tif')) %>%#("data/tifs/hispanic_lc.tif")) %>%
  rescale(1000, "km")
nhpi <- as.im(read_stars('data/quintiles/nhpi_q.tif')) %>%#("data/tifs/nhpi_lc.tif")) %>%
  rescale(1000, "km")
other <- as.im(read_stars('data/quintiles/other_q.tif')) %>%#("data/tifs/other_lc.tif")) %>%
  rescale(1000, "km")
tom <- as.im(read_stars('data/quintiles/tom_q.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
rural <- as.im(read_stars('data/rural_urban.tif')) %>%#("data/tifs/metro.tif")) %>%
  rescale(1000, "km") ##
#urban <- as.im(read_stars('data/quintiles/aian_q.tif')) %>%#("data/urban.tif")) %>%
#  rescale(1000, "km")
pov <- as.im(read_stars('data/quintiles/pov_q.tif')) %>%#("data/tifs/pov_lc.tif")) %>%
  rescale(1000, "km")
total <- as.im(read_stars("data/tifs/total_lc.tif")) %>%
  rescale(1000, "km")
# nonattainment / maintenance areas
naa_co <- as.im(read_stars("data/naa_co.tif")) %>%
  rescale(1000, "km")
naa_no2 <- as.im(read_stars("data/naa_no2.tif")) %>%
  rescale(1000, "km")
naa_o3 <- as.im(read_stars("data/naa_o3.tif")) %>%
  rescale(1000, "km")
naa_pb <- as.im(read_stars("data/naa_pb.tif")) %>%
  rescale(1000, "km")
naa_pm <- as.im(read_stars("data/naa_pm.tif")) %>%
  rescale(1000, "km")
naa_so2 <- as.im(read_stars("data/naa_so2.tif")) %>%
  rescale(1000, "km")

# ppms by criteria pollutant
fit_co <- ppm(monitors.ppp_co ~ (aian + asian + black + hisp +
                                   nhpi + other + tom)*pov*rural + naa_co + offset(total))
fit_no2 <- ppm(monitors.ppp_no2 ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*pov*rural + naa_no2 + offset(total)) # 3.2
fit_o3 <- ppm(monitors.ppp_o3 ~ (aian + asian + black + hisp +
                                 nhpi + other + tom)*pov*rural + naa_o3 + offset(total))
fit_pb <- ppm(monitors.ppp_pb ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*povrural + naa_pb + offset(total)) # 13.7
fit_pm <- ppm(monitors.ppp_pm ~ (aian + asian + black + hisp +
                                nhpi + other + tom)*pov*rural + naa_pm + offset(total)) # 2.1
fit_so2 <- ppm(monitors.ppp_so2 ~ (aian + asian + black + hisp +
                                     nhpi + other + tom)*pov*rural + naa_so2 + offset(total)) 
summary(fit_co)
AIC(fit_co)
fit_so2$Q
fit_pm$Q
fit_pb$Q
fit_co$Q
fit_no2$Q
fit_o3$Q

AIC(fit_co)
AIC(fit_no2)
AIC(fit_o3)
AIC(fit_pb)
AIC(fit_pm)
AIC(fit_so2)

VIF(fit_pm)

# > AIC(fit_co)
# [1] 4882.603 / 4880.171 / 4904.138 / 4901.207
# > AIC(fit_no2)
# [1] 8866.33 / 8873.301 / 8893.974 / 8877.923
# > AIC(fit_o3)
# [1] 22113.05 / 22140.32 / 22141.82 / 22125.16
# > AIC(fit_pb)
# [1] 2907.587 / 2922.753 / 2925.791 / 2921.695
# > AIC(fit_pm)
# [1] 22151.04 / 22202.69 / 22210.64 / 22163.81
# > AIC(fit_so2)
# [1] 8335.831 / 8337.19 / 8370.246 / 8365.49

#                  all intx / race*pov + metro /  all + / race*urban + pov
# > AIC(fit_co)   4765.965 / 4748.102* / 4801.056 / 4810.141
#                 4882.603 / 4880.171 / 
# > AIC(fit_no2)  8789.852* / 8790.951 / 8817.708 / 8810.91
# > AIC(fit_o3)   22061.49* / 22062.43 / 22126.73 / 22127.78 
# > AIC(fit_pb)   2917.702 / 2905.743* / 2934.745 / 2940.461
#                 2907.587 / 2922.753
# > AIC(fit_pm)   22067.79* / 22098.2 / 22134.23 / 22110.27
# > AIC(fit_so2)  8341.569 / 8331.611* / 8382.476 / 8385.113 // 8336.576 (race*urban + race*pov)
summary(fit_co)
summary(fit_no2)
summary(fit_o3)
summary(fit_pb)
summary(fit_pm)
summary(fit_so2)

fit_no2$internal$glmfit
fit_so2$internal$glmdata



VIF

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
VIF(fit_so2)
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
