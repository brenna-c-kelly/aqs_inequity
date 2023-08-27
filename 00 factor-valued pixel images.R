
library(sf)
library(stars)
library(raster)
library(moments)
library(spatstat)
library(regclass)
library(tidycensus)
library(RColorBrewer)

data("clmfires")
names(clmfires.extra)
names(clmfires.extra$clmcov200)
names(clmfires.extra$clmcov200$landuse)
levels(clmfires.extra$clmcov200$landuse$v)

plot(clmfires.extra$clmcov200, main = "200 m grid covariates")
class(clmfires.extra$clmcov200$landuse)

clmfires.lu.grid <- as(clmfires.extra$clmcov200$landuse,
                       "SpatialGridDataFrame")

aian_grid <- as(test,
                "SpatialGridDataFrame")
aian_grid$layer <- as.factor(aian_grid$layer)
aian_grid$layer <- relevel(aian_grid$layer, ref = "1")
summary(aian_grid)
spplot(aian_grid)

aian <- as.im(read_stars('data/aian_p.tif')) %>%#"data/tifs/aian_lc.tif")) %>%
  rescale(1000, "km")
asian <- as.im(read_stars('data/asian_p.tif')) %>%#("data/tifs/asian_lc.tif")) %>%
  rescale(1000, "km")
black <- as.im(read_stars('data/black_p.tif')) %>%#("data/tifs/black_lc.tif")) %>%
  rescale(1000, "km")
hisp <- as.im(read_stars('data/hisp_p.tif')) %>%#("data/tifs/hispanic_lc.tif")) %>%
  rescale(1000, "km")
nhpi <- as.im(read_stars('data/nhpi_p.tif')) %>%#("data/tifs/nhpi_lc.tif")) %>%
  rescale(1000, "km")
other <- as.im(read_stars('data/other_p.tif')) %>%#("data/tifs/other_lc.tif")) %>%
  rescale(1000, "km")
tom <- as.im(read_stars('data/tom_p_50.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
rural <- as.im(read_stars('data/rural_urban.tif')) %>%#("data/tifs/metro.tif")) %>%
  rescale(1000, "km") ##
#urban <- as.im(read_stars('data/quintiles/aian_q.tif')) %>%#("data/urban.tif")) %>%
#  rescale(1000, "km")
pov <- as.im(read_stars('data/pov_10p.tif')) %>%#pov_p_50.tif')) %>%#("data/tifs/pov_lc.tif")) %>%
  rescale(1000, "km")
total <- as.im(read_stars("data/total_10k.tif")) %>%
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
#test <- as.im(read_stars('data/test.tif')) %>%#("data/tifs/black_lc.tif")) %>%
#  rescale(1000, "km")

par(mfrow = c(2, 3))
plot(black.tess)
plot(hisp.tess)
plot(pov)
plot(rural.tess)
plot(naa_o3.tess)
dev.off()

pal <- brewer.pal(7, "YlGnBu")
rep()
max(pov)
cuts=c(-2, 0, 1, 2, 3, 4, 5, 6, 7, 8) #set breaks

pal <- colorRampPalette(c("white","black"))

plot(pov, breaks=cuts, col = brewer.pal(9, "YlOrRd"))
median(bg_covs$pov_p)
quantile(bg_covs$pov_p, 5/6)
max(bg_covs$pov_percent_10)
max(bg_covs$pov_p)
min(bg_covs$pov_percent_10)
min(bg_covs$pov_p)
mean(bg_covs$pov_percent_10)

(bg_covs$pov_p)

par(mfrow = c(2, 3))
plot(monitors.ppp_co)
plot(monitors.ppp_no2)
plot(monitors.ppp_pb)
plot(monitors.ppp_pm)
plot(monitors.ppp_so2)
dev.off()

plot(rural.tess, col = c("#0080bd", "#c0dea7"))

aian.cut <- cut(aian, breaks = c(-1, 
                                 #quantile(bg_covs$aian_p, prob = c(0.1, 0.9, 1))[[2]],
                                 quantile(bg_covs$aian_p, prob = c(0.5, 1))[[1]],
                                 
                                 quantile(bg_covs$aian_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                #                max(bg_covs$tom_p)), 
                labels = c("lowest", "upper"))
aian.tess <- tess(image = aian.cut)
plot(aian.tess)
asian.cut <- cut(asian, breaks = c(-1, 
                                   #quantile(bg_covs$asian_p, prob = c(0.1, 0.9, 1))[[2]],
                                   quantile(bg_covs$asian_p, prob = c(0.5, 1))[[1]],
                                   
                                   quantile(bg_covs$asian_p, prob = c( 0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                 #                max(bg_covs$tom_p)), 
                 labels = c("lowest", "upper"))

asian.tess <- tess(image = asian.cut)
plot(asian.tess)

black.cut <- cut(black, breaks = c(-1, 
                                   #quantile(bg_covs$black_p, prob = c(0.1, 0.9, 1))[[2]],
                                   quantile(bg_covs$black_p, prob = c(0.5, 1))[[1]],
                                   
                                   quantile(bg_covs$black_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                 #                max(bg_covs$tom_p)), 
                 labels = c("lowest", "upper"))
black.tess <- tess(image = black.cut)
plot(black.tess)
hisp.cut <- cut(hisp, breaks = c(-1, 
                                # quantile(bg_covs$hisp_p, prob = c(0.1, 0.9, 1))[[2]],
                                 quantile(bg_covs$hisp_p, prob = c(0.5, 1))[[1]],
                                 
                                 quantile(bg_covs$hisp_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                #                max(bg_covs$tom_p)), 
                labels = c("lowest", "upper"))
hisp.tess <- tess(image = hisp.cut)
plot(hisp.tess)
nhpi.cut <- cut(nhpi, breaks = c(-1,  # special case with nhpi
                                 #quantile(bg_covs$nhpi_p, prob = c(0.1, 0.9, 1))[[2]],
                                 quantile(bg_covs$nhpi_p, prob = c(0.5, 1))[[1]],
                                 quantile(bg_covs$nhpi_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                #                max(bg_covs$tom_p)), 
                labels = c("lowest", "upper"))
nhpi.tess <- tess(image = nhpi.cut)
plot(nhpi.tess)
other.cut <- cut(other, breaks = c(-1, 
                                   #quantile(bg_covs$other_p, prob = c(0.1, 0.9, 1))[[2]],
                                   quantile(bg_covs$other_p, prob = c(0.5, 1))[[1]],
                                   
                                   quantile(bg_covs$other_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
                 #                max(bg_covs$tom_p)), 
                 labels = c("lowest", "upper"))
other.tess <- tess(image = other.cut)
plot(other.tess)
quantile(bg_covs$tom_p, prob = c(0.2, 0.4, 0.6, 0.8, 1))
tom.cut <- cut(tom, breaks = c(-1, 
                               #quantile(bg_covs$tom_p, prob = c(0.1, 0.9, 1))[[2]],
                               quantile(bg_covs$tom_p, prob = c(0.5, 1))[[1]],
                               
                               quantile(bg_covs$tom_p, prob = c(0.5, 1))[[2]]),#median(bg_covs$tom_p), 
               #                max(bg_covs$tom_p)), 
               labels = c("lowest", "upper"))
tom.tess <- tess(image = tom.cut)
plot(tom.tess)


plot(pov)
#quantile(bg_covs$pov_p, prob=c(0.50), type=1)[[1]],
pov.cut <- cut(pov, breaks = c(-1,  median(bg_covs$pov_p), 
                               max(bg_covs$pov_p)), labels = 0:1)
pov.tess <- tess(image = pov.cut)
plot(pov.tess)

rural.cut <- cut(rural, breaks = c(-1, 0.5, 1), labels = 0:1)
rural.tess <- tess(image = rural.cut)
plot(rural.tess)

naa_co.cut <- cut(naa_co, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_co.tess <- tess(image = naa_co.cut)
plot(naa_co.tess)

naa_no2.cut <- cut(naa_no2, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_no2.tess <- tess(image = naa_no2.cut)
plot(naa_no2.tess)

naa_o3.cut <- cut(naa_o3, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_o3.tess <- tess(image = naa_o3.cut)
plot(naa_o3.tess)

naa_pb.cut <- cut(naa_pb, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_pb.tess <- tess(image = naa_pb.cut)
plot(naa_pb.tess)

naa_pm.cut <- cut(naa_pm, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_pm.tess <- tess(image = naa_pm.cut)
plot(naa_pm.tess)

naa_so2.cut <- cut(naa_so2, breaks = c(-1, 0.5, 1), labels = 0:1)
naa_so2.tess <- tess(image = naa_so2.cut)
plot(naa_so2.tess)






# ppms by criteria pollutant
fit_co <- ppm(monitors.ppp_co ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                   nhpi.tess + other.tess + tom.tess) + pov + rural.tess + 
                naa_co.tess + offset(total))
fit_no2 <- ppm(monitors.ppp_no2 ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                     nhpi.tess + other.tess + tom.tess)*pov + rural.tess + 
                 naa_no2 + offset(total)) # 3.2
fit_o3 <- ppm(monitors.ppp_o3 ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                   nhpi.tess + other.tess + tom.tess)*pov + rural.tess +
                naa_o3 + offset(total))
fit_pb <- ppm(monitors.ppp_pb ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                   nhpi.tess + other.tess + tom.tess)*pov + rural.tess + 
                naa_pb + offset(total)) # 13.7
fit_pm <- ppm(monitors.ppp_pm ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                   nhpi.tess + other.tess + tom.tess)*pov*rural.tess + 
                naa_pm + offset(total)) # 2.1
fit_so2 <- ppm(monitors.ppp_so2 ~ (aian.tess + asian.tess + black.tess + hisp.tess +
                                     nhpi.tess + other.tess + tom.tess)*pov + rural.tess + 
                 naa_so2 + offset(total)) 

AIC(fit_so2)
# median, intx: 8227.428    8266.563
# *pov + rural  8220.013    8254.576
# + pov + rural 8237.963    8261.156
# *rural + pov  8237.076    8263.375
summary(fit_so2)

AIC(fit_pm)
# median, intx: 22336.71    22319.97 **   22363.6 **
# *pov + rural  22332.48    22328.68      22382.5
# + pov + rural 22329.39 ** 22325.13      22394.58
# *rural + pov  22335.71    22331.71      22397.98
summary(fit_pm)

AIC(fit_pb)
# median, intx: 2860.77       dnc         dnc
# *pov + rural  2853.779     2852.772 **  2850.421 **
# + pov + rural 2852.469 **  2855.742     2870.919
# *rural + pov  dnc
summary(fit_pb)

AIC(fit_o3)
# median, intx: 21975.43    21966.24 **   21968.87
# *pov + rural  21966.60 ** 21968.92      21965.9 **
# + pov + rural 21984.43    21985.43      21984.1
# *rural + pov  21991.67    21992.68      21992.79
summary(fit_o3)

AIC(fit_no2)
# median, intx: 8918.675    8913.244      9006.16
# *pov + rural  8916.632    8905.125 **   8996.272 **
# + pov + rural 8916.607 ** 8911.902      9001.06
# *rural + pov  8916.607    8914.221      9005.026
summary(fit_no2)

AIC(fit_co)
# median, intx: 4662.27       4668.088
# *pov + rural  4650.557      4655.154
# + pov + rural 4649.878 **   4655.867 **
# *rural + pov  4650.468      4657.929
summary(fit_co)





