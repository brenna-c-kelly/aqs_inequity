
library(sf)
library(tidyr)
library(dplyr)
library(purrr)
library(raster)
library(viridis)
library(fasterize)
library(tidycensus)



bg_vars <- function(ST) {
  for (i in ST) {
    z_st <- get_acs(geography = "block group",
                    variables = c('B03002_004', # black 
                                  'B03002_005', #aian 
                                  'B03002_006', #asian 
                                  'B03002_007', #nhpi 
                                  'B03002_008', #som 
                                  'B03002_009', #tom 
                                  'B03002_012'),
                    #hisp, #"B02001_002"),#'B01001_001'),
                    state = i,
                    geometry = TRUE,
                    year = 2020,
                    show_call = FALSE)
    z_st <- z_st %>%
      spread(variable, estimate) %>%
      group_by(GEOID) %>%
      fill(B03002_004, # black
           B03002_005, #aian
           B03002_006, #asian
           B03002_007, #nhpi
           B03002_008, #som
           B03002_009, #tom
           B03002_012) %>% #hisp) %>%
      rename(black = B03002_004, #
             aian = B03002_005, #
             asian = B03002_006, #
             nhpi = B03002_007, #
             som = B03002_008, #
             tom = B03002_009, #
             hisp = B03002_012) %>%
      drop_na()#)
     st_write(z_st, paste0("data/race/", i, ".shp"))}
}
# 
# B03002_004 # black
# B03002_005 #aian
# B03002_006 #asian
# B03002_007 #nhpi
# B03002_008 #som
# B03002_009 #tom
# B03002_012 #hisp

ST <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
        "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY",
        "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
        "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
        "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
        "VT", "VA", "WA", "WV", "WI", "WY")

bg_vars(ST)

bg_covs <- map(list.files("data/race/", full.names = T, pattern = ".shp"), st_read) %>%
  bind_rows() %>%
  st_write("data/race_pop.shp")

# white <- st_read("data/white_pop.shp")
# pop <- st_read("data/total_pop.shp")
# white <- st_drop_geometry(white)
# 
# white_pop <- merge(pop, white, by = "GEOID")
# white_pop$non_white <- white_pop$total.x - white_pop$total.y
# white_pop$non_white_p <- white_pop$non_white / white_pop$total.x
# white_pop$white_pop_q <- ifelse(white_pop$non_white > median(white_pop$non_white),
                                1, 0)

white_pop <- st_transform(white_pop, st_crs(aea))

ext <- extent(white_pop)
r <- raster(ext, res = 4000)
table(st_is_empty(white))

library(moments)
white_pop <- white_pop[!st_is_empty(white_pop), , drop=FALSE]
min(white_pop[which(white_pop$non_white > 0), ]$non_white)
skewness(log(white_pop$non_white + 1))

white_pop_r <- fasterize(white_pop, r, field = "white_pop_q")

#plot(total_pop_r)
writeRaster(white_pop_r, filename = "data/nonwhite_q_pop.tif", overwrite = TRUE)
#head(total_pop_r)

#tm_shape(population) +
#  tm_polygons(col = "total")


total_2 <- as.im(read_stars("data/total_pop.tif")) %>%
  rescale(1000, "km")
plot(total)


head(population)

plot(pov)

poverty <- st_read("data/poverty.shp")

poverty$below_pov <- poverty$C17002_002 + poverty$C17002_003
poverty$pov_p <- poverty$below_pov / poverty$C17002_001
poverty$pov_p[is.na(poverty$pov_p)] <- 0
poverty$pov_perc <- poverty$pov_p*100

poverty <- poverty[!st_is_empty(poverty), , drop=FALSE]
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
poverty <- st_transform(poverty, st_crs(aea))

tm_shape(poverty) +
  tm_polygons(col = "pov_perc", lwd = 0, palette = "plasma")

poverty$pov_perc_lc <- log(poverty$pov_perc + 0.5) - log(mean(poverty$pov_perc + 0.5))

skewness(poverty$pov_perc_lc)

table(poverty$pov_perc > 0 & poverty$pov_perc < 0.5)

names(bg_vars)
hist(bg_vars$pov_perc_l)

library(moments)
skewness(bg_vars_log$aian_p_lc)

bg_vars$aian_p_lc <- log(bg_vars$aian_p + 0.00001) - log(mean(bg_vars$aian_p + 0.00001))
bg_vars$black_p_lc <- log(bg_vars$black_p + 0.0001) - log(mean(bg_vars$black_p + 0.0001))
bg_vars$

hist(bg_vars$aian_p_lc)
summary(bg_vars$aian_p_lc)
skewness(bg_vars$black_p_lc)

summary(bg_vars$black_p)

bg_vars$black_q <- case_when(bg_vars$black_p <= quantile(bg_vars$black_p, 0.25) ~ "0-0.25",
                             bg_vars$black_p <= quantile(bg_vars$black_p, 0.5) &
                               bg_vars$black_p > quantile(bg_vars$black_p, 0.25) ~ "0.25-0.50",
                             bg_vars$black_p <= quantile(bg_vars$black_p, 0.75) &
                               bg_vars$black_p > quantile(bg_vars$black_p, 0.5) ~ "0.50-0.75",
                             bg_vars$black_p > quantile(bg_vars$black_p, 0.75) ~ "0.75-1.00")
bg_vars$black_q <- ifelse(bg_vars$black_p > quantile(bg_vars$black_p, 0.75), "1", "0")
bg_vars$black_q <- relevel(as.factor(bg_vars$black_q), ref = "0-0.25")

head(bg_covs)

bg_vars <- bg_vars %>%
  mutate(black_q = case_when(bg_vars$black_p <= quantile(bg_vars$black_p, 0.2) ~ -2,
                             bg_vars$black_p <= quantile(bg_vars$black_p, 0.4) &
                               bg_vars$black_p > quantile(bg_vars$black_p, 0.2) ~ -1,
                             bg_vars$black_p <= quantile(bg_vars$black_p, 0.6) &
                               bg_vars$black_p > quantile(bg_vars$black_p, 0.4) ~ 0,
                             bg_vars$black_p <= quantile(bg_vars$black_p, 0.6) &
                               bg_vars$black_p > quantile(bg_vars$black_p, 0.8) ~ 1,
                             bg_vars$black_p <= quantile(bg_vars$black_p, 1) ~ 2)) %>%
  mutate(hispanic_q = case_when(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.2) ~ -2,
                                bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.4) &
                                  bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.2) ~ -1,
                                bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.6) &
                                  bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.4) ~ 0,
                                bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.6) &
                                  bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.8) ~ 1,
                                bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 1) ~ 2)) %>%
  mutate(aian_q = case_when(bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.2) ~ -2,
                                bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.4) &
                                  bg_vars$aian_p > quantile(bg_vars$aian_p, 0.2) ~ -1,
                                bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.6) &
                                  bg_vars$aian_p > quantile(bg_vars$aian_p, 0.4) ~ 0,
                                bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.6) &
                                  bg_vars$aian_p > quantile(bg_vars$aian_p, 0.8) ~ 1,
                                bg_vars$aian_p <= quantile(bg_vars$aian_p, 1) ~ 2)) %>%
  mutate(asian_q = case_when(bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.2) ~ -2,
                                bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.4) &
                                  bg_vars$asian_p > quantile(bg_vars$asian_p, 0.2) ~ -1,
                                bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.6) &
                                  bg_vars$asian_p > quantile(bg_vars$asian_p, 0.4) ~ 0,
                                bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.6) &
                                  bg_vars$asian_p > quantile(bg_vars$asian_p, 0.8) ~ 1,
                                bg_vars$asian_p <= quantile(bg_vars$asian_p, 1) ~ 2)) %>%
  mutate(nhpi_q = case_when(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.2) ~ -2,
                                bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.4) &
                                  bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.2) ~ -1,
                                bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.6) &
                                  bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.4) ~ 0,
                                bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.6) &
                                  bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.8) ~ 1,
                                bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 1) ~ 2)) %>%
  mutate(other_q = case_when(bg_vars$other_p <= quantile(bg_vars$other_p, 0.2) ~ -2,
                                bg_vars$other_p <= quantile(bg_vars$other_p, 0.4) &
                                  bg_vars$other_p > quantile(bg_vars$other_p, 0.2) ~ -1,
                                bg_vars$other_p <= quantile(bg_vars$other_p, 0.6) &
                                  bg_vars$other_p > quantile(bg_vars$other_p, 0.4) ~ 0,
                                bg_vars$other_p <= quantile(bg_vars$other_p, 0.6) &
                                  bg_vars$other_p > quantile(bg_vars$other_p, 0.8) ~ 1,
                                bg_vars$other_p <= quantile(bg_vars$other_p, 1) ~ 2)) %>%
  mutate(tom_q = case_when(bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.2) ~ -2,
                                bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.4) &
                                  bg_vars$tom_p > quantile(bg_vars$tom_p, 0.2) ~ -1,
                                bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.6) &
                                  bg_vars$tom_p > quantile(bg_vars$tom_p, 0.4) ~ 0,
                                bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.6) &
                                  bg_vars$tom_p > quantile(bg_vars$tom_p, 0.8) ~ 1,
                                bg_vars$tom_p <= quantile(bg_vars$tom_p, 1) ~ 2))


poverty <- poverty %>%
  mutate(pov_q = case_when(pov_perc <= quantile(pov_perc, 0.2) ~ -2,
                           pov_perc <= quantile(pov_perc, 0.4) &
                             pov_perc > quantile(pov_perc, 0.2) ~ -1,
                           pov_perc <= quantile(pov_perc, 0.6) &
                             pov_perc > quantile(pov_perc, 0.4) ~ 0,
                           pov_perc <= quantile(pov_perc, 0.6) &
                             pov_perc > quantile(pov_perc, 0.8) ~ 1,
                           pov_perc <= quantile(pov_perc, 1) ~ 2))   %>%
  mutate(pov_q.20 = ifelse(pov_perc <= quantile(pov_perc, 0.2), 1, 0)) %>%
  mutate(pov_q.40 = ifelse(pov_perc <= quantile(pov_perc, 0.4) &
                             pov_perc > quantile(pov_perc, 0.2), 1, 0)) %>%
  mutate(pov_q.60 = ifelse(pov_perc <= quantile(pov_perc, 0.6) &
                             pov_perc > quantile(pov_perc, 0.4), 1, 0)) %>%
  mutate(pov_q.80 = ifelse(pov_perc <= quantile(pov_perc, 0.8) &
                             pov_perc > quantile(pov_perc, 0.6), 1, 0)) %>%
  mutate(pov_q.100 = ifelse(pov_perc <= quantile(pov_perc, 1) &
                              pov_perc > quantile(pov_perc, 0.8), 1, 0))

summary(poverty$pov_q)

names(bg_vars)
ext <- extent(bg_vars)
r <- raster(ext, res = 4000)
rast <- fasterize(bg_vars, r, field = "tom_q")
writeRaster(rast, filename = "data/quintiles/tom_q.tif")

rast <- fasterize(poverty, r, field = "pov_q.100")
writeRaster(rast, filename = "data/quintiles/pov_q.100.tif")

black_q_im <- as.im(read_stars("data/quintiles/black_q.tif")) %>%
  rescale(1000, "km")

test <- st_drop_geometry(bg_vars)

f <- factor(test[, "black_q"])
dim(f) <- c(236882, 1)
plot(f)
f <- as.matrix(f)
Z <- as.im(as.matrix(bg_vars$black_q), W = states_shp.win)
plot(Z)
levels(black_q_im) <- c("0.25", "0.5", "0.75", "1")
  #c(1 == "0.25", 2 == "0.5", 3 == "0.75", 4 == "1")
fit_so2_q <- ppm(monitors.ppp_so2 ~ black_q_im*pov_im + urban + offset(total))
summary(fit_so2_q)
plot(black_q_im)

rbind(bg_vars$geometry, bg_vars$black_q)

test <- bg_vars %>%
  filter(aian_p != 0)
skewness(test$aian_p_lc)

table(bg_vars$aian_p == 0)

# rasterize log-transformed poverty
summary(ruca)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
pov_r <- fasterize(poverty, r, field = "pov_q.100")
writeRaster(pov_r, filename = "data/quintiles/pov_q.100.tif")
pov.100 <- as.im(read_stars("data/quintiles/pov_q.100.tif")) %>%
  rescale(1000, "km")
pov.80 <- as.im(read_stars("data/quintiles/pov_q.80.tif")) %>%
  rescale(1000, "km")
pov.60 <- as.im(read_stars("data/quintiles/pov_q.60.tif")) %>%
  rescale(1000, "km")
pov.40 <- as.im(read_stars("data/quintiles/pov_q.40.tif")) %>%
  rescale(1000, "km")
pov.20 <- as.im(read_stars("data/quintiles/pov_q.20.tif")) %>%
  rescale(1000, "km")


# testing dummy covariates
bg_vars <- bg_vars %>%
  mutate(tom_q.20 = ifelse(bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.2), 1, 0)) %>%
  mutate(tom_q.40 = ifelse(bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.4) &
                              bg_vars$tom_p > quantile(bg_vars$tom_p, 0.2), 1, 0)) %>%
  mutate(tom_q.60 = ifelse(bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.6) &
                             bg_vars$tom_p > quantile(bg_vars$tom_p, 0.4), 1, 0)) %>%
  mutate(tom_q.80 = ifelse(bg_vars$tom_p <= quantile(bg_vars$tom_p, 0.8) &
                             bg_vars$tom_p > quantile(bg_vars$tom_p, 0.6), 1, 0)) %>%
  mutate(tom_q.100 = ifelse(bg_vars$tom_p <= quantile(bg_vars$tom_p, 1) &
                             bg_vars$tom_p > quantile(bg_vars$tom_p, 0.8), 1, 0)) %>%
  mutate(black_q.20 = ifelse(bg_vars$black_p <= quantile(bg_vars$black_p, 0.2), 1, 0)) %>%
  mutate(black_q.40 = ifelse(bg_vars$black_p <= quantile(bg_vars$black_p, 0.4) &
                             bg_vars$black_p > quantile(bg_vars$black_p, 0.2), 1, 0)) %>%
  mutate(black_q.60 = ifelse(bg_vars$black_p <= quantile(bg_vars$black_p, 0.6) &
                             bg_vars$black_p > quantile(bg_vars$black_p, 0.4), 1, 0)) %>%
  mutate(black_q.80 = ifelse(bg_vars$black_p <= quantile(bg_vars$black_p, 0.8) &
                             bg_vars$black_p > quantile(bg_vars$black_p, 0.6), 1, 0)) %>%
  mutate(black_q.100 = ifelse(bg_vars$black_p <= quantile(bg_vars$black_p, 1) &
                              bg_vars$black_p > quantile(bg_vars$black_p, 0.8), 1, 0))  %>%
  mutate(aian_q.20 = ifelse(bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.2), 1, 0)) %>%
  mutate(aian_q.40 = ifelse(bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.4) &
                               bg_vars$aian_p > quantile(bg_vars$aian_p, 0.2), 1, 0)) %>%
  mutate(aian_q.60 = ifelse(bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.6) &
                               bg_vars$aian_p > quantile(bg_vars$aian_p, 0.4), 1, 0)) %>%
  mutate(aian_q.80 = ifelse(bg_vars$aian_p <= quantile(bg_vars$aian_p, 0.8) &
                               bg_vars$aian_p > quantile(bg_vars$aian_p, 0.6), 1, 0)) %>%
  mutate(aian_q.100 = ifelse(bg_vars$aian_p <= quantile(bg_vars$aian_p, 1) &
                                bg_vars$aian_p > quantile(bg_vars$aian_p, 0.8), 1, 0))  %>%
  mutate(asian_q.20 = ifelse(bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.2), 1, 0)) %>%
  mutate(asian_q.40 = ifelse(bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.4) &
                               bg_vars$asian_p > quantile(bg_vars$asian_p, 0.2), 1, 0)) %>%
  mutate(asian_q.60 = ifelse(bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.6) &
                               bg_vars$asian_p > quantile(bg_vars$asian_p, 0.4), 1, 0)) %>%
  mutate(asian_q.80 = ifelse(bg_vars$asian_p <= quantile(bg_vars$asian_p, 0.8) &
                               bg_vars$asian_p > quantile(bg_vars$asian_p, 0.6), 1, 0)) %>%
  mutate(asian_q.100 = ifelse(bg_vars$asian_p <= quantile(bg_vars$asian_p, 1) &
                                bg_vars$asian_p > quantile(bg_vars$asian_p, 0.8), 1, 0))  %>%
  mutate(nhpi_q.20 = ifelse(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.2), 1, 0)) %>%
  mutate(nhpi_q.40 = ifelse(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.4) &
                               bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.2), 1, 0)) %>%
  mutate(nhpi_q.60 = ifelse(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.6) &
                               bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.4), 1, 0)) %>%
  mutate(nhpi_q.80 = ifelse(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 0.8) &
                               bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.6), 1, 0)) %>%
  mutate(nhpi_q.100 = ifelse(bg_vars$nhpi_p <= quantile(bg_vars$nhpi_p, 1) &
                                bg_vars$nhpi_p > quantile(bg_vars$nhpi_p, 0.8), 1, 0))  %>%
  mutate(other_q.20 = ifelse(bg_vars$other_p <= quantile(bg_vars$other_p, 0.2), 1, 0)) %>%
  mutate(other_q.40 = ifelse(bg_vars$other_p <= quantile(bg_vars$other_p, 0.4) &
                               bg_vars$other_p > quantile(bg_vars$other_p, 0.2), 1, 0)) %>%
  mutate(other_q.60 = ifelse(bg_vars$other_p <= quantile(bg_vars$other_p, 0.6) &
                               bg_vars$other_p > quantile(bg_vars$other_p, 0.4), 1, 0)) %>%
  mutate(other_q.80 = ifelse(bg_vars$other_p <= quantile(bg_vars$other_p, 0.8) &
                               bg_vars$other_p > quantile(bg_vars$other_p, 0.6), 1, 0)) %>%
  mutate(other_q.100 = ifelse(bg_vars$other_p <= quantile(bg_vars$other_p, 1) &
                                bg_vars$other_p > quantile(bg_vars$other_p, 0.8), 1, 0))  %>%
  mutate(hisp_q.20 = ifelse(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.2), 1, 0)) %>%
  mutate(hisp_q.40 = ifelse(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.4) &
                               bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.2), 1, 0)) %>%
  mutate(hisp_q.60 = ifelse(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.6) &
                               bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.4), 1, 0)) %>%
  mutate(hisp_q.80 = ifelse(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 0.8) &
                               bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.6), 1, 0)) %>%
  mutate(hisp_q.100 = ifelse(bg_vars$hispanic_p <= quantile(bg_vars$hispanic_p, 1) &
                                bg_vars$hispanic_p > quantile(bg_vars$hispanic_p, 0.8), 1, 0))

ext <- extent(bg_vars)
r <- raster(ext, res = 4000)
rast <- fasterize(bg_vars, r, field = "hisp_q.100")
writeRaster(rast, filename = "data/quintiles/hispanic_q.100.tif")
rast <- fasterize(bg_vars, r, field = "hisp_q.80")
writeRaster(rast, filename = "data/quintiles/hispanic_q.80.tif")
rast <- fasterize(bg_vars, r, field = "hisp_q.60")
writeRaster(rast, filename = "data/quintiles/hispanic_q.60.tif")
rast <- fasterize(bg_vars, r, field = "hisp_q.40")
writeRaster(rast, filename = "data/quintiles/hispanic_q.40.tif")
rast <- fasterize(bg_vars, r, field = "hisp_q.20")
writeRaster(rast, filename = "data/quintiles/hispanic_q.20.tif")



hisp.20 <- as.im(read_stars('data/quintiles/hispanic_q.20.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
hisp.40 <- as.im(read_stars('data/quintiles/hispanic_q.40.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
hisp.60 <- as.im(read_stars('data/quintiles/hispanic_q.60.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
hisp.80 <- as.im(read_stars('data/quintiles/hispanic_q.80.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")
hisp.100 <- as.im(read_stars('data/quintiles/hispanic_q.100.tif')) %>%#("data/tifs/tom_lc.tif")) %>%
  rescale(1000, "km")

fit_co <- ppm(monitors.ppp_co ~ (aian + asian + black + hisp +
                                   nhpi + other + tom)*pov*rural + offset(total))
AIC(fit_co)
summary(fit_co)


