
library(sf)
library(tmap)
library(dplyr)
library(raster)
library(viridis)
library(fasterize)
library(tidycensus)
library(wesanderson)

naa_o3 <- st_read("/Users/brenna/Downloads/ozone_8hr_2015std_naa_shapefile")
naa_pm <- st_read("/Users/brenna/Downloads/pm25_2012std_naa_shapefile")
naa_so2 <- st_read("/Users/brenna/Downloads/so2_2010std_naa_shapefile")
naa_pb <- st_read("/Users/brenna/Downloads/lead_2008std_naa_shapefile")
naa_no2 <- st_read("/Users/brenna/Downloads/no2_1971std_naa_shapefile") # maintenance
naa_co <- st_read("/Users/brenna/Downloads/co_1971std_naa_shapefile") # maintenance


naa_co <- st_transform(naa_co, st_crs(aea))

print(wes_palette("Zissou1", 21, type = "continuous"))
poverty

total_l <- log(total)
viridis(100, option = "")
plot(log(total), col = viridis(100, direction = -1, option = "mako")) #col = c("#faf6bc", "#dfe9b4", "#c6ddac",
                            # "#a4d0ae", "#83c4b0", "#57a1b3",
                            # "#377fb8", "#2b5c9e", "#2c3985"))#col = rev(wes_palette("Zissou1", 21, type = "continuous")))#col = c("#29388a", "#fbf6b5", "#ff0000"))

col = c("#faf6bc", "#dfe9b4", "#c6ddac",
       "#a4d0ae", "#83c4b0", "#57a1b3",
       "#377fb8", "#2b5c9e", "#2c3985")

plot(nhpi.tess, col = c("#2c3985", "#faf6bc"))

pal <- brewer.pal(9, "OrRd")

median(bg_covs$aian_p)

tm_shape(poverty) +
  tm_polygons(col = "pov_p", lwd = 0, palette = c("#bedca5", "#f9f4b3",
                                                  "#fa8a3c", "#ec4a22", "#bb2503"),
              style = "cont")

poverty <- st_read("data/poverty.shp")
poverty <- poverty[!st_is_empty(poverty), , drop=FALSE]
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
poverty <- st_transform(poverty, st_crs(aea))

us_geog <- get_acs(geography = "us",
                   variables = c('C17002_003'),
                geometry = TRUE,
                year = 2020,
                show_call = FALSE)

us_geog <- st_transform(us_geog, st_crs(aea))

all_areas <- us_geog %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()





# pb
naa_no2 <- st_transform(naa_no2, st_crs(aea))
naa_no2 <- st_make_valid(naa_no2)
naa_no2_group <- naa_no2 %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_no2 <- st_difference(us_geog, naa_pb_group)
aa_no2 <- aa_no2[, c(1, 2)]
naa_no2 <- naa_no2[, c(1, 2)]
names(aa_no2) <- names(naa_no2)

aa_no2$naa <- 0
naa_no2$naa <- 1

all_no2 <- rbind(aa_no2, naa_no2)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
all_no2_r <- fasterize(st_collection_extract(all_no2, "POLYGON"), r, field = "naa")
plot(all_no2_r)
writeRaster(all_no2_r, filename = "data/naa_no2.tif")

# pb
naa_pb <- st_transform(naa_pb, st_crs(aea))
naa_pb <- st_make_valid(naa_pb)
naa_pb_group <- naa_pb %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_pb <- st_difference(us_geog, naa_pb_group)
aa_pb <- aa_pb[, c(1, 2)]
naa_pb <- naa_pb[, c(1, 2)]
names(aa_pb) <- names(naa_pb)

aa_pb$naa <- 0
naa_pb$naa <- 1

all_pb <- rbind(aa_pb, naa_pb)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
all_pb_r <- fasterize(st_collection_extract(all_pb, "POLYGON"), r, field = "naa")
plot(all_pb_r)
writeRaster(all_pb_r, filename = "data/naa_pb.tif")

# so2
naa_so2 <- st_transform(naa_so2, st_crs(aea))
naa_so2 <- st_make_valid(naa_so2)
naa_so2_group <- naa_so2 %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_so2 <- st_difference(us_geog, naa_so2_group)
aa_so2 <- aa_so2[, c(1, 2)]
naa_so2 <- naa_so2[, c(1, 2)]
names(aa_so2) <- names(naa_so2)

aa_so2$naa <- 0
naa_so2$naa <- 1

all_so2 <- rbind(aa_so2, naa_so2)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
all_so2_r <- fasterize(st_collection_extract(all_so2, "POLYGON"), r, field = "naa")
plot(all_so2_r)
writeRaster(all_so2_r, filename = "data/naa_so2.tif")

# pm
naa_pm <- st_transform(naa_pm, st_crs(aea))
naa_pm <- st_make_valid(naa_pm)
naa_pm_group <- naa_pm %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_pm <- st_difference(us_geog, naa_pm_group)
aa_pm <- aa_pm[, c(1, 2)]
naa_pm <- naa_pm[, c(1, 2)]
names(aa_pm) <- names(naa_pm)

aa_pm$naa <- 0
naa_pm$naa <- 1

all_pm <- rbind(aa_pm, naa_pm)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
all_pm_r <- fasterize(st_collection_extract(all_pm, "POLYGON"), r, field = "naa")
plot(all_pm_r)
writeRaster(all_pm_r, filename = "data/naa_pm.tif")

# o3
naa_o3 <- st_transform(naa_o3, st_crs(aea))
naa_o3 <- st_make_valid(naa_o3)
naa_o3_group <- naa_o3 %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_o3 <- st_difference(all_areas, naa_o3_group)
aa_o3$composite = "Ozone"
aa_o3$AREA_NAME = "AA"
#aa_o3 <- aa_o3[, c(1, 2)]
naa_o3 <- naa_o3[, c("geometry")]
names(aa_o3) <- names(naa_o3)

aa_o3$naa <- 0
naa_o3$naa <- 1

all_o3 <- rbind(aa_o3, naa_o3)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
naa_o3_r <- fasterize(st_collection_extract(all_o3, "POLYGON"), r, field = "naa")
writeRaster(naa_o3_r, filename = "data/naa_o3.tif", overwrite = TRUE)


# co
naa_co <- st_make_valid(naa_co)
naa_co_group <- naa_co %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

aa_co <- st_difference(us_geog, naa_co_group)
aa_co <- aa_co[, c(1, 2)]
names(aa_co) <- names(naa_co)

aa_co$naa <- 0
naa_co$naa <- 1

all_co <- rbind(aa_co, naa_co)

ext <- extent(poverty)
r <- raster(ext, res = 4000)
naa_co_r <- fasterize(st_collection_extract(all_co, "POLYGON"), r, field = "naa")
writeRaster(naa_co_r, filename = "data/naa_co.tif")



