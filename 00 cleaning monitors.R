
library(sf)
library(dplyr)

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
# 
# aqs <- read.csv("/Users/brenna/Downloads/aqs_sites-2.csv")
# table(aqs$Location.Setting, aqs$Land.Use)
# 
# table(aqs$Site.Closed.Date == "")
# 
# aqs <- aqs %>%
#   filter(Site.Closed.Date == "") # only open sites
table(aqs$Tribe.Name == "")
mons <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2022.csv")
head(mons)

table(mons$Pollutant.Standard)

length(unique(mons$lat_lon))

#table(mons$Parameter.Name, mons$Pollutant.Standard == "")

mons$lat_lon <- paste0(mons$Latitude, mons$Longitude)

#table(mons$Pollutant.Standard, mons$Parameter.Name)

#mons <- mons %>%
#  filter(Pollutant.Standard != "")

mons$criteria <- case_when(mons$Parameter.Name %in% c("Lead (TSP) LC",
                                                      "Lead (TSP) STP",
                                                      "Lead PM10 LC FRM/FEM") ~ "pb",
                           mons$Parameter.Name == "Carbon monoxide" ~ "co",
                           mons$Parameter.Name %in% c("PM10 Total 0-10um STP",
                                                      "PM2.5 - Local Conditions") ~ "pm",
                           mons$Parameter.Name == "Nitrogen dioxide (NO2)" ~ "no2",
                           mons$Parameter.Name == "Ozone" ~ "o3",
                           mons$Parameter.Name == "Sulfur dioxide" ~ "so2")

mons <- mons %>%
  filter(criteria != "")

mons$criteria <- as.factor(mons$criteria)

# mons.ppp <- ppp(x = mons$Latitude, 
#                 y = mons$Latitude, 
#                    win = lansing.win, 
#                    marks = lansing$species)
# 
# criteria <- ppp(x = mons$Latitude, y = mons$Longitude, 
#                 window = states_shp.win, marks = mons$criteria)
# criteria <- rescale(criteria, 1000, "km")
# 
# plot(criteria)
# plot(split(criteria), main = "All marks")
# plot(density(split(criteria)), main = "criteria density surfaces")

# no duplicate locations
#mons <- mons[!duplicated(mons$lat_lon), ]

table(duplicated(mons$co$lat_lon), mons$co$criteria)

mons <- st_as_sf(mons, coords = c("Longitude", "Latitude"), 
                        crs = 4326, agr = "constant")
mons <- st_transform(mons, st_crs(aea))

#head(mons$Site.Num)
#max(aqs$Site.Number)
#aqs$Site.Number <- str_pad(aqs$Site.Number, 4, pad = "0")
#mons$Site.Num <- str_pad(mons$Site.Num, 4, pad = "0")

#table(is.na(mons$o3$lat_lon))

mons <- split(mons, mons$criteria, 6)


mons$pm <- mons$pm[!duplicated(mons$pm$lat_lon), ]
mons$co <- mons$co[!duplicated(mons$co$lat_lon), ]
mons$no2 <- mons$no2[!duplicated(mons$no2$lat_lon), ]
mons$o3 <- mons$o3[!duplicated(mons$o3$lat_lon), ]
mons$pb <- mons$pb[!duplicated(mons$pb$lat_lon), ]
mons$so2 <- mons$so2[!duplicated(mons$so2$lat_lon), ]

#table(mons$criteria)


table(mons$criteria)
length(unique(mons$Site.Num))

test <- merge(mons, aqs, by.x = "Site.Num", by.y = "Site.Number", all.x = TRUE)


