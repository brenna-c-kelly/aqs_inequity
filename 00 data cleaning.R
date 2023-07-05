
library(tmap)
library(dplyr)
library(stringr)
library(fasterize)
library(tidycensus)

vars_acs_20 <- load_variables(2020, "acs5", cache = TRUE)

# block group data
head(bg_vars)
bg_vars <- st_read("/Users/brenna/Documents/School/GEOG 6960/bg_vars.shp")
bg_vars_log <- bg_vars %>%
  mutate(hispanic_p = hispanic_p + 1e-3) %>%
  mutate(white_p = white_p + 1e-3) %>%
  mutate(black_p = black_p + 1e-3) %>%
  mutate(aian_p = aian_p + 1e-3) %>%
  mutate(asian_p = asian_p + 1e-3) %>%
  mutate(nhpi_p = nhpi_p + 1e-3) %>%
  mutate(other_p = other_p + 1e-3) %>%
  mutate(tom_p = tom_p + 1e-3) %>%
  mutate(nonwhite_p = nonwhite_p + 1e-3) %>%
  mutate(hispanic_per_c = log(hispanic_p*100) - log(mean(hispanic_p))) %>%
  mutate(white_log_per_c = log(white_p*100) - log(mean(white_p))) %>%
  mutate(black_log_per_c = log(black_p*100) - log(mean(black_p))) %>%
  mutate(aian_log_per_c = log(aian_p*100) - log(mean(aian_p))) %>%
  mutate(asian_log_per_c = log(asian_p*100) - log(mean(asian_p))) %>%
  mutate(nhpi_log_per_c = log(nhpi_p*100) - log(mean(nhpi_p))) %>%
  mutate(other_log_per_c = log(other_p*100) - log(mean(other_p))) %>%
  mutate(tom_log_per_c = log(tom_p*100) - log(mean(tom_p))) %>%
  mutate(nonwhite_log_per_c = log(nonwhite_p*100) - log(mean(nonwhite_p)))


# ruca
ruca <- read.csv("/Users/brenna/Documents/School/GEOG 6960/ruca_etc.csv")
ruca$tractfp <- str_pad(ruca$tractfp, 11, pad = "0")

# prepping for join
bg_vars$tract_fips <- substr(bg_vars$geoid, 1, 11)

bg_vars_ruca <- merge(bg_vars, ruca, by.x = "tract_fips", by.y = "tractfp")
bg_vars_ruca <- bg_vars_ruca[!duplicated(bg_vars_ruca$geoid.x), ]

bg_vars_ruca$code <- as.factor(bg_vars_ruca$code)
bg_vars_ruca$code <- relevel(as.factor(bg_vars_ruca$code), ref = "metro")

bg_vars_ruca$rural_urban <- if_else(bg_vars_ruca$code %in% c("metro", "micro"),
                                    "urban", "rural")
bg_vars_ruca$rural_urban <- relevel(as.factor(bg_vars_ruca$rural_urban), ref = "urban")
ruca$code <- as.factor(ruca$code)
ruca$code <- relevel(as.factor(ruca$code), ref = "metro")

ruca$rural_urban <- if_else(ruca$code %in% c("metro", "micro"),
                                    "urban", "rural")
ruca$rural_urban <- relevel(as.factor(ruca$rural_urban), ref = "urban")
#bg_vars_ruca$rural_urban <- ifelse(bg_vars_ruca$rural_urban == "urban", 1, 0)

summary(bg_vars_ruca$rural_urban)

aggregate(ruca$total, by = list(ruca$rural_urban), FUN = max)
names(bg_vars_ruca)

tableone::CreateTableOne(vars = c("total.y", "hispanic_p.y", "white_p.y", "black_p.y",
                                  "aian_p.y", "asian_p.y", "nhpi_p.y", "other_p.y", 
                                  "tom_p.y"),
                         strata = c("rural_urban"),
                         data = bg_vars_ruca)
table(ruca$ruca_code, ruca$rural_urban)
library(moments)

plot(urban)

library(tmap)
tm_shape(bg_vars_ruca) +
  tm_polygons(col = "rural_urban", lwd = 0) +
  tm_shape(states)


#### monitors
monitors_imp <- read.csv("/Users/brenna/Documents/School/GEOG 6960/monitors_imp.csv")
monitors <- read.csv("/Users/brenna/Documents/School/GEOG 6960/final project/aqs_monitors-2.csv")
names(monitors) <- tolower(names(monitors))

monitors$state.code <- str_pad(monitors$state.code, 2, pad = "0")
monitors$county.code <- str_pad(monitors$county.code, 3, pad = "0")
monitors$site.number <- str_pad(monitors$site.number, 4, pad = "0")
monitors$geoid <- paste(monitors$state.code, monitors$county.code, sep = "")
monitors$site_id <- paste(monitors$state.code, monitors$county.code, 
                          monitors$site.number, sep = "")
monitors$lat_long <- paste0(monitors$latitude, monitors$longitude)

# exclude non-continental and missing geolocation
monitors <- monitors %>%
  filter(!state.name %in% c(#"Alaska", "Puerto Rico", "Hawaii",
    "Virgin Islands", "County Of Mexico")) %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude))
monitors$long <- monitors$longitude
monitors$lat <- monitors$latitude

aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

monitors_sf <- st_as_sf(monitors, coords = c("longitude", "latitude"), 
                        crs = 4326, agr = "constant")
monitors_sf <- st_transform(monitors_sf, st_crs(aea))
#monitors_sf <- monitors#st_as_sf(monitors, coords = c("longitude", "latitude"), 
#crs = "EPSG:5070", agr = "constant")


#criteria pollutants only
monitors_criteria <- monitors_sf %>%
  filter(parameter.code %in% c(42101, 14129, 85129,
                               42602, 44201, 81102,
                               88101, 42401) |
           grepl("PM2.5", parameter.name) |
           grepl("PM10", parameter.name))
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 42101] <- "carbon monoxide"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code %in% c(14129, 85129)] <- "lead"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 42602] <- "nitrogen dioxide"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 44201] <- "ozone"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 81102] <- "pm10"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 88101] <- "pm25"
monitors_criteria$criteria_pollutant[monitors_criteria$parameter.code == 42401] <- "sulfur dioxide"

# no duplicate points
monitors_criteria <- monitors_criteria[!duplicated(monitors_criteria$lat_long), ]

