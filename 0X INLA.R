

library(sf)
library(INLA)
library(spatstat)
library(raster)
library(tidycensus)
install.packages("spatstat.utils")
library(spatstat.utils)
rmonth <- getData(name = "worldclim", var = "tmin", res = 10)
rcov <- mean(rmonth)


dpts <- SpatialPoints(mons[, c("Longitude", "Latitude")])

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
states_shp.win <- as.owin(states_shp)
states_poly <- owin2SP(states_shp.win)
# convert spatstat objects to sp classes

owin2Polygons <- function(x, id="1") {
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z))
}

owin2SP <- function(x) {
  stopifnot(is.owin(x))
  y <- owin2Polygons(x)
  z <- SpatialPolygons(list(y))
  return(z)
}
plot(states_poly)
plot(r)

resolution <- 100
r <- raster(states_poly, resolution = resolution)

bdy <- do.call(cbind, monitors.ppp$window$bdry[[1]])

max.edge <- diff(range(st_coordinates(mons)[,1]))/(3*5)
mesh <- inla.mesh.2d(loc.domain = bdy, max.edge = max.edge*1.1,
                     offset = c(1, 1))
pts <- as.matrix(coords(monitors.ppp))
mesh_pts <- as.matrix(mesh$loc[, 1:2])
allpts <- rbind(pts, mesh_pts)
plot(mesh)

nv <- mesh$n
n <- nrow(pts)

# create SPDE
mon_spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                                prior.range = c(50, 0.9), # P(range < 50) = 0.9
                                prior.sigma = c(1, 0.01) # P(sigma > 10) = 0.01
                                )
library(rgeos)
library(ggvoronoi)

rescale(tiles, 1000, "km")

tiles <- tile.list(deldir(pts))
tiles <- SpatialPolygons(Polygons(tiles))
triangle <- triang.list(deldir(pts))

polys = SpatialPolygons(
  lapply(1:length(triang.list(deldir(pts))),
         function(i){
           Polygons(
             list(
               Polygon(triang.list(deldir(pts))[[i]][c(1:3,1),c("x","y")])
             ),ID=i)
         }
  )
)
SpatialPolygonsDataframe(triangle)
str(polys)
tiles[, pt]
bdy_sp <- SpatialPolygons(list(Polygons(list(Polygon (bdy)),
                                        ID = "1")))


fun <- function(x, y) {
  SpatialPolygonsDataFrame(x, y, match.ID = F)
}

library(SDraw)

install.packages("/Users/brenna/Downloads/SDraw_2.1.13.tar", type = "source")
library(purrr)
triangle_df <- map_df(triangle, ~as.data.frame(t(.)))
head(triangle_df)
gIntersects(tiles[1, ], bdy_sp)
# compute weights
w <- sapply(1:length(tiles), function(p) {
  aux <- tiles_df[p, ]
  if(gIntersects(aux, bdy_sp)) {
    return(gArea(gIntersection(aux, bdy_sp)))
  } else {
    return(0)
  }
})

install_github("tmcd82070/SDraw")
library(SDraw)
install.packages( c("spsurvey", "rgeos", "sp", "deldir"), repos="http://cran.r-project.org")

mytiles <- deldir::voronoi.polygons(SpatialPoints(mesh$loc[, 1:2]))


pts_df <- data.frame(pts)
us.voronoi <- voronoi_polygon(data = pts_df,
                                x = "x", y= "y",
                                outline = bdy)
# us.voronoi <- fortify_voronoi(us.voronoi)
# 
# ggplot(us, aes(x = long, y = lat)) +
#   geom_polygon() +
#   geom_point(pts_df, aes(x = x, y = y))

library(ggmap)

us <- map_data("usa")
us <- us[us$region == "main", ]




nrow <- nrow(r)
## [1] 31
ncol <- ncol(r)
## [1] 33
nrow*ncol
## [1] 1023
#We initially set to 0 the values of all the raster cells by using r[] <-0. Then, we use cellFromXY() to obtain the number of sloths in each of the cells, and assign these counts to each of the cells of the raster.
r[] <- 0
plot(r)
tab <- table(cellFromXY(r, dpts))
r[as.numeric(names(tab))] <- tab
#Finally, we convert the raster r to a SpatialPolygonsDataFrame object called grid using rasterToPolygons(). This grid will be used to fit the model with the R-INLA package.
grid <- rasterToPolygons(r)

grid <- grid[as.vector(t(matrix(1:nrow(grid), nrow = ncol, ncol = nrow))), ]
grid$id <- 1:nrow(grid)
grid$Y <- grid$layer
grid$cellarea <- resolution*resolution

grid$cov <- extract(rcov, coordinates(grid))

gridmap <- raster::intersect(grid, map)
grid <- grid[grid$id %in% gridmap$id, ]

summary(grid)

indNA <- which(is.na(grid$cov))
indNA

grid$cov[indNA] <- grid$cov[indNA+1]

library(rgeos)
gridborder <- gUnaryUnion(grid)


grid$id2 <- grid$id
formula <- Y ~ 1 + cov +
  f(id, model="rw2d", nrow = nrow, ncol = ncol) +
  f(id2, model="iid")

res <- inla(formula, family = "poisson", data = grid@data,
            E = cellarea, control.predictor = list(compute = TRUE))

