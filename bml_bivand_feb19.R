## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
library(osmdata)
library(sf)
bbox <- opq(bbox = 'bergen norway')
byb0 <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'light_rail'))$osm_lines
tram <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'tram'))$osm_lines
byb1 <- tram[!is.na(tram$name),]
o <- intersect(names(byb0), names(byb1))
byb <- rbind(byb0[,o], byb1[,o])

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
library(mapview)
mapview(byb)


## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
all(st_is(byb, "XY"))
str(st_coordinates(st_geometry(byb)[[1]]))

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
library(elevatr)
elevation <- get_elev_raster(as(byb, "Spatial"), z = 14)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
mapview(elevation, col=terrain.colors)

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
library(rgdal)
fl <- "../ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"
LUC <- readGDAL(fl, offset=c(10400,66450), region.dim=c(400, 700),
  output.dim=c(400, 700))

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
mapview(LUC[, , c(1,13,24)])

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
library(sp)
str(slot(as(st_geometry(byb), "Spatial"), "lines")[[1]])

## ---- echo = TRUE, mysize=TRUE, size='\\tiny'----------------------------
strwrap(st_as_text(st_geometry(byb)[[1]]))

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
elevation
slot(LUC, "grid")

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
library(stars)
LUC_stars <- read_stars(fl, proxy=TRUE)
LUC_stars
LUC_stars1 <- st_as_stars(LUC_stars[byb,,,])

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
legs <- read.csv("../ESACCI-LC-Legend.csv", header = TRUE, sep = ";")
plot(LUC_stars1, band=24, rgb = legs, main="")

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------
library(raster)
rst1 <- raster("../LUC3.tif", band=1)
rst3 <- raster("../LUC3.tif", band=3)
rst1 <- as.factor(rst1)
rst3 <- as.factor(rst3)
rst <- stack(rst1, rst3)
hex <- grDevices::rgb(legs$R, legs$G, legs$B, maxColorValue = 255,
 names = legs$NB_LAB)
hex <- hex[names(hex) %in% intersect(as.character(levels(rst)[[1]]$ID),
 as.character(levels(rst)[[2]]$ID))]
mapview(rst, col.regions = hex)
#library(leaflet.opacity)
#library(leaflet)
mapview(rst, col.regions = hex, alpha=1)#@map %>% addOpacitySlider("rst")

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
(WKT <- st_crs(byb))
strwrap(gsub(",", ", ", st_as_text(WKT)))
byb_utm <- st_transform(byb, crs=32632)
st_crs(byb_utm)

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
head(st_coordinates(st_geometry(byb)[[1]]), n=1)
system(paste("echo 5.333375 60.30436 |", 
  "proj +proj=pipeline +ellps=WGS84", 
  "+step +init=epsg:32632"), intern=TRUE)
head(st_coordinates(st_geometry(byb_utm)[[1]]), n=1)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, cache=TRUE, size='\\tiny'----
#!!!!! DIFFERENT SESSION !!!!!
#library(plumber)
#r <- plumb(system.file("plumber/server.R", package = "stars"))
#r$run(port=8000)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, cache=TRUE, size='\\tiny'----
source(system.file("plumber/client.R", package = "stars"),,
 echo = TRUE)
plot(xx)

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny'----------------
bike_fls <- list.files("../bbs")
trips0 <- NULL
for (fl in bike_fls) trips0 <- rbind(trips0,
  read.csv(file.path("../bbs", fl), header=TRUE))
trips0 <- trips0[!(trips0[,9]==84),]
trips <- cbind(trips0[,c(1, 4, 2, 9)], data.frame(count=1))
from <- unique(trips0[,c(4,5,7,8)])
names(from) <- substring(names(from), 7)
to <- unique(trips0[,c(9,10,12,13)])
names(to) <- substring(names(to), 5)
stations0 <- st_as_sf(merge(from, to, all=TRUE),
  coords=c("station_longitude", "station_latitude"))
stations <- aggregate(stations0, list(stations0$station_id),
  head, n=1)
suppressWarnings(stations <- st_cast(stations, "POINT"))
st_crs(stations) <- 4326
od <- aggregate(trips[,-(1:4)], list(trips$start_station_id,
  trips$end_station_id), sum)
library(stplanr)
od_lines <- od2line(flow=od, zones=stations, zone_code="Group.1",
  origin_code="Group.1", dest_code="Group.2")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, cache=TRUE, size='\\tiny'----
mapview(od_lines, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)
## Sys.setenv(CYCLESTREET="xXxXXxXxXxXxXxXxX")
## od_routes <- line2route(od_lines, "route_cyclestreet",
##  plan = "fastest")
od_routes <- readRDS("../od_routes.rds")
mapview(od_routes, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny', message=FALSE, warning=FALSE----
library(geoR)
data(SIC)
library(sp)
sic.100SP <- SpatialPointsDataFrame(SpatialPoints(sic.100$coords),
 data=data.frame(precip=sic.100$data, altitude=sic.100$altitude))
sic.allSP <- SpatialPointsDataFrame(SpatialPoints(sic.all$coords),
 data=data.frame(precip=sic.all$data, altitude=sic.all$altitude))
names(sic.allSP) <- c("precip", "altitude")
sic.367SP <- sic.allSP[which(!rownames(sic.all$coords) %in% 
  rownames(sic.100$coords)),]
library(automap)
aK <- autoKrige(formula=precip ~ altitude, input_data=sic.100SP,
  new_data=sic.367SP)
res <- aK$krige_output

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny', message=FALSE, warning=FALSE----
library(rgeos)
dist0 <- as.data.frame(gDistance(sic.100SP["precip"],
  sic.100SP["precip"], byid=TRUE))
names(dist0) <- paste("layer", names(dist0), sep=".")
dist1 <- as.data.frame(gDistance(sic.100SP["precip"],
  sic.367SP["precip"], byid=TRUE))
names(dist1) <- paste("layer", names(dist1), sep=".")
rm.precip <- cbind(as.data.frame(sic.100SP)[c("precip", "altitude")],
  dist0)
rm.precip1 <- cbind(as.data.frame(sic.367SP)[c("altitude")], dist1)
dn0 <- paste(names(dist0), collapse="+")
fm0 <- as.formula(paste("precip ~ altitude +", dn0))
library(ranger)
m.precip <- ranger(fm0, rm.precip, quantreg=TRUE, num.trees=150,
  seed=1, keep.inbag=TRUE)
res$rf_pred <- predict(m.precip, rm.precip1,
  type="response")$predictions
res$rf_sd1 <- predict(m.precip, rm.precip1, type="se",
  se.method="infjack")$se
res$rf_sd2 <- predict(m.precip, rm.precip1, type="se",
  se.method="jack")$se

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
spplot(res, zcol=c(1,4))
spplot(res, zcol=c(3,5,6))

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny', message=FALSE, warning=FALSE----
library(mlr)
data(spatial.task)
learner.rf = makeLearner("classif.ranger", predict.type = "prob")
resampling = makeResampleDesc("SpRepCV", fold = 5, reps = 5)
set.seed(123)
out_sp = resample(learner = learner.rf, task = spatial.task,
  resampling = resampling, measures = list(auc))
learner.rf = makeLearner("classif.ranger", predict.type = "prob")
resampling = makeResampleDesc("RepCV", fold = 5, reps = 5)
set.seed(123)
out_nsp = resample(learner = learner.rf, task = spatial.task,
  resampling = resampling, measures = list(auc))

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------
library(cowplot)
plots = createSpatialResamplingPlots(spatial.task,
 list("SpRepCV" = out_sp, "RepCV" = out_nsp), crs = 32717,
 repetitions = 1, x.axis.breaks = c(-79.075),
 y.axis.breaks = c(-3.975))
plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 2,
 labels = plots[["Labels"]], label_size = 8)

## ---- echo = TRUE, mysize=TRUE, cache=TRUE, size='\\tiny', message=FALSE, warning=FALSE----
library(lagsarlmtree) # development version
data(elect80, package="spData")
res <- lagsarlmtree(pc_turnout ~ pc_college + pc_homeownership +
  pc_income | pc_college + pc_homeownership + pc_income,
  data=as(elect80, "data.frame"), listw=elect80_lw, method="Matrix")
coef(res, model = "rho")

## ---- echo = FALSE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------
plot(res, tp_args = list(which = 1))
dums <- res$lagsarlm$tarX[,1:12]
vals <- as.numeric(substring(colnames(dums), 7))
elect80$fdums <- as.factor(dums %*% vals)
library(RColorBrewer)
mapview(elect80, zcol="fdums", col.regions=brewer.pal(12, "Paired"))

## ---- echo = TRUE, mysize=TRUE, size='\\tiny', message=FALSE, warning=FALSE----
library(openeo) # "master" branch
euracHost <- "http://saocompute.eurac.edu/openEO_0_3_0/openeo/"
eurac <- connect(host = euracHost,disable_auth = TRUE)
pgb <- eurac %>% pgb()
bb <- list(west=10.98,east=11.25,south=46.58,north=46.76)
tt <- c("2017-01-01T00:00:00Z","2017-01-31T00:00:00Z")
task <- pgb$collection$S2_L2A_T32TPS_20M %>%
  pgb$filter_bbox(extent=bb) %>%
  pgb$filter_daterange(extent=tt) %>%
  pgb$NDVI(red="B04",nir="B8A") %>%
  pgb$min_time()
tf <- tempfile()
raster <- eurac %>% preview(task=task, format="GTiff",
  output_file=tf)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'----------------
mapview(raster(raster))

