
#### Libraries ####
library(raster) # Will load "sp" package as a dependency
library(rgdal)
library(randomForest)

# ETH Cities and Towns
eth.places <- raster::shapefile("data/places/places.shp")
# Population data
Ethpop15 <- raster("data/Ethiopia_100m_Population/ETH15adjv5.tif")

#### Create Maize Prices data ####
output.dir.raster <- "output/priceRasters"
files.csv <- list.files("output/ETHPriceData/", full.names = TRUE)



for (file.csv in files.csv) {
  

maize.price.all <- read.csv(file.csv, as.is = TRUE)
maize.price.column <- "Unmilled.Maize.Price.Birr.KG" # "Milled.Maize.Price.Birr.KG"
Longitude.column <- "Longitude"
Latitude.column <- "Latitude"
maize.price.all <- maize.price.all[complete.cases(maize.price.all[ , 
                                                                   c(Longitude.column, Latitude.column)]),
                                   ]
head(maize.price.all, 5) 

maize.price1 <- maize.price.all[!duplicated(maize.price.all[,c(Longitude.column, Latitude.column)]),]

sumryStatFun <- function(maize.price.row, maize.price.all, Longitude.column, Latitude.column, maize.price.column) {
  lati <- as.numeric(maize.price.row[Latitude.column])
  long <- as.numeric(maize.price.row[Longitude.column])
  location.prices <- maize.price.all[round(maize.price.all[, Longitude.column], 3) == round(long, 3)  & 
                                     round(maize.price.all[, Latitude.column ], 3) == round(lati, 3), maize.price.column]
  # if (!length(location.prices))print(maize.price.row)
  maize.price.row[maize.price.column] <- median(as.numeric(location.prices), na.rm = TRUE)
  # print(maize.price.row)
  # print(location.prices)
  return(maize.price.row)
}
maize.price <-  data.frame(t( apply(maize.price1, 1, sumryStatFun, 
                                    maize.price.all = maize.price.all,
                                    Longitude.column = Longitude.column,
                                    Latitude.column = Latitude.column,
                                    maize.price.column = maize.price.column
                                    )
                              )
                           )
                  
maize.price[, Longitude.column] <- as.numeric(levels(maize.price[, Longitude.column])[maize.price[, Longitude.column]])
maize.price[, Latitude.column]  <- as.numeric(levels(maize.price[, Latitude.column])[maize.price[, Latitude.column]])
maize.price[, maize.price.column] <- as.numeric(levels(maize.price[, maize.price.column])[maize.price[, maize.price.column]])

#### Create Maize Prices SpatialDataFrame ####
# wgs84.prj: projection for coordinates in prices csv
wgs84.prj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
maize.price.shp <- SpatialPointsDataFrame(
    coords = data.frame(maize.price[, Longitude.column], maize.price[, Latitude.column] ),
    data = maize.price,
    proj4string = wgs84.prj
  )

#### Get Ethiopia national boundary and plot Maize prices
ETH_natbnd <- getData('GADM', country = 'ETH', level = 0)
plot(ETH_natbnd, axes = TRUE, main = "Locations With Known Maize Prices")
plot(maize.price.shp,
     pch = 20,
     col = "Red",
     add = TRUE)

#### Transform maize prices to laea projection(projection used in analysis)
laea.prj <-
  CRS(
    "+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  )
maizepriceshp_laea <- spTransform(maize.price.shp, laea.prj)


##### Create rastr stack ####
rasterstack <- stack()
rasterstack1 <- stack()
crs(rasterstack) <- laea.prj # raster::crs differs with sp::CRS
crs(rasterstack1) <- laea.prj # raster::crs differs with sp::CRS
rasterlist <- list.files(path = "./rasters",
                         pattern = "*.tif$",
                         full.names = TRUE) # Character vector of relative filepaths
for (rasterpath in rasterlist) {
  rasterfile <- raster(rasterpath)
  print(rasterpath)
  print(crs(rasterfile))
  
  tmp.stack <- try(addLayer(rasterstack, rasterfile), silent = TRUE)
  if (class(tmp.stack) == "try-error") {
    rasterfile <- projectRaster(rasterfile, rasterstack)
  }
  rasterstack <- addLayer(rasterstack, rasterfile)
  
}



###### b) Add Latitude and Longitude Rasters

refrenceraster <- rasterstack[[1]]

# Step 1
natbnd_laea <- spTransform(ETH_natbnd, laea.prj)
# Step 2
natbnd.raster <- rasterize(natbnd_laea, refrenceraster)
# Step 3
latitudes <-
  xFromCell(natbnd.raster, 1:length(natbnd.raster))
longitudes <-
  yFromCell(natbnd.raster, 1:length(natbnd.raster))
# Step 4
natbnd.raster.lati <- natbnd.raster.long <- natbnd.raster
values(natbnd.raster.lati) <- latitudes
values(natbnd.raster.long) <- longitudes
# Step 5
names(natbnd.raster.long) <- Longitude.column
names(natbnd.raster.lati) <- Latitude.column
rasterstack <- addLayer(rasterstack, natbnd.raster.long)
rasterstack <- addLayer(rasterstack, natbnd.raster.lati)

names(rasterstack)




# First, we prepare the model training data (response variable and prediction variables).
# Takes Time ~ 20 min
# Prediction variables
predict.vrbs1 <- list()
for (i in 1:length(names(rasterstack))) {
  print(names(rasterstack)[i])
  predict.vrbs1[[names(rasterstack)[i]]] <- extract(rasterstack[[i]], maizepriceshp_laea, buffer = 5000, small = TRUE, fun = mean )
  # break()
                                                 
}
predict.vrbs2 <- do.call(cbind, predict.vrbs1)
predict.vrbs <- predict.vrbs2[complete.cases(predict.vrbs2),]


# Responce variable
responce.vrbs <- maizepriceshp_laea@data[complete.cases(predict.vrbs2), maize.price.column]


##### 1. Tune The Forest
trf <- tuneRF(x = predict.vrbs, 
              y = responce.vrbs
              )

# We consider the prediction error
mintree <- trf[which.min(trf[, 2]), 1]


##### 2. Fit The Model
price.model <-
  randomForest(
    x = predict.vrbs,
    # Prediction variables
    y = responce.vrbs,
    # Responce variable
    mtry = mintree,
    # Number of variables in subset at each split
    importance = TRUE # Assess importance of predictors.
  )
plot(price.model)

varImpPlot(price.model)

#### (III) Price Prediction and Results Evaluation

##### 1. Price Prediction
# NB:Both the "stats" package(loaded as a randomForest dependency) and
# the "raster" package have a function called "predict" that can make predictions.
# Since we are dealing with spatial data, we add a prefix to the function name to
# ensure the "predict" function in the raster "package" is used.
spatial.prediction <-
  raster::predict(rasterstack,
                  # Prediction variable rasters
                  price.model) # Prediction  model)            # takes time!

# Get Tanzania Regions Shapefile
ETH_natbnd_1 <-
  getData('GADM', country = 'ETH', level = 1)
# Transform to laea projection
ETH_natbnd_1_laea <-
  spTransform(ETH_natbnd_1, laea.prj)
#  Plot the Predicted prices and the regional boundaries
plot(spatial.prediction, main = "Ethiopia, 2008 Predicted Maize Prices (ETB/KG)")
plot(ETH_natbnd_1_laea, add = TRUE)


##### 2. Prediction Evaluation

# Considering that the training data, is in point form as opposed to raster,
# We will make a non-spatial prediction using the predict function in the "stats" package
non.spatial.prediction <-
  stats::predict(price.model, predict.vrbs)
plot(
  responce.vrbs,
  non.spatial.prediction,
  col = 'blue',
  xlab = 'observed',
  ylab = 'predicted',
  xlim = c(min(responce.vrbs) - 1, max(responce.vrbs) + 1),
  ylim = c(min(non.spatial.prediction) - 1, max(non.spatial.prediction) + 1)
)
lm_line <-
  lm(
    non.spatial.prediction ~ responce.vrbs,
    data = data.frame(responce.vrbs,
                      non.spatial.prediction)
  )
abline(lm_line)
dir.create(file.path(output.dir.raster))
outpath1 <- gsub("[[:space:]]", "", basename(file.csv))
outpath <- gsub("[.]", "", basename(outpath1))
writeRaster(spatial.prediction, paste0(file.path(output.dir.raster,outpath), ".tif"))

}

# 3. Extract Price data in Ethiopia Cities/Towns
eth_places <- raster::extract( spatial.prediction, eth.places, 
                                 method = 'bilinear', buffer = 5000, 
                                 fun = median, sp=TRUE, 
                                 na.rm=TRUE
)

# 4. Extract Population data in Ethiopia Cities/Towns 
eth_places <- raster::extract( Ethpop15, eth.places, 
                               method = 'bilinear', buffer = 5000, 
                               fun = sum, sp=TRUE, 
                               na.rm=TRUE
)

# Select Cities and towns from places shapefile
  for (place in unique(eth_places$type)) {
  print(paste0(place, " : ", length(eth_places$type[eth_places$type == place])))
}

eth.citytown <- ethplaces[ethplaces$type == "city" | ethplaces$type == "town",]

dir.create("scratch", showWarnings = FALSE)
shapefile(eth.citytown, "scratch/EthCityTown.shp")

