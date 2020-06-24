# terrestrialSDM
# #############
# Purpose: Function to run a species distribution model using GBIF distribution
# data and WorldClim, ProtectedAreas, OpenStreetMap, SRTM environmental data.
# Author: RS-eco
# R version and packages: R 3.2.4 Revised (2016-03-16 r70336) -- "Very Secure Dishes"
# #############

terrestrialSDM <- function(species="Equus burchellii", limit = 50000, download_sp = TRUE,
                           output_sp = TRUE, plot_sp = TRUE, rm.outlier = TRUE,
                           subset_sp = FALSE, datatype="worldclim", var = "bio", res=2.5,
                           download_env = TRUE, model="GAM",
                           path = "/home/mabi/Documents/03 Wissenschaft/Data/",
                           html_output = FALSE){

  # Get data of species
  if(file.exists(paste0(path, "GBIF/", species, "/", species, ".mif"))){
    species_data <- readOGR(paste0(path, "GBIF/", species, "/", species, ".mif"), layer = species, verbose=FALSE)
  } else{
    # Download species location data from gbif
    species_data <- occ_search(taxonKey=name_backbone(name=species)$speciesKey, return="data",
                          fields=c("species", "year", "month", "decimalLatitude",
                                   "decimalLongitude"), limit= limit)

    # Discard data with errors in coordinates
    species_data <- species_data[complete.cases(species_data[,c("decimalLongitude", "decimalLatitude")]),]

    # Set spatial coordinates
    coordinates(species_data) <- c("decimalLongitude", "decimalLatitude")

    # Define spatial projection
    proj4string(species_data) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

    # Save species records in mif-format (preserves full column names)
    writeOGR(species_data, paste0(path, "GBIF/", species), species, driver="MapInfo File",
             dataset_options="FORMAT=MIF", overwrite=TRUE)
  }

  # Transform species occurrence data to same projection as country shapefile
  species_data <- spTransform(species_data, CRS="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

  # Get high resolution country data
  data(countriesHigh)

  # Remove species data at sea
  country_per_point <- over(species_data, countriesHigh)
  species_data <- species_data[!is.na(country_per_point$SOV_A3),]
  rm(country_per_point)

  # Remove outliers
  species_data <- species_data[species_data$coords.x1 != outlier(species_data$coords.x1),]
  species_data <- species_data[species_data$coords.x2 != outlier(species_data$coords.x2),]

  # Remove species data without time information
  # species_data <- species_data[!is.na(species_data$year),]

  # Create dataframe with time spans of different environmental datasets
  timeframe <- data.frame(datatype = c("worldclim"), startyear = c("1950"),
                          endyear = c("2000"))

  #Subset species records to timeframe of environmental variables
  # species_data <- species_data[species_data$year > as.character(timeframe$startyear[timeframe$datatype == datatype]),]
  # species_data <- species_data[species_data$year <= as.character(timeframe$endyear[timeframe$datatype == datatype]),]

  # Plot species data
  ggplot() + geom_point(data = data.frame(species_data), aes(x = coords.x1, y = coords.x2, colour = "red")) +
    scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-180,180), expand=c(0,0)) +
    scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(-80,90), expand=c(0,0)) +
    geom_polygon(data=map_data(map="world"), aes(x=long, y=lat, group=group), fill="transparent", color="black")

  # Get extent of study area
  studyarea <- extent(species_data)

  # Load environmental data
  bio <- getData(datatype, var = var, res = res, lon = centerPoint(studyarea)$x,
                 lat = centerPoint(studyarea)$y, path = paste0(path, "worldclim/"),
                 download = download_env)
  altitude <- getData(datatype, var = "alt", res= res, path = paste0(path, "altitude/"),
                 download = download_env); names(alt) <- "alt"

  #Calculate terrain variables from altitude (aspect, slope)
  aspect <- terrain(alt, opt = "aspect", unit = "degrees", df = F); names(asp) <- "asp"
  slope <- terrain(alt, opt = "slope", unit = "degrees", df = F); names(slo) <-"slo"

  # Stack terrain variables
  env_data <- stack(altitude, aspect, slope, bio); rm(altitude, aspect, slope, bio)

  # Crop environmental data by extent of study area
  env_data <- crop(env_data, studyarea)

  # Transform temperature data to degree celsius
  env_data[[]] <- calc(env_data[[]], fun=function(x){x/10})
  # env_data[[]] <- calc(env_data[[]], fun=function(x){x/10})

  # Make sure environmental data has same projection as species data
  proj4string(env_data) <- projection(species_data)

  # Save environmental data to file
  # writeRaster(env, filename="env.grd", bandorder='BIL', overwrite=TRUE)

  # Select only species records for which environmental information is available
  species_data <- species_data[complete.cases(extract(env_data, species_data)),]

  # Select 10000 random background points
  set.seed(2)
  background <- randomPoints(env_data, 10000, species_data)

  # Save background as dataframe and assign 0 value
  background <- as.data.frame(background)
  background$presence <- 0

  #Select only one presence record in each cell of the environmental layer
  presence <- gridSample(species_data, env_data, n=1)

  # Now we combine the presence and background points, adding a
  # column "species" that contains the information about presence (1)
  # and background (0)
  gam_data <- SpatialPointsDataFrame(rbind(presence, background),
                                     data = data.frame("species" = rep(c(1,0), c(nrow(presence), nrow(background)))),
                                     match.ID = FALSE, proj4string = CRS(projection(env_data)))

  # Add information of environmental conditions at point locations
  gam_data@data <- cbind(gam_data@data, extract(env_data, gam_data))
  gam_data <- as(gam_data, "data.frame")

  #Convert presence data into numbers
  presence <- as(species_data, "data.frame")
  colnames(presence) <- c("presence", "year", "month", "x", "y")
  presence$presence <- as.numeric(presence$presence)

  #Combine presence and background points
  presence <- presence[c(1,4,5)]
  maxent_data <- rbind(presence[, colnames(background)], background)
  coordinates(maxent_data) <- ~ x + y
  proj4string(maxent_data) <- proj4string(env_data)

  #Add information of environmental conditions at point locations
  maxent_data@data <- cbind(maxent_data@data, extract(env_data, maxent_data))

  # Check for collinearity of environmental data
  cm <- cor(gamdata[,c(2:5,8,17,18,20:23,26,28)], use = "complete.obs")

  # Plot correlation matrix
  plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"), main="Pearson correlation", mar=c(0.5,1.5,1.5,0.5))

  # Remove collinear variables

  ###

  # Run model
  model <- switch(model,
         RandomForest = randomForest(species ~., data=gam_data),
         SVM = svm(),
         GAM = gam(species ~ s(gam_data[2]) + s(gam_data[3]) + s(gam_data[4]) + s(gam_data[5]),
                   family="binomial", data=gam_data),
         MaxEnt = maxent(maxent_data[,-"presence"],
                         maxent_data[,"presence"]))

  # Model performance: crossvalidation
  set.seed(2)
  fold <- kfold(fulldata, k = 5, by = fulldata)
  fulldata$cv_pred <- NA # The variable cv_pred will contain the cross-validated predictions
  for (i in unique(fold)) {
    traindata <- fulldata[fold != i, ]
    testdata <- fulldata[fold == i, ]
    cv_model <- gam(species ~ s(alt) + s(slo) + s(bio1) + s(bio2), family="binomial", data=traindata)
    fulldata$cv_pred[fold == i] <- predict(cv_model, testdata, type="response")
  }

  #Calculate Somer's Dxy Rank Correlation and the corresponding receiver operating characteristic curve area C.
  round(somers2(fulldata$cv_pred, fulldata$species), 2)

  #Calculate performance for whole dataset
  round(somers2(predict(gammodel, fulldata, args='outputformat=raw'),
                fulldata$species), 2)

  set.seed(2)
  fold <- kfold(fulldata_ter_maxent, k = 5, by = fulldata_ter_maxent$presence)
  fulldata_ter_maxent$cv_pred_me <- NA # The variable cv_pred will contain the cross-validated predictions
  for (i in unique(fold)) {
    traindata <- fulldata_ter_maxent[fold != i, ]
    testdata <- fulldata_ter_maxent[fold == i, ]
    cv_model <- maxent(traindata[,c("alt", "bio1", "bio2", "bio7")], traindata[,"presence"])
    fulldata_ter_maxent$cv_pred_me[fold == i] <- predict(cv_model, testdata, args='outputformat=raw')
  }

  #Calculate Somer's Dxy Rank Correlation and the corresponding receiver operating characteristic curve area C.
  round(somers2(fulldata_ter_maxent$cv_pred_me, fulldata_ter_maxent$presence), 2)

  #Calculate performance for whole dataset
  round(somers2(predict(maxentmodel_ter, fulldata_ter_maxent, args='outputformat=raw'), fulldata_ter_maxent$presence), 2)

  #Just select presence records
  presence_ter_maxent <- fulldata_ter_maxent[fulldata_ter_maxent$presence==1,]

  #Plot total environmental data vs. species data
  par(mfrow=c(2,2),  mar=c(3,4,2,2))
  varnames <- c("alt", "bio1", "bio2", "bio7")
  env_data <- as.data.frame(env[[varnames]])
  axislabels <- c("altitude", "mean annual temperature", "mean diurnal range", "temperature annual range")
  for (i in 1:4){
    j <- varnames[i]
    k <- axislabels[i]
    boxplot(env_data[[j]], presence_ter_maxent[[j]], names=c("Global", "Puffin"), col=c("darkgray", "green3"), ylab=k, cex.lab=1.5, cex.axis=1.5, notch=TRUE)
  }

  #Variable importance
  gamimp_ter <- varImp(gammodel_ter, varnames_ter, fulldata_ter_gam)
  par(mar=c(3,4,2,2))
  barplot(100*gamimp_ter/sum(gamimp_ter), ylab = "Variable importance (%)", ylim=c(0,40), col="green3", space=1)
  box()
  plot(maxentmodel_ter, pch=16, col="green3")

  #Response functions
  plot(gammodel_ter, select=1, ylim=c(-1600,1600))
  response(maxentmodel_ter, var="alt", expand = 0, fun = function(x, y) predict(x, y, args='outputformat=raw'), ylim = c(0, 0.00001))

  #Prediction map
  gammap_ter <- predict(env, gammodel_ter, type = "response")
  maxentmap_ter <- predict(maxentmodel_ter, env, args='outputformat=raw')
  levelplot(gammap_ter, margin=FALSE, col.regions = rev(terrain.colors(255)),
            main="GAM", colorkey=list(space="right"),
            panel=panel.levelplot.raster) +
  layer(sp.polygons(countriesHigh, lwd=0.5, col="black"))
}
