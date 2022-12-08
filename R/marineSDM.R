#' Run SDM for a marine species
#'
#' Function to run a species distribution model using GBIF distribution data
#' and Bio-Oracle, MarSpec, WOA, WDPA and GEBCO environmental data.
#'
#' @param species A Latin name of a species.
#' @param limit Number of data records to obtain from GBIF
#' @param download_sp logical if gbif data should be downloaded
#' @param path_sp character. Specifying location of GBIF data
#' @param output_sp logical. Should GBIF data be returned
#' @param plot_sp logical. Should GBIF data be plotted.
#' @param datatype factor. One of MARSPEC, Bio-Orcale, ... 
#' @param download logical. Should environmental data be downloaded
#' @param path character. Path where environmental data is located
#' @param model factor. Which model type should be used, RandomForest.
#' @return species distribution of this species
#' @examples
#' marine_sdm(species="Thunnus alalunga", env="", model="RandomForest")
#' @export
marine_sdm <- function(species="Thunnus alalunga", limit = 50000, download_sp = TRUE,
                       path_sp = "/home/mabi/Documents/03 Wissenschaft/Data/GBIF/", 
                       output_sp = TRUE, plot_sp = TRUE,
                       datatype="MARSPEC", download = TRUE, path="",
                       model="RandomForest"){
  ###Bio-ORACLE data

  ##Downloading 70N-70S real values of the Bio-Oracle dataset
  #(see http://www.oracle.ugent.be/download.html for more information)
  #filepath <- "http://www.oracle.ugent.be/DATA/70_70_RV/BioOracle_7070RV.rar"
  #download.file(filepath, filedir)

  ##Remove file.path from workspace
  #rm(filepath)

  ##Unzipping rar file - ???
  unzip()

  #Load Bio-Oracle data
  bio_oracle <- stack(list.files("C:/Users/Admin/Documents/03 Wissenschaft/Data/
                                 BioOracle_7070RV/", pattern="*.asc", full.names=T))
  #bio_oracle <- stack(list.files("G:/03 Wissenschaft/Data/BioOracle_7070RV/",
  #pattern="*.asc", full.names=T))

  proj4string(bio_oracle) <- crs.geo

  #Load bathymetry data
  library(ncdf)
  depth <- raster("C:/Users/Admin/Documents/03 Wissenschaft/Data/
                  RN-6759_1433160789681/GEBCO_2014_1D.nc")

  #Calculate terrain variables from bathymetry (aspect, slope)
  asp <- terrain(depth, opt = "aspect", unit = "degrees", df = F)
  slo <- terrain(depth, opt = "slope", unit = "degrees", df = F)

  #Stack terrain data
  terrain <- stack(depth, asp, slo)
  names(terrain[[1]]) <- "depth"
  rm(depth, asp, slo)

  #Adjust extent and resolution to bio-oracle dataset
  terrain <- crop(terrain, bio_oracle)
  terrain <- resample(terrain, bio_oracle[[1]])
  terrain <- raster::mask(terrain, bio_oracle[[1]])

  #Calculate distance to coast
  coast_raster <- rasterize(countriesLow, terrain[[1]])
  coast_raster[coast_raster > 0] <- 1
  dist_coast <- distance(coast_raster)/1000
  dist_coast <- raster::mask(dist_coast, terrain[[1]])
  rm(coast_raster)

  #Stack terrain data and distance to coast
  terrain <- stack(terrain, dist_coast)
  names(terrain[[4]]) <- "distance"
  rm(dist_coast)

  #Save terrain raster
  writeRaster(terrain, filename="terrain.grd", bandorder='BIL', overwrite=TRUE)

  #Load terrain raster
  terrain <- stack("C:/Users/Admin/Documents/07 Bayreuth University/GCE/2nd Semester/
                 B5 - Global Change Impacts on Species Distributions/Puffin/terrain.grd")
  proj4string(terrain) <- crs.geo

  #Stack bio-oracle and terrain data
  marine <- stack(terrain, bio_oracle)
  rm(bio_oracle, terrain)

  #Projection of data
  proj4string(marine) <- crs.geo

  #Select only species records for which environmental information is available
  presence_mar <- Fratercula.arctica[complete.cases(
    extract(marine, Fratercula.arctica)),]

  #Selecting 10000 random background points
  set.seed(2)
  background <- randomPoints(marine, 10000, presence_mar)

  #Save background as dataframe and assign 0 value
  background_df <- as.data.frame(background)
  background_df$presence <- 0

  #Select only one presence record in each cell of the environmental layer
  presence <- gridSample(presence_mar, marine, n=1)

  # Now we combine the presence and background points, adding a
  # column "species" that contains the information about presence (1)
  # and background (0)
  fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                     data = data.frame("species" = rep(c(1,0),
                                                                       c(nrow(presence), nrow(background)))),
                                     match.ID = FALSE,
                                     proj4string = CRS(projection(marine)))

  # Add information of environmental conditions at point locations
  fulldata@data <- cbind(fulldata@data, extract(marine, fulldata))
  fulldata <- as(fulldata, "data.frame")

  #Convert presence data into numbers
  presence <- as.data.frame(presence_mar)
  colnames(presence) <- c("presence", "year", "month", "x", "y")
  presence$presence <- as.numeric(presence$presence)

  #Combine presence and background points
  presence <- presence[c(1,4,5)]
  fulldata <- rbind(presence[, colnames(background_df)], background_df)
  coordinates(fulldata) <- ~ x + y
  proj4string(fulldata) <- proj4string(env)
  rm(presence, background_df)

  #Add information of environmental conditions at point locations
  fulldata@data <- cbind(fulldata@data, extract(marine, fulldata))

  varnames_mar <- c("sstmean", "sstrange", "depth", "distance")

  ## Generalized additive models
  gammodel_mar <- gam(species ~ s(sstmean) + s(sstrange)
                      + s(depth) + s(distance),
                      family="binomial", data=fulldata_mar_gam)
  summary(gammodel_mar)

  #Model performance: crossvalidation
  set.seed(2)
  fold <- kfold(fulldata_mar_gam, k = 5,
                by = fulldata_mar_gam$species)
  fulldata_mar_gam$cv_pred <- NA
  # The variable cv_pred will contain the cross-validated predictions
  for (i in unique(fold)) {
    traindata <- fulldata_mar_gam[fold != i, ]
    testdata <- fulldata_mar_gam[fold == i, ]
    cv_model <- gam(species ~ s(sstmean) + s(sstrange)
                    + s(depth) + s(distance),
                    family="binomial", data=traindata)
    fulldata_mar_gam$cv_pred[fold == i] <- predict(cv_model,
                                                   testdata,
                                                   type="response")
  }

  #Calculate Somer's Dxy Rank Correlation and
  #the corresponding receiver operating characteristic curve area C.
  round(somers2(fulldata_mar_gam$cv_pred, fulldata_mar_gam$species), 2)

  #Calculate performance for whole dataset
  round(somers2(predict(gammodel_mar, fulldata_mar_gam,
                        args='outputformat=raw'), fulldata_mar_gam$species), 2)
  #If discrepancy is too big, you have to reduce the complexity of the model

  #SDM using Maxent

  #The following code assumes that the column
  #with the species information is in the first position
  maxentmodel_mar <- maxent(fulldata_mar_maxent[,c("sstmean", "sstrange",
                                                   "depth", "chlomean")],
                            fulldata_mar_maxent[,"presence"])
  maxentmodel_mar

  #Model performance: crossvalidation
  set.seed(2)
  fold <- kfold(fulldata_mar_maxent, k = 5, by = fulldata_mar_maxent$presence)
  fulldata_mar_maxent$cv_pred_me <- NA
  # The variable cv_pred will contain the cross-validated predictions
  for (i in unique(fold)) {
    traindata <- fulldata_mar_maxent[fold != i, ]
    testdata <- fulldata_mar_maxent[fold == i, ]
    cv_model <- maxent(traindata[,c("sstmean", "sstrange",
                                    "depth", "chlomean")],
                       traindata[,"presence"])
    fulldata_mar_maxent$cv_pred_me[fold == i] <- predict(cv_model,
                                                         testdata,
                                                         args='outputformat=raw')
  }

  #Calculate Somer's Dxy Rank Correlation and
  #the corresponding receiver operating characteristic curve area C.
  round(somers2(fulldata_mar_maxent$cv_pred_me,
                fulldata_mar_maxent$presence), 2)

  #Calculate performance for whole dataset
  round(somers2(predict(maxentmodel_mar, fulldata_mar_maxent,
                        args='outputformat=raw'),
                fulldata_mar_maxent$presence), 2)
  #If discrepancy is too big, you have to reduce the complexity of the model

}
