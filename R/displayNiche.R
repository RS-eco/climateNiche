##' Plot the climatic niche of a species
#'
#' @description
#' This function extracts worldclim data for a given spatial object 
#' and displays the worldclim data for the spatial points.
#' 
#' @param data raster layer or stack of the presence of one or multiple species, 
#' also takes a list
#' @param lon Name of longitude column in data file, default x
#' @param lat Name of latitude column in data file, default y
#' @param name specifying which environmental data to use, currently only worldclim is supported.
#' @param res specifying the desired spatial resolution. Valid resolutions are 0.5, 2.5, 5, 10 (minutes of a degree).
#' @param variables specifying which variables to get, can be tmin, tmax and/or prec or bio.
#' @param tres specifying the desired temporal resolution. One of year, month, quarter.
#' @param path Character. Path name indicating where to store the data. Default is the current working directory.
#' @param extent specify extent of area considered
#' @param hist logical. specify if a histogram should be added to the plot.
#' @param map logical. specify if a map should be added to the plot.
#' @param download logical. download worldclim data or not. If download=FALSE the bioclimatic variables need to be provided with the data
#' that is inserted into the function and bioclimatic variables need to be named accordingly (bio1, bio2, ...).
#' @return plot of climatic niche of species
#' @examples
#' # Load library
#' library(climateNiche)
#' 
#' # Load House sparrow data
#' data(Passer_domesticus)
#' 
#' # Create plot of climatic niche
#' displayNiche(data=Passer_domesticus, variables=c("tmax", "prec"), tres="month")
#' 
#' # Load Great sparrow data
#' data(Passer_motitensis)
#' 
#' # Create plot of climatic niche
#' displayNiche(data=Passer_motitensis, variables=c("tmin", "tmax", "prec"), tres="quarter")
#' 
#' # Create list of the two species
#' Passer_spp <- list(Passer_domesticus, Passer_motitensis)
#' 
#' # Create plot of climatic niche
#' displayNiche(data=Passer_spp, variables=c("tmin", "tmax"))
#' 
#' # Load data of the Somali sparrow & the Asian desert sparrow
#' data(Passer_castanopterus)
#' data(Passer_zarudnyi)
#' 
#' # Create list of four species
#' Passer_spp4 <- list(Passer_castanopterus, Passer_domesticus, 
#'                     Passer_motitensis, Passer_zarudnyi)
#'
#' # Create plot of climatic niche
#' displayNiche(data=Passer_spp4, variables=c("tmin", "tmax", "prec"))
#' @export
displayNiche <- function(data, lon="x", lat="y", name="worldclim", res=10, 
                         variables=c("tmin", "tmax", "prec"), tres="year", 
                         path='', extent=NA, hist=FALSE, map=FALSE, download=TRUE){
  if(any(variables %in% c("bio2", "bio3", "bio4"))){
    stop("bio2, bio3 and bio4 are currently not supported.")
  }
  
  # Convert data to dataframe
  if(is(data, "list")){
    if(!is.null(names(data))){
      species <- names(data)
    } else{
      species <- sapply(data, function(x) names(x[[1]]))
    }
    data <- lapply(1:length(data), FUN=function(x){
      if(any(is(data[[x]]) %in% c("RasterLayer", "RasterStack", "RasterBrick"))){
        data <- data.frame(raster::rasterToPoints(data[[x]]))
        data$species <- species[x]
        colnames(data) <- c(lon, lat, "presence", "species")
      } else{
        data <- data[[x]]
        data$species <- species[x]
        data <- data %>% dplyr::select(tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                                            "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                                            "decimalLatitude", "Longitude", "Latitude", 
                                                            "long", "lat", "Long", "Lat")), 
                                       tidyselect::one_of(lon, lat), "species", 
                                       tidyselect::any_of(c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                                                            "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", 
                                                            "bio15", "bio16", "bio17", "bio18", "bio19")))
        data$presence <- 1
        colnames(data)[1:3] <- c(lon, lat, "species")
      }
      return(data)
    })
    data <- do.call("rbind", data)
    data$species <- factor(data$species, ordered=TRUE)
  } else if(any(is(data) %in% c("RasterLayer", "RasterStack", "RasterBrick"))){
    species <- names(data)
    data <- data.frame(raster::rasterToPoints(data))
    data$species <- factor(species, ordered=TRUE); rm(species)
  } else{
    data <- as.data.frame(data)
    data <- data %>% dplyr::select(tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                                        "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                                        "decimalLatitude", "Longitude", "Latitude", 
                                                        "long", "lat", "Long", "Lat")), tidyselect::one_of(lon, lat), 
                                   tidyselect::one_of(c("species", "SCIENTIFIC.NAME", "scientificName",
                                                        "scientificname", "name")),
                                   tidyselect::any_of(c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                                                                      "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", 
                                                                      "bio15", "bio16", "bio17", "bio18", "bio19")))
    data$presence <- 1
    colnames(data)[1:3] <- c(lon, lat, "species")
  }
  
  # Determine extent if not provided as input
  if(is.na(extent)){
    extent <- raster::extent(c(min(data[,c(lon)], na.rm=T), max(data[,c(lon)]), 
                               min(data[,c(lat)]), max(data[,c(lat)])))
  } else{
    if(any(is(extent) != "Extent")){
      extent <- raster::extent(extent)
    }
  }
  # Use bio variables when asking for tres=year (Check it is same as bio!!!)
  
  # Get environmental data
  if(download == TRUE){
    if(name=="worldclim"){
      ####
      # Remove if(res== 0.5), once ggmap2 function is finished!!!
      ####
      #if(res == 0.5){
      #  if(any(grepl("bio", variables))){
      #    envdata <- ggmap2::getData(name=name, var="bio", res=res, path=path)
      #    if(unique(variables == "bio")){variables <- c("bio1", "bio5", "bio6", "bio7", "bio8", "bio9", 
      #                                      "bio10", "bio11", "bio12", "bio13", "bio14", 
      #                                      "bio15", "bio16", "bio17", "bio18", "bio19")
      #    } else{envdata <- envdata[[variables]]}
      #  } else{envdata <- lapply(variables, FUN=function(z){ggmap2::getData(name=name, var=z, res=res, path=path)})}
      #} else{
      if(any(grepl("bio", variables))){
        # Get data
        envdata <- raster::getData(name=name, var="bio", res=res, path=path)
        envdata <- raster::crop(envdata, extent)
        if(unique(variables == "bio")==TRUE){variables <- c("bio1", "bio5", "bio6", "bio7", "bio8", "bio9", 
                                                "bio10", "bio11", "bio12", "bio13", "bio14", 
                                                "bio15", "bio16", "bio17", "bio18", "bio19")}
        # Subset data by variables
        envdata <- envdata[[variables]]
        
        # Convert temperature values into degree C
        env_temp <- envdata[[variables[variables %in% c("bio1", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11")]]]/ 10
        env_prec <- envdata[[variables[! variables %in% c("bio1", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11")]]]
        envdata <- raster::stack(env_temp, env_prec); rm(env_temp, env_prec)
      } else{
        # Get data
        envdata <- lapply(variables, FUN=function(z){
          data <- raster::getData(name=name, var=z, res=res, path=path)
          raster::crop(data, extent)})
        # Convert temperature values into degree C
        if("tmin" %in% variables){envdata[[which(variables == "tmin")]] <- envdata[[which(variables == "tmin")]]/10}
        if("tmax" %in% variables){envdata[[which(variables == "tmax")]] <- envdata[[which(variables == "tmax")]]/10}
      }
      #}
    }
    
    # Calculate mean and sum by year, quarter or month for tmin, tmax and prec
    if(unique(variables %in% c("tmin", "tmax", "prec")) == TRUE){
      if(tres == "year"){
        #Mean of tmin and tmax
        if("tmin" %in% variables){envdata[[which("tmin" == variables)]] <- raster::calc(envdata[[which("tmin" == variables)]], mean)}
        if("tmax" %in% variables){envdata[[which("tmax" == variables)]] <- raster::calc(envdata[[which("tmax" == variables)]], mean)}
        # Sum of prec
        if("prec" %in% variables){envdata[[which("prec" == variables)]] <- raster::calc(envdata[[which("prec" == variables)]], sum)}
        
        # Extract envdata for points
        envdata <- data.frame(do.call("cbind", lapply(envdata, FUN=function(x){
          raster::extract(x, data[,c(lon,lat)])})))
        colnames(envdata) <- variables
      } else if(tres %in% c("quarter", "month")){
        if(tres == "quarter"){
          # time
          tm <- seq(as.Date('1000-01-15'), as.Date('1000-12-15'), 'month') # Year is irrelevant
          
          #Mean of tmin and tmax
          if("tmin" %in% variables){
            envdata[[which("tmin" == variables)]] <- raster::setZ(envdata[[which("tmin" == variables)]], tm, 'months')
            envdata[[which("tmin" == variables)]] <- raster::zApply(envdata[[which("tmin" == variables)]], 
                                                              by=zoo::as.yearqtr, fun=min, name='quarters')}
          if("tmax" %in% variables){
            envdata[[which("tmax" == variables)]] <- raster::setZ(envdata[[which("tmax" == variables)]], tm, 'months')
            envdata[[which("tmax" == variables)]] <- raster::zApply(envdata[[which("tmax" == variables)]], 
                                                              by=zoo::as.yearqtr, fun=max, name='quarters')}
          # Sum of prec
          if("prec" %in% variables){
            envdata[[which("prec" == variables)]] <- raster::setZ(envdata[[which("prec" == variables)]], tm, 'months')
            envdata[[which("prec" == variables)]] <- raster::zApply(envdata[[which("prec" == variables)]], 
                                                              by=zoo::as.yearqtr, fun=sum, name='quarters')}
          time <- c("Q1", "Q2", "Q3", "Q4")
        } else{
          time <- month.abb
        }
        # Extract envdata for points
        envdata <- do.call("cbind", lapply(1:length(envdata), FUN=function(x){
          data <- data.frame(raster::extract(envdata[[x]], data[,c(lon,lat)]))
          colnames(data) <- time
          data <- tidyr::gather(data, "month", "var")
          colnames(data) <- c("time", variables[x])
          return(data)
        }))
        #envdata %>% dplyr::arrange(time)
        envdata <- envdata[,c(1,seq(2,ncol(envdata),by=2))]
      } else{
        stop("Wrong temporal resolution (tres) supplied. Should be one of month, quarter, year.")
      }
    } else{
      envdata <- data.frame(raster::extract(envdata, data[,c(lon,lat)]))
      colnames(envdata) <- variables
    }
    
    # Merge data with locations
    envdata <- cbind(data[,"species"], envdata)
    
    # Set colnames to variablesnames
    colnames(envdata)[1] <- c("species")
    
    # Remove NAs
    envdata <- stats::na.omit(envdata)
    
    # Change data format
    if(tres!="year"){
      envdata <- tidyr::gather(envdata, "var", "value", -c(species,time))
      envdata$time <- factor(envdata$time, levels=time)
    } else{
      envdata <- tidyr::gather(envdata, "var", "value", -species)
    }
    
    # Order variables factor
    envdata$var <- factor(envdata$var)
    if(any(c("tmin", "tmax", "prec") %in% variables)){
      envdata$var <- factor(envdata$var, levels=c("tmin", "tmax", "prec"))
    } else if(any(c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                    "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", 
                    "bio16", "bio17", "bio18", "bio19") %in% variables)){
      envdata$var <- factor(envdata$var, 
                            levels=c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                                     "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", 
                                     "bio15", "bio16", "bio17", "bio18", "bio19"))
    }
    envdata$var <- droplevels(envdata$var)
  } else{
    # Subset data by variables
   envdata <- data %>% tidyr::pivot_longer(cols = tidyselect::any_of(c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                                                                      "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", 
                                                                      "bio15", "bio16", "bio17", "bio18", "bio19")), 
                                            names_to="var", values_to="value")
   envdata <- envdata[envdata$var %in% variables,]
   envdata$var <- as.factor(envdata$var)
  }
  
  if(any(c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
           "bio7", "bio8", "bio9", "bio10", "bio11") %in% variables)){
    # Define temperature breaks
    t_range <- range(envdata$value[envdata$var %in% 
                                     c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                                       "bio7", "bio8", "bio9", "bio10", "bio11")], na.rm=T)
    t_diff <- ceiling((ceiling(t_range[2]) - floor(t_range[1]))/5)*5
    if(t_diff >= 60){
      t_breaks=20} else if(t_diff >= 40){
        t_breaks=10} else if(t_diff >= 20){
          t_breaks = 5} else{
            t_breaks = 2.5}
  }
  
  if(any(c("prec","bio12", "bio13", "bio14", "bio15", 
           "bio16", "bio17", "bio18", "bio19") %in% variables)){
    if(any(c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
             "bio7", "bio8", "bio10", "bio11") %in% variables)){
      # Define precipitation breaks
      if(max(envdata$value[envdata$var %in% 
                           c("prec","bio12", "bio13", "bio14", "bio15", 
                             "bio16", "bio17", "bio18", "bio19")], na.rm=T) > 5000){
        p_breaks=1500}else if(max(envdata$value[envdata$var %in% 
                                                c("prec","bio12", "bio13", "bio14", "bio15", 
                                                  "bio16", "bio17", "bio18", "bio19")], na.rm=T) >= 3000){
          p_breaks=1000}else if(max(envdata$value[envdata$var %in% 
                                                  c("prec","bio12", "bio13", "bio14", "bio15", 
                                                    "bio16", "bio17", "bio18", "bio19")], na.rm=T) > 1500){
            p_breaks = 500
          }else if(max(envdata$value[envdata$var %in% 
                                     c("prec","bio12", "bio13", "bio14", "bio15", 
                                       "bio16", "bio17", "bio18", "bio19")], na.rm=T) >= 750){
            p_breaks = 250
          } else if(max(envdata$value[envdata$var %in% 
                                      c("prec","bio12", "bio13", "bio14", "bio15", 
                                        "bio16", "bio17", "bio18", "bio19")], na.rm=T) >= 200){
            p_breaks = 100} else {p_breaks = 50}
      
      # Transform precipitation
      offset <- min(envdata$value[envdata$var %in% 
                                    c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                                      "bio7", "bio8", "bio10", "bio11")], na.rm=T)
      max_offset <- max(envdata$value[envdata$var %in% 
                                        c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                                          "bio7", "bio8", "bio10", "bio11")]-offset, na.rm=T)/max(envdata$value[envdata$var %in% c("prec","bio12", "bio13", "bio14", "bio15", 
                                                                                                                                   "bio16", "bio17", "bio18", "bio19")], na.rm=T)
      envdata$value[envdata$var %in% c("prec","bio12", "bio13", "bio14", "bio15", 
                                       "bio16", "bio17", "bio18", "bio19")] <- 
        envdata$value[envdata$var %in% c("prec","bio12", "bio13", "bio14", "bio15", 
                                         "bio16", "bio17", "bio18", "bio19")]*max_offset+offset
    } else{
      offset <- 0
      max_offset <- 1
    }
  }
  
  # Determine orientation of axes label and plot margins
  if(length(variables) > 5 | tres == "month"){
    las=2
    graphics::par(mar = c(4.5,5,1.5,7.5)+.1, bg="white", cex.lab=1.5, cex.axis=1.2)
  } else{
    las=1
    graphics::par(mar = c(2.5,5,1.5,7.5)+.1, bg="white", cex.lab=1.5, cex.axis=1.2)
  }
  
  # Create plot
  if(tres != "year"){
    col <- variables
    col[which(col == "tmin")] <- "dodgerblue"
      col[which(col == "tmax")] <- "darkblue"
        col[which(col == "prec")] <- "red"
          graphics::boxplot(value ~ var:time, envdata, pch=19, col=col, xlab="", ylab="", axes=F)
          graphics::legend('topright', horiz = F, fill = col, legend = levels(envdata$var), 
                           bty = 'n')
          graphics::axis(side=1, at=seq(1, nlevels(envdata$time)*nlevels(envdata$var), 
                                        by=nlevels(envdata$var))+(0.5*nlevels(envdata$var)-0.5), 
                         labels=levels(envdata$time), las=las)
          col.axis <- c("blue", "red")
  } else if(nlevels(envdata$species) > 1){
    col <- ggsci::pal_d3("category10")(nlevels(envdata$species))
    graphics::boxplot(value ~ species:var, envdata, pch=19, col=col, xlab="", ylab="", axes=F)
    graphics::abline(v=length(which(variables %in% c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                                               "bio7", "bio8", "bio9", "bio10", "bio11")))*nlevels(envdata$species)+0.5, 
                     col="grey", lty="dashed")
    graphics::legend('topright', horiz = F, fill = col, legend = levels(envdata$species), 
                     bty = 'n')
    graphics::axis(side=1, at=seq(1, nlevels(envdata$var)*nlevels(envdata$species), 
                                  by= nlevels(envdata$species))+(0.5*nlevels(envdata$species)-0.5), 
                   labels=levels(envdata$var), las=las)
    col.axis <- c("black", "black")
  } else{
    col <- variables
    col[which(col %in% c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                         "bio7", "bio8", "bio9", "bio10", "bio11"))] <- "blue"
      col[col != "blue"] <- "red"
        graphics::boxplot(value ~ var, envdata, pch=19, col=col, xlab="", ylab="", axes=F)
        graphics::abline(v=length(which(variables %in% c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                                                   "bio7", "bio8", "bio9", "bio10", "bio11")))+0.5, 
                         col="grey59", lty="dashed")
        graphics::axis(side=1, at=seq(1,nlevels(envdata$var), 
                                      length.out=nlevels(envdata$var)), 
                       labels=levels(envdata$var), las=las)
        col.axis <- c("blue", "red")
  }
  if(any(variables %in% c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                    "bio7", "bio8", "bio9", "bio10", "bio11"))){
    graphics::axis(side=2, at=seq(floor(min(envdata$value)/t_breaks)*t_breaks, 
                                  ceiling(max(envdata$value)/t_breaks)*t_breaks, by=t_breaks), 
                   col.axis=col.axis[1], las=1)
    graphics::mtext(side = 2, line=4, expression(paste("Temperature (",degree,"C)")), 
                    cex=1.5, col=col.axis[1], padj=1)
  }
  if(any(variables %in% c("prec","bio12", "bio13", "bio14", "bio15", "bio16", 
                    "bio17", "bio18", "bio19"))){
    if(any(variables %in% c("tmin", "tmax","bio1", "bio2", "bio3", "bio4", "bio5", "bio6", 
                      "bio7", "bio8", "bio9", "bio10", "bio11"))){
      graphics::axis(side=4, at=seq(min(envdata$value), max(envdata$value), by=p_breaks*max_offset), 
                     labels=seq((min(envdata$value)-offset)/max_offset, 
                                (max(envdata$value)-offset)/max_offset, by=p_breaks), col.axis=col.axis[2], las=1)
      graphics::mtext(side = 4, line=4, "Precipitation (mm)", col=col.axis[2], cex=1.5)
    } else{
      graphics::axis(side=2, at=seq(min(envdata$value), max(envdata$value), by=p_breaks*max_offset), 
                     labels=seq((min(envdata$value)-offset)/max_offset, 
                                (max(envdata$value)-offset)/max_offset, by=p_breaks), 
                     col.axis=col.axis[2], las=1)
      graphics::mtext(side = 2, line=4, "Precipitation (mm)", col=col.axis[2], cex=1.5)
    }
  }
  graphics::box()
  
  # Add histogram
  if(hist==TRUE){
    if(tres == "month"){
      print("hist=TRUE is not provided for tres=month. Use hist=FALSE or tres=year instead.")
    } else if(nlevels(envdata$species) > 1){
      # Plot histogram with one or multiple species
      # Does not work for monthly or quarterly resolution
      # How to incorporate multiple variables???
      #p1 <- hist(data$value, main="")
      #p2 <- hist(data$value)
      #set.seed(42)
      #plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
      #plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second
      # Add line with mean and 95% quantiles!
    } else{
      # Plot multiple variables next to each other!!!
      # Split only temp and prec variables!!!
      ## first plot - left half of x-axis, right margin set to 0 lines
      #par(fig = c(0, .5, 0, 1), mar = c(5,4,3,0))
      #hist(data$diff[data$type==0], ann = FALSE, las = 1)
      
      ## second plot - right half of x-axis, left margin set to 0 lines
      #par(fig = c(.5, 1, 0, 1), mar = c(5,0,3,2), new = TRUE)
      #hist(data$diff[data$type==1], ann = FALSE, axes = FALSE)
      #axis(1)
      #axis(2, lwd.ticks = 0, labels = FALSE)
      
      #title(main = 'Histogram', xlab = 'x label', outer = TRUE, line = -2)
    }
  }
  
  # Add map
  if(map == TRUE){
    # Plot geographic map of climatic data
    # Plot bubblemap of species data coloured by climate, month!
    # Different shapes for different species
    #(see R book Numerical Ecology for bubbleplot!)
  }
  #return(p)
}
