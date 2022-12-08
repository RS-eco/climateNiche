#' Create a climate envelope plot of one or multiple species
#' 
#' Read a SpatialPointsDataFrame or dataframe with species location data and 
#' return a plot with the current and future climate envelope
#' 
#' @param data RasterLayer, SpatialPointsDataFrame or data.frame
#' @param climate character. One of Worldclim, Chelsa or ISIMIP2b
#' @param model character. Should be one of "AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", 
#' "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", or "NO".
#' @param res Valid resolutions are 0.5, 2.5, 5, and 10 (minutes of a degree). In the case of res=0.5, you must also provide a lon and lat argument for a tile; for the lower resolutions global data will be downloaded.
#' @param lon Longitude argument for a tile, only applicable if res = 0.5
#' @param lat Latitude argument for a tile, only applicable if res = 0.5
#' @param rcp character. RCP scenario for future climate data, should be one of 26, 45, 60 or 85.
#' @param year numeric. Year of future climate scenario, should be one of 50, 70.
#' @param extent specify extent of area considered
#' @param method character. One of CH (convex hull) or MCP (minimum convex polygon) or MFB (personal method).
#' @param path Character. Path name indicating where to store the data. 
#' Default is the current working directory.
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' library(climateNiche)
#' data(Passer_domesticus)
#' 
#' climateEnvelope(data=Passer_domesticus, method="MCP")
#' 
#' climateEnvelope(data=Passer_domesticus, method="CH")
#' 
#' climateEnvelope(data=Passer_domesticus, method="MFB")
#' }
#' @export
climateEnvelope <- function(data, climate="worldclim", model="MI", 
                            res=10, lon="x", lat="y", rcp=60, year=50, extent=NA, 
                            method="MCP", path=""){
  # Convert data to dataframe
  if(any(is(data) %in% c("RasterStack", "RasterBrick"))){data <- raster::unstack(data)}
  if(is(data, "list")){
    if(!is.null(names(data))){
      label <- names(data)} else{
        label <- sapply(data, function(x) names(x[[1]]))
      }
    data <- lapply(1:length(data), FUN=function(x){
      if(is(data[[x]], "RasterLayer")){
        data <- data.frame(raster::rasterToPoints(data[[x]]))
        data$label <- label[x]
        colnames(data) <- c("x", "y", "presence", "label")
      } else{
        data <- data[[x]]
        data$label <- label[x]
        data <- data %>% dplyr::select(tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                                "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                                "decimaLatitude", "Longitude", "Latitude", 
                                                "long", "lat", "Long", "Lat", "x", "y")), "label")
        data$presence <- 1
        colnames(data) <- c("x", "y", "label", "presence")
      }
      return(data)
    })
    data <- do.call("rbind", data)
    data$label <- factor(data$label, ordered=TRUE)
  } else if(any(is(data) %in% c("RasterLayer", "RasterStack", "RasterBrick"))){
    label <- names(data)
    data <- data.frame(raster::rasterToPoints(data))
    data$label <- factor(label, ordered=TRUE); rm(label)
  } else{
    data <- as.data.frame(data)
    data <- data %>% dplyr::select(tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                            "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                            "decimaLatitude", "Longitude", "Latitude", 
                                            "long", "lat", "Long", "Lat", "x","y")), 
                                   tidyselect::one_of(c("label", "SCIENTIFIC.NAME", "scientificName",
                                            "scientificname", "name")))
    data$presence <- 1
    colnames(data) <- c("x", "y", "label", "presence")
  }
  
  # Get current environmental data
  if(climate=="worldclim"){
    ####
    # Remove if(res== 0.5), once ggmap2 function is finished!!!
    ####
    #if(res == 0.5){
    #  envdata <- ggmap2::getData(name=climate, var="bio", res=res, path=path)
    #  futuredata <- ggmap2::getData(name="CMIP5", var="bio", res=res, rcp=rcp, 
    #                                model=model, year=year, path=path)
    #} else{# Get data
      envdata <- raster::getData(name=climate, var="bio", res=res, path=path)
      futuredata <- raster::getData(name="CMIP5", var="bio", res=res, rcp=rcp,
                                    model=model, year=year, path=path)
    #}
    # Only extract bio1 and bio12
    envdata <- envdata[[c(1, 12)]]
    futuredata <- futuredata[[c(1,12)]]
    
    # Convert temperature values into degree C
    envdata[[1]] <- envdata[[1]]/ 10
    futuredata[[1]] <- futuredata[[1]]/ 10
  } else{
    
  }
  
  # Extract envdata for points
  envdata <- data.frame(raster::extract(envdata, data[,c(lon,lat)]))
  futuredata <- data.frame(raster::extract(futuredata, data[,c(lon,lat)]))
  colnames(envdata) <- c("bio1", "bio12")
  colnames(futuredata) <- c("bio1", "bio12")
  
  # Merge data with locations
  envdata <- cbind(data[,"label"], envdata)
  if(climate == "worldclim"){envdata$year <- 1975}
  futuredata <- cbind(data[,"label"], futuredata)
  futuredata$year <- paste0(20, year)
  envdata <- rbind(envdata, futuredata)
  colnames(envdata) <- c("label", "bio1", "bio12", "year")
  envdata$year <- as.factor(envdata$year)
  
  # Remove NA's
  envdata <- stats::na.omit(envdata)
  
  ## Method 1 ##
  
  # Calculate convex hull of climatic niche
  if(method == "CH"){
    hpts <- grDevices::chull(x = envdata[envdata$year == levels(envdata$year)[1],]$bio12, 
                  y = envdata[envdata$year == levels(envdata$year)[1],]$bio1)
    hpts <- c(hpts, hpts[1])
    poly_cur <- data.frame(x=envdata[envdata$year == levels(envdata$year)[1],]$bio12[hpts], 
                           y=envdata[envdata$year == levels(envdata$year)[1],]$bio1[hpts])
    hpts <- grDevices::chull(x = envdata[envdata$year == levels(envdata$year)[2],]$bio12, 
                  y = envdata[envdata$year == levels(envdata$year)[2],]$bio1)
    hpts <- c(hpts, hpts[1])
    poly_fut <- data.frame(x=envdata[envdata$year == levels(envdata$year)[2],]$bio12[hpts], 
                           y=envdata[envdata$year == levels(envdata$year)[2],]$bio1[hpts])
    
    # Create plot
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[1],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="black",alpha=0.1) + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[2],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="red", alpha=0.1) + 
      ggplot2::geom_polygon(data=poly_cur, ggplot2::aes_string(x="x",y="y"), fill=NA, colour="black") + 
      ggplot2::geom_polygon(data=poly_fut, ggplot2::aes_string(x="x",y="y"), fill=NA, colour="red") + 
      ggplot2::theme_bw() + ggplot2::labs(x="Total annual precipitation [mm]", y=expression("Annual mean temperature [", degree,"C]"))
  } else if(method=="MCP"){
    ## Method 2 ##
    ## Calculate minimum convex polygon of climatic niche
    # With percent=100, same result as chull function!
    
    sp::coordinates(envdata) <- c("bio12", "bio1")
    mcp1 <- adehabitatHR::mcp(envdata[envdata$year == levels(envdata$year)[1],], percent=95)
    mcp2 <- adehabitatHR::mcp(envdata[envdata$year == levels(envdata$year)[2],], percent=95)
    envdata <- data.frame(envdata)
    
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[1],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="black",alpha=0.1) + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[2],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="red", alpha=0.1) + 
      ggplot2::geom_polygon(data=mcp1, ggplot2::aes_string(x="long", y="lat"), 
                            colour="black", alpha=0.05, fill=NA) +
      ggplot2::geom_polygon(data=mcp2, ggplot2::aes_string(x="long", y="lat"), 
                            colour="red", alpha=0.05, fill=NA) + 
      ggplot2::theme_bw() +  ggplot2::labs(x="Total annual precipitation [mm]", y=expression("Annual mean temperature [", degree,"C]"))
  } else if(method == "MFB"){
    ## Method 3
    requireNamespace("sf")
    envdata_sub <- envdata
    envdata_sub$bio12 <- round(envdata_sub$bio12/100)*100
    envdata_sub <-  dplyr::group_by("envdata_sub", "year", "bio12")
    envdata_min <- dplyr::summarise(envdata_sub, "bio1"=min(rlang::.data$bio1))
    envdata_min <- dplyr::arrange(envdata_min, rlang::.data$bio12)
    envdata_max <- dplyr::summarise(envdata_sub, "bio1"=max(rlang::.data$bio1))
    envdata_max <- dplyr::arrange(envdata_max, dplyr::desc(rlang::.data$bio12))
    envdata_sub <- rbind(envdata_min, envdata_max)
    envdata_poly <- lapply(1:nlevels(envdata_sub$year), function(year){
      sf::st_polygon(list(cbind(c(envdata_sub[envdata_sub$year == levels(envdata_sub$year)[year],]$bio12, 
                                  envdata_sub[envdata_sub$year == levels(envdata_sub$year)[year],]$bio12[1]),
                                c(envdata_sub[envdata_sub$year == levels(envdata_sub$year)[year],]$bio1, 
                                  envdata_sub[envdata_sub$year == levels(envdata_sub$year)[year],]$bio1[1]))))
    })
    envdata_poly <- lapply(envdata_poly, sf::st_geometry)
    envdata_poly <- lapply(envdata_poly, sf::st_sf)
    envdata_poly[[1]] <- methods::as(envdata_poly[[1]], "Spatial")
    envdata_poly[[2]] <- methods::as(envdata_poly[[2]], "Spatial")
    p <- ggplot2::ggplot() + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[1],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="black",alpha=0.1) + 
      ggplot2::geom_point(data=envdata[envdata$year == levels(envdata$year)[2],], 
                          ggplot2::aes_string(x="bio12",y="bio1"), fill=NA, colour="red", alpha=0.1) + 
      ggplot2::geom_polygon(data=envdata_poly[[1]], ggplot2::aes_string(x="long", y="lat"), 
                            colour="black", alpha=0.05, fill=NA) +
      ggplot2::geom_polygon(data=envdata_poly[[2]], ggplot2::aes_string(x="long", y="lat"), colour="red", alpha=0.05, fill=NA) + 
      ggplot2::theme_bw() +  ggplot2::labs(x="Total annual precipitation [mm]", y=expression(paste("Annual mean temperature [",degree,"C]")))
  }
  return(p)
}
