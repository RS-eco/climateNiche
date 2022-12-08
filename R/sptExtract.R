#' Extract spatio-temporal environmental data
#'
#' Function to extract spatio-temporal
#' environmental data for given locations and certain time interval.
#'
#' @param locations A spatial points data frame.
#' @param env_data A raster time series object.
#' @param tres A time interval.
#' @return data frame with the environmental data for each location \code{lm}.
#' @examples
#' \dontrun{
#' # Read Species Data
#' library(climateNiche)
#' sp_data <- get(data("Equus_quagga"))
#' 
#' # Remove entries where no month is provided
#' sp_data <- sp_data[which(!is.na(sp_data$month)),]
#' 
#' # Get environmental data
#' prec <- raster::getData(name = "worldclim", var = "prec", res = 10)
#'
#' sp_prec_mon <- sptExtract(sp_data, prec, tres="month")
#' sp_prec_qtr <- sptExtract(sp_data, prec, tres="quarter")
#' }
#' @export
sptExtract <- function(locations, env_data, tres="month") {
  if(tres == "month"){
    months <- c(1:12)
    months <- which(months %in% unique(locations$month))
    env_locs <- lapply(months, FUN=function(i){
      locations_sub <- locations[locations$month == i,]
      env_sub <- env_data[[i]]
      if(length(locations_sub) > 0){
        raster::extract(env_sub, locations_sub)
      }
    })
    env_locs <- do.call("c", env_locs)
    locations@data <- cbind(locations@data, env_locs)
  } else if(tres == "quarter"){
    locations$quarter <- ceiling(locations$month/3)
    month_quarters <- list(c(1:3), c(4:6), c(7:9), c(10:12))
    quarters <- which(c(1:4) %in% unique(locations$quarter))
    env_locs <- lapply(quarters, FUN=function(i){
      locations_sub <- locations[locations$month %in% month_quarters[[i]],]
      env_sub <- raster::calc(env_data[[month_quarters[[i]]]], fun=mean)
      if(length(locations_sub) > 0){
        raster::extract(env_sub, locations_sub)
      }
    })
    env_locs <- do.call("c", env_locs)
    locations@data <- cbind(locations@data, env_locs)
  } else {
    stop(tres, " not recognized as a valid temporal resolution.")
  }
  return(locations)
}
