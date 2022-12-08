#' Create plot of  Distribution or Range
#' 
#' Read a SpatialPointsDataFrame or normal dataframe and plot the coordinates
#' 
#' @param data RasterLayer, SpatialPointsDataFrame or data.frame
#' @param name character. Adds a name to the legend if provided.
#' @param extent Specify extent of your Map
#' @param country Should country outline be plotted or not
#' @param cut logical. Cut outline by extent
#' @param grid logical. Should grid lines and axes be drawn or not
#' @param save logical. If TRUE plot object is saved, see also the subsequent parameters.
#' @param filename character. File name to create on disk.
#' @param dpi numeric. Plot resolution. 
#' @param width,height,units Plot size in units ("in", "cm", or "mm"). If not supplied, uses the size of current graphics device.
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' library(climateNiche)
#' data(Passer_domesticus)
#' 
#' plotSp(data=Passer_domesticus)
#' }
#' @export
plotSp <- function(data, name="", extent=NA, country=TRUE, cut=FALSE, grid=FALSE, save=FALSE,
                   filename, dpi=300, width=NA, height=NA, units=c("in", "cm", "mm")){
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
        data <- dplyr::select(data, tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                                         "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                                         "decimalLatitude", "Longitude", "Latitude", 
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
    data <- dplyr::select(data, tidyselect::one_of(c("decimallongitude", "decimallatitude", 
                                                     "LONGITUDE", "LATITUDE", "decimalLongitude", 
                                                     "decimaLatitude", "Longitude", "Latitude", 
                                                     "long", "lat", "Long", "Lat", "x","y")), 
                          tidyselect::one_of(c("label", "SCIENTIFIC.NAME", "scientificName",
                                               "scientificname", "name")))
    data$presence <- 1
    colnames(data) <- c("x", "y", "label", "presence")
  }
  
  # Define extent
  if(anyNA(extent)){
    extent <- raster::extent(c(min(data$x), max(data$x), min(data$y), max(data$y)))
  }else if(any(is(extent) != "Extent")){
    extent <- raster::extent(extent)
  }
  
  #Load world data for mapping
  data(outline, package="ggmap2", envir = environment())
  if(cut == TRUE){
    # and crop by extent
    outline <- raster::crop(outline, extent)
  }
  
  # Add colours and names
  if(nlevels(factor(data$label)) == 1){
    p <- ggplot2::ggplot() + ggplot2::geom_tile(data=data, ggplot2::aes_string(x="x",y="y", fill="label")) + 
      ggplot2::scale_fill_manual(name=name, values=c("blue"), na.value="transparent") + 
      ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                                                axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                                                panel.grid = ggplot2::element_blank(), legend.position="none")
  } else{
    p <- ggplot2::ggplot() + ggplot2::geom_tile(data=data, ggplot2::aes_string(x="x",y="y", fill="label")) + 
      ggplot2::scale_fill_manual(name=name, na.value="transparent",
                                 values= ggsci::pal_d3("category10")(nlevels(data$label))) + 
      ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                                                axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                                                panel.grid = ggplot2::element_blank(), legend.position="bottom") 
  }
  
  # Create plot with or without country outline
  if(country == TRUE){
    p <- p + ggplot2::geom_sf(data=sf::st_as_sf(outline), fill="transparent", color="black") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(), legend.position="bottom")
  } else{
    # Set up a default plot
    if(grid == TRUE){
      # Calculate required axes breaks
      breaks_x <- round(seq(raster::xmin(extent), raster::xmax(extent), length=7))
      breaks_y <- round(seq(raster::ymin(extent), raster::ymax(extent), length=7))
      
      #Create plot
      p <- p + ggplot2::coord_equal() + 
        ggplot2::scale_x_continuous(name=expression(paste("Longitude (",degree,")")), 
                                    expand=c(0.01,0.01), breaks=breaks_x) + 
        ggplot2::scale_y_continuous(name=expression(paste("Latitude (",degree,")")), 
                                    expand=c(0,5), breaks=breaks_y) + ggplot2::theme_bw() + 
        ggplot2::theme(axis.title.x = ggplot2::element_text(size=16),
                       axis.title.y = ggplot2::element_text(size=16, angle=90),
                       axis.text.x = ggplot2::element_text(size=14),
                       axis.text.y = ggplot2::element_text(size=14),
                       legend.text = ggplot2::element_text(size=14),
                       legend.title = ggplot2::element_text(size=16),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
    } else{
      # Without labels
      p <- p + ggplot2::coord_quickmap() + ggplot2::theme_classic() + 
        ggplot2::theme(axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(), legend.position="bottom")
    }
  }
  
  if(save == TRUE){
    ggplot2::ggsave(plot=p, filename=filename, dpi=dpi, width=width, height=height, units=units) 
  }
  return(p)
}
