#' Get GBIF data for a terrestrial species
#'
#' Function to extract GBIF data and save it to file.
#' Output further does not contain any outliers and only files on land.
#'
#' @param species Latin name of species.
#' @param genus Genus of species
#' @param limit Maximum number of records to obtain from GBIF. Note that there is a hard maximum of 200,000, which is calculated as the limit+start, so start=199,000 and limit=2000 won't work
#' @param hasCoordinate logical. Return only occurence records with lat/long data (TRUE) or all records (FALSE, default).
#' @param path File path, where to save the species data
#' @param overwrite logical. If TRUE, fill will be overwritten if it exists.
#' @return SpatialPointsDataFrame with the occurrence of the species
#' @examples
#' \dontrun{
#' terrestrialGBIF(species="Equus quagga", limit=100)
#' }
#' @export
terrestrialGBIF <- function(species="Equus quagga", genus=NULL, limit = 50000, hasCoordinate=TRUE,
                            path = getwd(), overwrite = FALSE){
  # Create GBIF folder if it does not exist
  if(!dir.exists(paste0(path))){dir.create(path=paste0(path))}
  
  # Get data of species
  species_name <- paste0(strsplit(species, split=" ")[[1]][1], "_", strsplit(species, split=" ")[[1]][2])
  if(overwrite == FALSE & file.exists(paste0(path, species_name, ".rds"))){
    species_data <- readRDS(paste0(path, species_name, ".rds"))
    
    # Get high resolution country data
    countriesHigh <- get(utils::data(countriesHigh, package="rworldxtra", envir = environment()))
    
    # Remove species data at sea
    country_per_point <- sp::over(species_data, countriesHigh)
    species_data <- species_data[!is.na(country_per_point$SOV_A3),]
    rm(country_per_point)
    
    # Remove outliers
    species_data <- species_data[species_data$decimalLongitude != outliers::outlier(species_data$decimalLongitude),]
    species_data <- species_data[species_data$decimalLatitude != outliers::outlier(species_data$decimalLatitude),]
    
    # Return data
    return(species_data)
  } else{
    # Download species location data from gbif
    if(is.null(species)){
      species_data <- rgbif::occ_search(taxonKey=rgbif::name_backbone(name=species)$speciesKey, return="data", 
                                        hasCoordinate = hasCoordinate, limit= limit,
                                        fields=c("species", "year", "month", "decimalLatitude", "decimalLongitude"))
      # Set spatial coordinates
      sp::coordinates(species_data) <- c("decimalLongitude", "decimalLatitude")
      
      # Define spatial projection
      sp::proj4string(species_data) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
      
      # Save species records as RDS-File
      saveRDS(species_data, file = paste0(path, species_name, ".rds"), compress = "xz")
      
      # Get high resolution country data
      countriesHigh <- get(utils::data(countriesHigh, package="rworldxtra", envir = environment()))
      
      # Remove species data at sea
      country_per_point <- sp::over(species_data, countriesHigh)
      species_data <- species_data[!is.na(country_per_point$SOV_A3),]
      rm(country_per_point)
      
      # Remove outliers
      species_data <- species_data[species_data$decimalLongitude != outliers::outlier(species_data$decimalLongitude),]
      species_data <- species_data[species_data$decimalLatitude != outliers::outlier(species_data$decimalLatitude),]
      
      # Return data
      return(species_data)
    } else if(is.null(genus)){
      species_data <- rgbif::occ_search(genusKey=rgbif::name_backbone(name=species)$genusKey, return="data", 
                                        hasCoordinate = hasCoordinate, limit= limit, 
                                        fields=c("species", "year", "month", "decimalLatitude", "decimalLongitude"))
      # Set spatial coordinates
      sp::coordinates(species_data) <- c("decimalLongitude", "decimalLatitude")
      
      # Define spatial projection
      sp::proj4string(species_data) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
      
      # Save species records as RDS-File
      saveRDS(species_data, file = paste0(path, species_name, ".rds"), compress = "xz")
      
      # Get high resolution country data
      countriesHigh <- get(utils::data(countriesHigh, package="rworldxtra", envir = environment()))
      
      # Remove species data at sea
      country_per_point <- sp::over(species_data, countriesHigh)
      species_data <- species_data[!is.na(country_per_point$SOV_A3),]
      rm(country_per_point)
      
      # Remove outliers
      species_data <- species_data[species_data$decimalLongitude != outliers::outlier(species_data$decimalLongitude),]
      species_data <- species_data[species_data$decimalLatitude != outliers::outlier(species_data$decimalLatitude),]
      
      # Return data
      return(species_data)
    } else{print("No species or genus specified!")}
  }
}