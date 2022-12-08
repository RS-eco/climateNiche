#' Get Species data for all locations based on a certain grid
#'
#' Get raster files of multiple species and extract only the coordinates, where individuals are present
#' 
#' @param species Latin name of bird species
#' @param path Specifying the location of ebird data
#' @param extent Extent of study area
#' @param resolution Resolution of grid cells
#' @param filename Specify filename of output
#' @return dataframe with species name, latitude, longitude, location name, 
#' abundance count and retrieval date
#' @examples
#' \dontrun{
#' getebird()
#' }
#' @export
getebird <- function(species = "Spinus tristis", path="E:/Data/ebird_us48/2014", extent=c(-180,180,-90,90), 
                     resolution=0.5, filename=NA){
  requireNamespace(rebird)
  
  # Turn extent into an exten object
  if(class(extent) != "Extent"){
    extent <- raster::extent(extent)
  }
  
  # Read ebird data
  checklists <- read.csv(paste0(path, "/checklists_50_record_sample.csv"))
  core_covariates <- read.csv(paste0(path, "/core-covariates_50_record_sample.csv"))
  extended_covariates <- read.csv(paste0(path, "/extended-covariates_50_record_sample.csv"))
  
  # Extract species data
  sp_data <- checklists[,20:ncol(checklists)]
  
  # Extract lat, lon, date
  xyz <- checklists[,c("LONGITUDE", "LATITUDE", "YEAR", "MONTH", "DAY", "TIME")]
  xyz$DATE <- as.Date(paste(xyz$YEAR, "01", "01", sep="-"))
  xyz$DATE <- as.Date(xyz$DAY, origin=xyz$DATE)
 
  # Add time, still does not work
  xyz$DATE <- as.Date(paste(xyz$DATE, xyz$TIME, sep=" "), format="%Y-%m-%d %H.%M")
  
  # Merge xyz and sp_data and convert to long format (reduce 0s)
  # TO DO!!!
  
  # Create an empty raster with specified extent and resolution
  r <- raster(x=extent, resolution=resolution)
  
  # ebirdgeo takes a certain distance into account
  # need to specify distance according to resolution of raster cells
  pt1 = readWKT("POINT(0 0)")
  #crs(pt1) <- CRS("+init=epsg:4326")
  pt2 = readWKT(paste0("POINT(", 0+resolution, " ", 0+resolution, ")"))
  #crs(pt2) <- CRS("+init=epsg:4326")
  #dist <- gDistance(pt1, pt2)
  
  # Sightings at location determined by latitude/longitude
  
  ## Search for bird occurrences by latitude and longitude point
  data <- ebirdgeo(species = species, lat = 42, lng = -76)
  
  ## Same, but with additional parameter settings, returning only 10 records, 
  ## including provisional records, and hotspot records
  data <- ebirdgeo(lat = 42, lng = -76, max = 10, 
                   includeProvisional = TRUE, hotspot = TRUE)
  
  ## Search for bird occurrences by region and species name
  ebirdregion(region = 'US', species = 'Setophaga caerulescens')
  
  # Recent sightings frm location IDs
  
  ## Search for bird occurrences for two locations by their IDs
  ebirdloc(locID = c('L99381','L99382'))
  
  ## Search by location ID and species name, as well as some additional parameter settings
  ebirdloc(locID = 'L99381', species = 'larus delawarensis', max = 10, provisional = TRUE, hotspot=TRUE)
  
  ## Obtain frequency data for a given state or county
  ebirdfreq(loctype = 'states', loc = 'CA-BC')
  # loctype= states, counties, hotspots
  # startyear, endyear, startmonth, endmonth
  # long=TRUE or FALSE
  
  ## Search for notable sightings at a given latitude and longitude
  ebirdnotable(lat = 42, lng = -70)
  
  ## Return eBird taxonomy
  ebirdtaxonomy()
}
