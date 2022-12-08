# Create Data for climateNichepackage

# Raster files have been created as part of the rasterSp package and are then copied to 
# the extdata folder

# Read Raster file
Passer_castanopterus <- raster::raster("inst/extdata/Passer_castanopterus.tif")
Passer_domesticus <- raster::raster("inst/extdata/Passer_domesticus.tif")
Passer_motitensis <- raster::raster("inst/extdata/Passer_motitensis.tif")
Passer_zarudnyi <- raster::raster("inst/extdata/Passer_zarudnyi.tif")

# Force data into memory
Passer_castanopterus <- raster::readAll(Passer_castanopterus)
Passer_domesticus <- raster::readAll(Passer_domesticus)
Passer_motitensis <- raster::readAll(Passer_motitensis)
Passer_zarudnyi <- raster::readAll(Passer_zarudnyi)

# Save as .rda file
save(Passer_castanopterus, file="data/Passer_castanopterus.rda", compress="xz")
save(Passer_domesticus, file="data/Passer_domesticus.rda", compress="xz")
save(Passer_motitensis, file="data/Passer_motitensis.rda", compress="xz")
save(Passer_zarudnyi, file="data/Passer_zarudnyi.rda", compress="xz")

# Get Equus quagga GBIF data
source("R/terrestrialGBIF.R")
terrestrialGBIF(species="Equus quagga", genus=NULL, limit = 200000, hasCoordinate=TRUE,
                path = "data/", overwrite=T)
Equus_quagga <- readRDS("data/Equus_quagga.rds")
save(Equus_quagga, file="data/Equus_quagga.rda", compress="xz")
file.remove("data/Equus_quagga.rds")
