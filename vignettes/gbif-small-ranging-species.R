## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval=F
)

## ------------------------------------------------------------------------
#  #Automatically install required packages, which are not yet installed
#  packages <- c("raster", "dplyr", "tidyr", "remotes")
#  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) install.packages(new.packages); rm(new.packages)
#  
#  # Load packages
#  l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)
#  
#  # Instal missing packages from GitHub
#  packages_github <- c("rasterSp", "climateNiche")
#  new.packages <- packages_github[!(packages_github %in% installed.packages()[,"Package"])]
#  if(length(new.packages)) remotes::install_github(paste0("RS-eco/", new.packages))
#  rm(new.packages)

## ------------------------------------------------------------------------
#  # Load small range data
#  data("amphibians_dist_smallrange", package="rasterSp")
#  data("ter_birds_dist_smallrange", package="rasterSp")
#  data("ter_mammals_dist_smallrange", package="rasterSp")
#  
#  # Get species names
#  amphinames <- as.character(unique(amphibians_dist_smallrange$species))
#  avesnames <- as.character(unique(ter_birds_dist_smallrange$species))
#  mamanames <- as.character(unique(ter_mammals_dist_smallrange$species))

## ------------------------------------------------------------------------
#  # Connect to GBIF database
#  con <- DBI::dbConnect(RSQLite::SQLite(),
#                        dbname=paste0(filedir,"/gbif_database.sqlite"))
#  gbif <- tbl(con, "gbif")
#  
#  # Run loop for all species of each of the three taxa
#  dataG <- lapply(1:3, function(j){
#    taxa <- list(amphinames, mamanames, avesnames)[[j]]
#    dataG <- list()
#    for(i in 1:length(taxa)){
#      #collect data from gbif
#      data <- gbif %>% filter(species == taxa[i]) %>% collect() %>% data.frame()
#      #get rid of data missing long or lat
#      data <- data[which(!is.na(data$decimallatitude) & !is.na(data$decimallongitude)),]
#  
#      dataG[[taxa[[i]]]] <- data
#  
#      #I added a break-signal after reaching x species for testing
#      #if(i == 50){break}
#    }
#    return(dataG)
#  })
#  
#  # Disconnect from database
#  DBI::dbDisconnect(con); rm(gbif,con)
#  
#  # Assign correct filename to data
#  for(j in 1:3){assign(x=paste0(c("amphibians", "ter_mammals", "ter_birds")[j], "_smallrange_GBIF"), value=dataG[[j]])}

## ------------------------------------------------------------------------
#  data <- lapply(1:3, function(j){
#    r_dataG <- list()
#    dataLess2G <- character()
#    group <- c("amphibians", "ter_mammals", "ter_birds")[j]
#    dataG <- get(load(paste0("data/", group, "_smallrange_GBIF.rda")))
#    for(i in 1:length(dataG)){
#      #collect data from List
#      data <- dataG[[i]]
#      #data should have at least 2 data points
#      if(length(unique(data$decimallatitude))+length(unique(data$decimallongitude))>=4){
#        #rasterizePoints
#        r_data <- rasterSp::rasterizePoints(data,long = "decimallongitude",
#                                            lat = "decimallatitude", res = 0.25); rm(data)
#        names(r_data) <- names(dataG)[[i]]
#        r_dataG[[i]] <- r_data; rm(r_data)
#      } else{
#        #  #if there are less than 2 data-points the species name is saved in another list, to have
#        #  #the opportunity to check for errors in the data-set like spelling-errors etc.
#        dataLess2G[[i]] <- names(dataG)[[i]]; rm(data)
#      }
#      #I added a break-signal after reaching x species for testing
#      #if(i == 50){break}
#    }
#    #save the Data as rasterStack
#    r_dataG <- Filter(Negate(is.null), r_dataG)
#    r_dataG <- raster::stack(r_dataG)
#    return(r_dataG)
#  })

## ------------------------------------------------------------------------
#  #convert to one data.frame
#  amphiSmall_GBIF <- do.call("rbind", amphibians_smallrange_GBIF)
#  mamaSmall_GBIF <- do.call("rbind", ter_mammals_smallrange_GBIF)
#  avesSmall_GBIF <- do.call("rbind", ter_birds_smallrange_GBIF)
#  
#  amphiSmall_GBIF$taxa <- "amphibians"
#  amphiSmall_GBIF$total <- length(amphinames)
#  mamaSmall_GBIF$taxa <- "ter_mammals"
#  mamaSmall_GBIF$total <- length(mamanames)
#  avesSmall_GBIF$taxa <- "ter_birds"
#  avesSmall_GBIF$total <- length(avesnames)
#  
#  alltaxa_small_GBIF <- rbind(amphiSmall_GBIF, mamaSmall_GBIF, avesSmall_GBIF)

## ---- asis=TRUE----------------------------------------------------------
#  alltaxa_small_GBIF %>% select(taxa, species, total) %>% tidyr::drop_na() %>%
#    group_by(taxa) %>% summarise(n = n_distinct(species), total=mean(total)) %>%
#    mutate(perc = round(n/total*100, digits=2)) %>% knitr::kable()

## ---- asis=TRUE----------------------------------------------------------
#  alltaxa_small_GBIF %>% select(taxa, species, total) %>% tidyr::drop_na() %>%
#    group_by(species) %>% filter(n() > 10) %>% ungroup() %>%
#    group_by(taxa) %>% summarise(n = n_distinct(species), total=mean(total)) %>%
#    mutate(perc = round(n/total*100, digits=2)) %>% knitr::kable()

## ---- eval=F-------------------------------------------------------------
#  group <- c("amphibians", "ter_mammals", "ter_birds")[1]
#  
#  # Create plot
#  climateNiche::plotSp(r_dataG[[1:5]])

